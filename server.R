library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
library(lubridate)
library(ggplot2)
library(sp)
library(adehabitatHR)
library(maptools)
library(shinyjs)
library(magrittr)
library(highcharter)
source("global.R")

dat <- read_csv("Collars.csv", n_max = 10000)
dat_animal <- read_csv("Animals.csv")
#dat <- read_csv('/home/ubuntu/data/collars.csv')
#dat_animal <- read_csv('/home/ubuntu/data/animals.csv')

shinyServer(function(input, output, session) {

################  
# PAGE 1 LOGIC #
################    
  ## update selectize input for slz_ndowid
  ndowList <- reactive({
    l <- dat_animal %>% filter(spid == input$sl_species & mgmtarea == input$sl_mgmtarea) %>% 
      dplyr::select(ndowid) %>% extract2(1) %>% unique() %>% sort()
    return(l)
  })
  observeEvent(input$sl_species, {
    mgmtList <- dat_animal %>% filter(spid == input$sl_species) %>% dplyr::select(mgmtarea) %>% extract2(1) %>% unique() %>% sort()
    updateSelectizeInput(session, 'sl_mgmtarea', choices = mgmtList)
    updateSelectizeInput(session, 'slz_ndowid', choices = c('', ndowList()), selected = '')
  })
  observeEvent(input$sl_mgmtarea, {
    updateSelectizeInput(session, 'slz_ndowid', choices = c('', ndowList()), selected = '')
  })
  
  ## subset dat by input: species, mgmt area, id, date
  df_subset <- reactive({
    df <- dat %>% filter(species == input$sl_species & mgmtarea == input$sl_mgmtarea)

    # filter by ndow id, only if not null
    if (!(is.null(input$slz_ndowid) | '' %in% input$slz_ndowid)) {
      df <- df %>% filter(ndowid %in% as.numeric(input$slz_ndowid))
    }
    # filter by date, only if checked
    if (input$ck_date == TRUE) {
      df <- df %>% filter(date(timestamp) >= input$sl_dates[1] &
                   date(timestamp) <= input$sl_dates[2])
    }
    return(df)
  })
  
  ## preview map, shows 1 locations per day for selected input
  output$preview <- renderLeaflet({
    CollarMap(df_subset())
  })
  
  ## table below the preview map on page 1
  output$animal.table <- DT::renderDataTable({
    df <- dat_animal %>% 
      filter(spid == input$sl_species & mgmtarea == input$sl_mgmtarea) %>% 
      dplyr::select(c(2, 1, 3, 5, 4, 8, 9, 6))
    DT::datatable(df, rownames = FALSE,
                  colnames = c("Species", "NDOW ID", "Sex", "Device ID", "Area",
                               "Inservice Date", "Outservice Date", "Fate"),
              class = "cell-border stripe")
  })


  # n NAs (lat, long) for error rate
  df_na <- reactive({
    return(df_subset() %>% filter(is.na(long_x) | is.na(lat_y)) %>% nrow() %>% as.numeric())
  })

  # output info for animals selected in map
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<b>Total Animals:</b> ", length(unique(df_subset()$ndowid))),
            paste("<b>Total Points:</b> ", nrow(df_subset())),
            paste("<b>Error Rate:</b> ",
                  round(100 * (df_na() / nrow(df_subset())), 4), '%'),
            paste("<b>Min. Date:</b> ", min(df_subset()$timestamp)),
            paste("<b>Max. Date:</b> ", max(df_subset()$timestamp))
            ))
      })

  # CLEAR INPUT FOR PREVIEW MAP
  observeEvent(input$ac_reset, {
    shinyjs::reset("slz_ndowid")
    shinyjs::reset("sl_dates")
    shinyjs::reset("ck_date")
  })
  
  # CHANGE TAB TO SPATIAL AFTER CLICKING 'USE DATA'
  observeEvent(input$ac_UseData, {
    shinyjs::logjs('use data button pushed')
    updateNavbarPage(session, "nav", "Spatial")
  })

###########################  
# PAGE 2 SPATIAL ANALYSIS #
###########################
  ## create dataframe of movement parameters for analysis
  move_df <- eventReactive(input$ac_UpdateMap, {
    df <- xyConv(df_subset())
    move <- df %>%
      group_by(ndowid) %>%
      mutate(Distance = moveDist(x, y),
             sigDist = cumsum(Distance),
             NSD = moveNSD(x, y),
             dTime = moveDt(timestamp),
             Speed = moveSpeed(Distance, dTime),
             Year = year(timestamp),
             Month = month(timestamp),
             Day = day(timestamp),
             Hour = hour(timestamp)) %>%
      ungroup()
    return(move)
  })

  ## countour list for home range contours
  pct_contour <- reactive({
    return(as.numeric(strsplit(input$tx_Contour, ', ')[[1]]))
  })

  ## home range estimation
  hr_ud <- eventReactive(input$ac_UpdateMap, {
    df <- as.data.frame(move_df())
    if (input$sl_HomeRange == 'Minimum Convex Polygon') {
      spdf <- SpatialPointsDataFrame(coordinates(cbind(df$x, df$y)),
                                     data = df, proj4string = CRS('+proj=utm +zone=11'))
      cp <- mcp(spdf[, 2], percent = 99)
      cp <- spTransform(cp, CRS('+proj=longlat'))
      ids <- cp$id
      hr <- vector("list", length(cp$id))
      for (i in seq_along(ids)) {
        poly <- cp[cp$id == ids[i], ]
        poly <- geojson_json(poly)
        hr[[i]] <- poly
      }
      names(hr) <- ids
    } else if (input$sl_HomeRange == 'Kernel Density') {
      kd <- SpatialPointsDataFrame(coordinates(cbind(df$long_x, df$lat_y)), data = df,
                                   proj4string = CRS('+proj=longlat'))
      kd <- kernelUD(kd[, 2])
      hr <- lapply(kd, function(x) getContours(x, pct_contour()))

      ## spdf to geojson, wrapping this in an event reactive
      #hr <- lapply(hr, function(x) geojson_json(x))
    } else if (input$sl_HomeRange == 'Brownian Bridge') {
      bb <- to_ltraj(df)
      bb <- estimate_bbmm(bb)
      bb <- bbBugFix(bb)
      hr <- lapply(bb, function(x) getContours(x, pct_contour()))
      for (i in seq_along(bb)) {
        hr[[i]]@proj4string <- CRS('+init=epsg:26911')
        hr[[i]] <- spTransform(hr[[i]], CRS('+init=epsg:4326'))
      }

      ## spdf to geojson, wrapping this in an event reactive
      #hr <- lapply(hr, function(x) geojson_json(x))
    }
    shinyjs::logjs('home range calculated')
    shinyjs::logjs(dput(hr))
    return(hr)
  })

  ## BASEMAP
  lfMap <- eventReactive(input$ac_UpdateMap, {
    shinyjs::logjs(input$sl_HomeRange)
    hr <- hr_ud()
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      hr <- lapply(hr, function(x) geojson_json(x))
    }

    lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                           options = providerTileOptions(attribution = NA))
    
    shinyjs::logjs('home range added to map')
    
    if (input$sl_HomeRange == 'Display Points') {
      lflt <- lflt %>% mapPoints(move_df())
    } else {
      lflt <- lflt %>% mapPolygons(hr) %>% mapPoints(move_df())
    }
  })

  # MAP OUTPUT
  output$map <- renderLeaflet({
    shinyjs::logjs('leaflet (re)-rendered')
    lfMap()
  })

  # SHAPEFILE OUTPUT
  output$dl_Shape <- downloadHandler(
    filename = function() 'UtlzDist_Export.zip',
    content = function(file) {
      if (length(Sys.glob('UtlzDist_shp.*')) > 0){
        file.remove(Sys.glob('UtlzDist_shp.*'))
      }
      shpOut <- correctIDs(hr_ud())
      shpOut <- Reduce(rbind, shpOut)
      writePolyShape(shpOut, 'UtlzDist_shp')
      zip(zipfile = 'UtlzDist_Export.zip', files = Sys.glob('UtlzDist_shp.*'))
      file.copy('UtlzDist_Export.zip', file)
      if(length(Sys.glob('UtlzDist_shp.*')) > 0){
        file.remove(Sys.glob('UtlzDist_shp.*'))
      }
    },
    contentType = 'application/zip'
  )

  # HIDE POLYGON OUTPUT IF MCP IS USED
  observeEvent(input$sl_HomeRange, {
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      shinyjs::show('dl_Shape')
    } else {
      shinyjs::hide('dl_Shape')
    }
  })

  # SPATIAL DATA OUTPUT
  output$downloadData <- downloadHandler(
    filename = function() {paste("CollarData", ".", sep = "")},
    content = function(file) {
      write.csv(move_df(), file)
    }
  )

#############################
# PAGE 3, MOVEMENT ANALYSIS #
#############################
  move_plots <- eventReactive(input$ac_RunAnalysis, {
    p <- movement_eda(move_df(), plot_var = input$y.input, type = input$fig.type)
    return(p)
  })
  output$move.plot <- renderPlot({
    move_plots()
  })

  observeEvent(input$slz_ndowid, {
    ids <- input$slz_ndowid
    updateSelectizeInput(session, 'slz_nsdID', choices = sort(ids), selected = ids[1])
  })

  output$nsdTimeSeries <- highcharter::renderHighchart({
    df <- move_df() %>%
      mutate(ts = as_date(timestamp)) %>% 
      arrange(ndowid, ts) %>%
      group_by(ndowid, ts) %>%
      slice(1) %>% 
      ungroup()

    ids <- input$slz_nsdID
    hc <- highchart()
    for(i in seq_along(ids)) {
      d <- df %>% filter(ndowid == ids[i])
      hc <- hc_add_series_times_values(hc, dates = d$ts, values = d$NSD,
                                       color = color_pal[i], name = ids[i]) %>% 
            hc_yAxis(max = 1)
    }
    hc
  })

################
# PAGE 4, DATA #
################  
  # DT display
  output$collar.table <- DT::renderDataTable({
    DT::datatable(move_df(), rownames = FALSE,
                  class = "cell-border stripe")
  })

  # DOWNLOAD DATA BUTTON
  output$downloadData <- downloadHandler(
    filename = function() {paste("CollarData", ".csv", sep = "")},
    content = function(file) {
      write.csv(move_df(), file)
    }
  )
})
