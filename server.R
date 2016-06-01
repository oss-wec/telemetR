library(shiny)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
library(lubridate)
library(ggplot2)
library(sp)
library(adehabitatHR)
library(fasttime)
library(maptools)
library(shinyjs)
library(dplyr)
library(magrittr)
library(highcharter)
source("global.R")

dat <- fread("S:/MGritts/telemetR/Collars.csv")
dat_animal <- read.csv("S:/MGritts/telemetR/Animals.csv")
#dat <- fread("Collars.csv")
#dat_animal <- read.csv("Animals.csv")
dat$timestamp <- dat[, fastPOSIXct(timestamp)]

dat_animal <- dat_animal[dat_animal$deviceid < 1000000, ] # THIS REMOVES ALL VHF COLLARS, WORK AROUND
mgmtList <- dat_animal %>% dplyr::select(mgmtarea) %>% extract2(1) %>% unique() %>% sort()  # vector of mgmtlist for sl_mgmtarea

shinyServer(function(input, output, session) {
  
# PAGE 1 LOGIC
  ## enable/disable mgmt area based on species input
  observeEvent(input$sl_species, {
    if (input$sl_species == 'MULD') {
      shinyjs::enable('sl_mgmtarea')
    } else {
      shinyjs::disable('sl_mgmtarea')
    }
  })
  
  ## update select input for mgmt area
  updateSelectInput(session, 'sl_mgmtarea', choices = mgmtList, selected = '19')
  
  ## update selectize input for slz_ndowid
  ndowList <- reactive({
    if (input$sl_species == 'MULD') {
      l <- dat_animal %>% filter(spid == input$sl_species & mgmtarea == input$sl_mgmtarea) %>% 
        select(ndowid) %>% extract2(1) %>% unique() %>% sort()
    } else {
      l <- dat_animal %>% filter(spid == input$sl_species) %>% 
        select(ndowid) %>% extract2(1) %>% unique() %>% sort() 
    }
    return(l)
  })
  observeEvent(input$sl_species, {
    updateSelectizeInput(session, 'slz_ndowid', choices = c('', ndowList()), selected = '')
  })
  observeEvent(input$sl_mgmtarea, {
    updateSelectizeInput(session, 'slz_ndowid', choices = c('', ndowList()), selected = '')
  })
  
  ## table below the preview map on page 1
  output$animal.table <- DT::renderDataTable({
    df <- dat_animal[dat_animal$spid == input$sl_species,
                     c(2, 1, 4, 3, 7, 8, 5)]
    if (input$sl_species == "MULD") {
      df <- df[df$mgmtarea == input$sl_mgmtarea, ]
    }
    DT::datatable(df, rownames = FALSE,
                  colnames = c("Species", "NDOW ID", "Device ID", "Area",
                               "Inservice Date", "Outservice Date", "Fate"),
              class = "cell-border stripe")
  })

  ## preview map, shows every 20 locations for selected input
  output$preview <- renderLeaflet({
      CollarMap(df_subset())
    })

  ## list of NDOW IDs to subset dataframe
  # id_list <- reactive({
  #   return(as.numeric(strsplit(input$tx_ndowid, ', ')[[1]]))
  # })

  # DATAFRAME SUBSET BY SELECTED SPECIES, MGMT AREA, ID, DATE
  df_subset <- reactive({
    if (is.null(input$slz_ndowid) | '' %in% input$slz_ndowid) {
      df <- dat[species == input$sl_species, ]
      if (input$sl_species == "MULD") {
        df <- df[mgmtarea == input$sl_mgmtarea, ]
      }
    } else {
      df <- dat[species == input$sl_species &
                ndowid %in% as.numeric(input$slz_ndowid), ]
      if (input$ck_date == TRUE) {
        df <- df[timestamp >= as.POSIXct(input$sl_dates[1]) &
                 timestamp <= as.POSIXct(input$sl_dates[2]), ]
      }
    }
    # err_pts <- as.numeric(strsplit(input$tx_ErrPoints, ', ')[[1]])
    # df <- df[!(locid %in% err_pts), ]
    return(df)
  })

  # DATAFRAME OF ROWS WITH NA VALUES FOR LAT OR LONG
  df_na <- reactive({
    return(df_subset()[is.na(df_subset()$long_x | df_subset()$lat_y), ])
  })

  # OUTPUT INFO FOR ANIMALS SELECTED IN MAP
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<b>Total Animals:</b> ", length(unique(df_subset()$ndowid))),
            paste("<b>Total Points:</b> ", nrow(df_subset())),
            paste("<b>Error Rate:</b> ",
                  round(nrow(df_na()) / nrow(df_subset()), 4)),
            paste("<b>Min. Date:</b> ", min(df_subset()$timestamp)),
            paste("<b>Max. Date:</b> ", max(df_subset()$timestamp))
            ))
      })

  # CLEAR INPUT FOR PREVIEW MAP
  observeEvent(input$ac_reset, {
    shinyjs::reset("tx_ndowid")
    shinyjs::reset("sl_dates")
    shinyjs::reset("ck_date")
  })

# PAGE 2 LOGIC, SPATIAL ANALYSIS
  # CREATE DATAFRAME WITH MOVEMENT PARAMETERS
  move_df <- eventReactive(input$ac_UpdateMap, {
    df <- coord_conv(df_subset())
    df[, ':=' (dist = move.dist(x, y),
               R2n = move.r2n(x, y),
               mth = month(timestamp),
               hr = hour(timestamp),
               dt = move.dt(timestamp)), by = ndowid]
    df[, ':=' (sig.dist = cumsum(dist),
               speed = move.speed(dist, dt)), by = ndowid]
    #p <- movement_eda(df, plot_var = input$y.input, type = input$fig.type)
    #return(list(df, p))
    return(df)
  })

# PAGE 2 MAP, EVERY POINT
  ## CONTOUR LIST
  pct_contour <- reactive({
    return(as.numeric(strsplit(input$tx_Contour, ', ')[[1]]))
  })

  # TESTING TEXT OUTPUT
  output$tx_Conts <- renderText({
    dput(as.character(pct_contour()))
  })

  ## HOME RANGE ESTIMATION
  hr_ud <- eventReactive(input$ac_UpdateMap, {
    if (input$sl_HomeRange == 'Minimum Convex Polygon') {
      spdf <- SpatialPointsDataFrame(coordinates(cbind(move_df()$x, move_df()$y)),
                                     data = move_df(), proj4string = CRS('+proj=utm +zone=11'))
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
      kd <- SpatialPointsDataFrame(coordinates(cbind(move_df()$long_x, move_df()$lat_y)), data = move_df(),
                                   proj4string = CRS('+proj=longlat'))
      kd <- kernelUD(kd[, 2])
      hr <- lapply(kd, function(x) getContours(x, pct_contour()))
      
      ## spdf to geojson, wrapping this in an event reactive
      #hr <- lapply(hr, function(x) geojson_json(x))
    } else if (input$sl_HomeRange == 'Brownian Bridge') {
      bb <- to_ltraj(move_df())
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
    return(hr)
  })
  
  ## BASEMAP
  lfMap <- eventReactive(input$ac_UpdateMap, {
    hr <- hr_ud()
    if (input$sl_HomeRange == 'Brownian Bridge' | input$sl_HomeRange == 'Kernel Density') {
      hr <- lapply(hr, function(x) geojson_json(x))
    }
    
    lflt <- leaflet() %>% addProviderTiles('Esri.WorldTopoMap',
                                           options = providerTileOptions(attribution = NA))

    if (input$sl_HomeRange == 'Select Method') {
      lflt %>% mapPoints(move_df())
    } else {
      lflt %>% mapPolygons(hr) %>% mapPoints(move_df())
    }
  })

  # MAP OUTPUT
  output$map <- renderLeaflet({
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

# PAGE 3, MOVEMENT ANALYSIS
  move_plots <- eventReactive(input$ac_RunAnalysis, {
    p <- movement_eda(move_df(), plot_var = input$y.input, type = input$fig.type)
    return(p)
  })
  output$move.plot <- renderPlot({
    move_plots()
  })
  
  output$nsdTimeSeries <- highcharter::renderHighchart({
    highchart() %>% 
      hc_add_series_times_values(dates = as.Date(move_df()$timestamp), values = move_df()$R2n)
  })

  # PAGE 4
  # ALL DATA OUTPUT BUTTON
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
