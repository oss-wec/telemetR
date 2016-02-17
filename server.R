library(shiny)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
library(lubridate)
library(ggplot2)
library(sp)
library(adehabitatHR)
source("global.R")

dat <- fread("V:/ActiveProjects/Game/BGDB/Collars.csv", encoding = "UTF-8")
dat_animal <- read.csv("V:/ActiveProjects/Game/BGDB/Animals.csv")
#dat <- fread("Collars.csv", encoding = "UTF-8", nrow = 1000000)
#dat_animal <- read.csv("Animals.csv")
dat$date <- dat[, as.Date(timestamp)]
dat_animal <- dat_animal[dat_animal$deviceid < 1000000, ] # THIS REMOVES ALL VHF COLLARS, WORK AROUND

shinyServer(function(input, output) {

  # PAGE 1 LOGIC
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
  
  # PREVIEW MAP, EVERY 20 LOCATIONS
  output$preview <- renderLeaflet({ 
      CollarMap(df_subset())
    })

  # LIST OF NDOW IDS TO SUBSET DATAFRAME
  id_list <- reactive({
    id_list <- strsplit(input$tx_ndowid, ", ")
    id_list <- id_list[[1]]
    id_list <- as.numeric(id_list)
    return(id_list)
  })
  
  # DATAFRAME SUBSET BY SELECTED SPECIES, MGMT AREA, ID, DATE
  df_subset <- reactive({
    if (is.null(input$tx_ndowid) | input$tx_ndowid == "") {
      df <- dat[species == input$sl_species, ]
      if (input$sl_species == "MULD") {
        df <- df[mgmtarea == input$sl_mgmtarea, ]
      }
    } else {
      df <- dat[species == input$sl_species &
                ndowid %in% id_list(), ]
      if (input$ck_date == TRUE) {
        df <- df[date >= input$sl_dates[1] & date <= input$sl_dates[2], ]
      }
    }
    return(df)
  })
  
  # OUTPUT INFO FOR ANIMALS SELECTED IN MAP
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<b>Total Animals:</b> ", length(unique(df_subset()$ndowid))),
            paste("<b>Total Points:</b> ", nrow(df_subset())),
            paste("<b>Min. Date:</b> ", min(df_subset()$date)),
            paste("<b>Max. Date:</b> ", max(df_subset()$date))
            ))
      })
  
  # PAGE 1, CLEAR INPUT
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
    p <- movement_eda(df, plot_var = input$y.input, type = input$fig.type)
    #return(list(df, p))
    return(df)
  })
  
  # PAGE 2 MAP, EVERY POINT
  pg2map <- eventReactive(input$ac_UpdateMap, {
    if (input$ck_AllPoints == TRUE) {
      m <- DeviceMapping(df_subset())
    } else {
      m <- CollarMap(df_subset())
    }
    if (input$ck_BBMM == TRUE) {
      traj <- to_ltraj(move_df())
      bb <- estimate_bbmm(traj)
      ud <- get_ud(bb, 90)
      ud <- geojson_json(ud)
      m <- m %>% addGeoJSON(ud, stroke = F, color = "midnightblue", fillOpacity = .4,
                            group = "BBMM 95%")
    }
    return(m)
  })
  
  # MAP OUTPUT
  output$map <- renderLeaflet({
    pg2map()
  })
  
# PAGE 3, MOVEMENT ANALYSIS
  move_plots <- eventReactive(input$ac_RunAnalysis, {
    p <- movement_eda(move_df(), plot_var = input$y.input, type = input$fig.type)
    return(p)
  })
  output$move.plot <- renderPlot({
    move_plots()
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
      write.csv(df_subset(), file)
    }
  )
})
