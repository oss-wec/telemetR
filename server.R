library(shiny)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
source("global.R")

#dat <- fread("V:/ActiveProjects/Game/BGDB/AllCollars.csv", encoding = "UTF-8")
dat <- fread("Collars.csv", encoding = "UTF-8", nrows = 50000)
dat_animal <- read.csv("Animals.csv")
dat$date <- dat[, as.Date(timestamp)]
#dat_animal <- read.csv("V:/ActiveProjects/Game/BGDB/Animals.csv")
dat_animal <- dat_animal[dat_animal$deviceid < 1000000, ] # THIS REMOVES ALL VHF COLLARS, WORK AROUND

shinyServer(function(input, output) {
  
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
  
  output$preview <- renderLeaflet({ 
      CollarMap(df_subset())
    })

  # SUBSET GPS DATA BY SPECIES
  id_list <- reactive({
    id_list <- strsplit(input$tx_ndowid, ", ")
    id_list <- id_list[[1]]
    id_list <- as.numeric(id_list)
    return(id_list)
  })

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
  
  output$dataInfo <- renderUI({
    HTML(
      paste(sep = "<br/>",
            paste("<b>Total Animals:</b> ", length(unique(df_subset()$ndowid))),
            paste("<b>Total Points:</b> ", nrow(df_subset())),
            paste("<b>Min. Date:</b> ", min(df_subset()$date)),
            paste("<b>Max. Date:</b> ", max(df_subset()$date))
            ))
      })
  
  output$map <- renderLeaflet({
    CollarMap(df_subset())
  })

  observeEvent(input$ac_reset, {
    shinyjs::reset("tx_ndowid")
    shinyjs::reset("sl_dates")
    shinyjs::reset("ck_date")
  })

  output$collar.table <- DT::renderDataTable({
    DT::datatable(df_subset(), rownames = FALSE,
              class = "cell-border stripe")
  })

  output$downloadData <- downloadHandler(
    filename = function() {paste("CollarData", ".csv", sep = "")},
    content = function(file) {
      write.csv(df_subset(), file)
    }
  )

  migration_df <- eventReactive(input$plotMigration,  {
    df <- df_subset()
    return(df)
  })

  output$migrationAnalysis <- renderPlot({
    traj <- to_ltraj(migration_df())[[1]]
    traj$cumdist <- cumsum(traj$dist)
    grid.arrange(movement_eda(traj, "sig.dist", "line", "royalblue4"),
                 movement_eda(traj, "R2n", "line", "royalblue4"),
                 movement_eda(traj, "dist", "point", "royalblue4"),
                 movement_eda(traj, "dist", "hist", "royalblue4"), ncol = 1)
  })
  
  move_df <- eventReactive(input$run, {
    df <- coord_conv(df_subset())
    df[, ':=' (dist = move.dist(x, y),
               R2n = move.r2n(x, y),
               sig.dist = cumsum(move.dist(x, y))), by = ndowid]
    p <- movement_eda(df, plot_var = input$y.input, type = input$fig.type)
    return(list(df, p))
  })
  
  output$move.plot <- renderPlot({
    #p <- movement_eda(move_df(), plot_var = input$y.input, type = input$fig.type)
    move_df()[[2]]
  })
  
  output$move.table <- renderTable({
    head(move_df()[[1]])
  })
  
  output$allpoints <- renderLeaflet({
    m <- DeviceMapping(migration_df())
    if (input$run.bbmm == TRUE) {
      traj <- to_ltraj(migration_df())
      bb <- estimate_bbmm(traj)
      ud <- get_ud(bb, 95)
      ud <- geojson_json(ud)
      m <- m %>% addGeoJSON(ud,
                           stroke = F, color = "midnightblue", fillOpacity = .4,
                           group = "BBMM 95%")
    }
    m
  })
})
