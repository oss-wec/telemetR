library(shiny)
library(leaflet)
library(data.table)
library(gridExtra)
library(geojsonio)
source("global.R")

#dat <- fread("V:/ActiveProjects/Game/BGDB/AllCollars.csv", encoding = "UTF-8")
dat <- fread("Collars.csv", encoding = "UTF-8")
dat_animal <- read.csv("Animals.csv")
dat$date <- dat[, as.Date(timestamp)]
#dat_animal <- read.csv("V:/ActiveProjects/Game/BGDB/Animals.csv")
dat_animal <- dat_animal[dat_animal$deviceid < 1000000, ] # THIS REMOVES ALL VHF COLLARS, WORK AROUND

shinyServer(function(input, output) {
  # SUBSETTING FRONT PAGE TABLE BY SPECIES AND MANAGEMENT AREA
  # IF MULD ALLOW TO SUBSET BY MGMT AREA, MAYBE HUNT UNIT TOO. RIGHT NOW MOST RECORDS ARE MULD
  output$animal.table <- DT::renderDataTable({
    df <- dat_animal[dat_animal$spid == input$species, ]
    if (input$species == "MULD") {
      df <- df[df$mgmtarea == input$mgmtarea, ]
    }
    DT::datatable(df, rownames = FALSE,
              class = "cell-border stripe")
  })

  # SUBSET GPS DATA BY SPECIES
  id_list <- reactive({
    id_list <- strsplit(input$ndowid, ", ")
    id_list <- id_list[[1]]
    id_list <- as.numeric(id_list)
    return(id_list)
  })

  df_subset <- reactive({
    if (is.null(input$ndowid) | input$ndowid == "") {
      df <- dat[species == input$species, ]
      if (input$species == "MULD") {
        df <- df[mgmtarea == as.numeric(input$mgmtarea), ]
      }
    } else {
      df <- dat[species == input$species &
                ndowid %in% id_list(), ]
      if (input$use.date == TRUE) {
        df <- df[date >= input$dates[1] & date <= input$dates[2], ]
      }
    }
    return(df)
  })

  output$map <- renderLeaflet({
    CollarMap(df_subset())
  })

  observeEvent(input$reset, {
    shinyjs::reset("controls")
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
