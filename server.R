library(shiny)
library(leaflet)
library(data.table)
library(DT)
source("global.R")

# ALL COLLARS. HOPEFULLY CONNECT TO THE DATABASE TO GET THIS DATA. IF I CAN 
# CONNECT TO THE DATABASE I'LL USE SPECIES AND MANAGEMENT AREA AS QUERY PARAMETERS 
dat <- fread("V:/ActiveProjects/WildlifeDiversity/Gritts_Mitch/AllCollars.csv")
dat$date <- as.Date(dat$timestamp, format = "%m/%d/%Y %I:%M:%S %p")
dat_animal <- read.csv("V:/ActiveProjects/WildlifeDiversity/Gritts_Mitch/collaredanimals2.csv")
dat_animal <- dat_animal[dat_animal$deviceid < 1000000, ] # THIS REMOVES ALL VHF COLLARS, WORK AROUND

shinyServer(function(input, output) {

  # SUBSETTING FRONT PAGE TABLE BY SPECIES AND MANAGEMENT AREA
  # IF MULD ALLOW TO SUBSET BY MGMT AREA, MAYBE HUNT UNIT TOO. RIGHT NOW MOST RECORDS ARE MULD
  output$animal.table <- DT::renderDataTable({
    df <- dat_animal[dat_animal$spid == input$species, ]
    if (input$species == "MULD") {
      df <- df[df$mgmtarea == input$mgmtarea, ]
    }
    datatable(df, rownames = FALSE,
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
  
  output$collar.table <- DT::renderDataTable({
    datatable(df_subset(), rownames = FALSE,
              class = "cell-border stripe")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {paste("CollarData", ".csv", sep = "")},
    content = function(file) {
      write.csv(df_subset(), file)
    }
  )
})
