library(shiny)
library(leaflet)
library(data.table)
library(DT)
source("global.R")

# ALL COLLARS. HOPEFULLY CONNECT TO THE DATABASE TO GET THIS DATA. IF I CAN 
# CONNECT TO THE DATABASE I'LL USE SPECIES AND MANAGEMENT AREA AS QUERY PARAMETERS 
dat <- fread("data/WorkingExample.csv")
dat_animal <- read.csv("data/collaredanimals2.csv")
#dat_animal$Collar_Date <- as.Date(strptime(dat_animal$Collar_Date, format = "%m/%d/%Y"))

shinyServer(function(input, output) {

  # SUBSETTING FRONT PAGE TABLE BY SPECIES AND MANAGEMENT AREA
  # IF MULD ALLOW TO SUBSET BY MGMT AREA, MAYBE HUNT UNIT TOO. RIGHT NOW MOST RECORDS ARE MULD
  output$animal.table <- DT::renderDataTable({
    df <- dat_animal[dat_animal$spid == input$species, ]
    datatable(df, rownames = FALSE,
              class = "cell-border stripe")
  })
  
  # SUBSET GPS DATA BY SPECIES  
  df_subset <- reactive({
    df[species == input$species, ]
  })
  
#   # TESTING DATE RANGE OUTPUT, THIS WAS A BITCH
#   output$date.out <- renderPrint({
#     t <- as.Date(input$fdate[1]) == dat_animal$Collar_Date[1]
#     print(dat_animal$Collar_Date[1])
#     print(t)
#   })
  
#   # ANIMALS WITH COLLARS TABLE OUTPUT. THIS WILL BE FILTERED BY SPECIES AND MGMT UNIT
#   # THE GOAL OF THIS FUNCTIONALITY IS TO GENERATE A TABLE THAT SHOW THE ANIMALS WITH 
#   # COLLARS AND THEIR IDs TO SHOW ON THE MAP PAGE.
#   output$animal.table <- DT::renderDataTable({
#     st_date <- as.Date(input$fdate[1])
#     end_date <- as.Date(input$fdate[2])
#     df <- dat_animal[dat_animal$Species == input$species &
#                      dat_animal$Collar_Date >= st_date &
#                      dat_animal$Collar_Date <= end_date, ]
#     datatable(df, rownames = FALSE, 
#               class = "cell-border stripe")
#   })
  
  # THE MAP INITITIALLY OPENS TO A SIMPLIFIED VERSION OF THE MAP; EACH ANIMALS FIRST
  # AND LAST LOCATION ARE SHOWN, WITH A LINE BETWEEN THEM. FOR THE LINE BETWEEN THEM,
  # I MAY TRY TO USE EVERY 10 OR SO POINTS AS LINE TO GET A GENERAL OVERVIEW OF THE PATH.
  output$map <- renderLeaflet({
    CollarMap(df_subset())
  })
  
  output$dates.out <- renderPrint({
    print(as.Date(input$dates[1]))
    print(class(input$dates[2]))
    })

})
