library(shiny)
library(leaflet)
library(data.table)
library(DT)
source("global.R")

# ALL COLLARS. HOPEFULLY CONNECT TO THE DATABASE TO GET THIS DATA. IF I CAN 
# CONNECT TO THE DATABASE I'LL USE SPECIES AND MANAGEMENT AREA AS QUERY PARAMETERS 
dat <- fread("V:/ActiveProjects/WildlifeDiversity/Gritts_Mitch/WorkingExample2.csv")
dat_animal <- read.csv("V:/ActiveProjects/WildlifeDiversity/Gritts_Mitch/collaredanimals2.csv")
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
  
  id_list <- reactive({
    id_list <- strsplit(input$ndowid, ", ")
    id_list <- id_list[[1]]
    id_list <- as.numeric(id_list)
    return(id_list)
  })
  
  df_subset <- reactive({
    if (is.null(input$ndowid) | input$ndowid == "") {
      dat[species == input$species, ]
    } else {
      dat[species == input$species &
           ndowid %in% id_list(), ]
    }
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
  
  output$map <- renderLeaflet({
    CollarMap(df_subset())
  })
  
  output$id.out <- renderPrint({
    print(id_list())
  })
  
  output$dates.out <- renderPrint({
    print(as.Date(input$dates[1]))
    print(class(input$dates[2]))
    })
  
  output$collar.table <- DT::renderDataTable({
    datatable(df_subset(), rownames = FALSE,
              class = "cell-border stripe")
  })

})
