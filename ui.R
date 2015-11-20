library(shiny)
library(leaflet)

shinyUI(navbarPage("NDOW GPS Collar", id = "nav",
   tabPanel("Collared Animals",
            fluidRow(h2("Filter Data"),
                     column(4,
                            selectInput("species", "Species", 
                                        unique(c("CBHS", "DBHS", "MTGT", "MULD", "RBHS", "RMEL"))
                                        )
                            ),
                     column(4,
                            selectInput("mgmtarea", "Management Area",
                                        choices = 1:29,
                                        selected = 10
                            )
                     ),
            hr(),
            DT::dataTableOutput("animal.table", width = "100%", height = "auto"))),   
                   
   tabPanel("Map",
    div(class = "outer",
        
        tags$head(includeCSS("style.css")),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, width = 330, height = "auto",
                      top = 110, bottom = "auto", left = "auto", right = 10,
                      
                      h2("Animal Selection"),
                      
                      textInput("ndowid", "NDOW ID:", NULL),
                      dateRangeInput("dates", "Date Range:",
                                     start = "2010-01-01",
                                     min = "2010-01-01"),
                      checkboxInput("use.date", "Use Date Range", value = FALSE)
                      )
    )
  ),
  
  tabPanel("Data",
           h2("All GPS Data"),
           p("All GPS collar data for seleceted animals. To download the data in the table below, click the download button."),
           downloadButton("downloadData", "Download Data"),
           hr(),
           dataTableOutput("collar.table"))
  
))