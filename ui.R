library(shiny)
library(leaflet)

shinyUI(navbarPage("NDOW GPS Collar", id = "nav",
   tabPanel("Collared Animals",
            fluidRow(h2("Filter Table"),
                     column(4,
                            selectInput("species", "Species", 
                                        unique(c("CBHS", "DBHS", "MTGT", "MULD", "RBHS", "RMEL"))
                                        )
                            ),
                     column(4,
                            selectInput("mgmtarea", "Management Area",
                                           1:29)
                            )
                     ),
            hr(),
            DT::dataTableOutput("animal.table", width = "100%", height = "auto")),   
                   
   tabPanel("Map",
    div(class = "outer",
        
        tags$head(includeCSS("style.css")),
        
        leafletOutput("map", width = "100%", height = "100%"),
        
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, width = 330, height = "auto",
                      top = 110, bottom = "auto", left = "auto", right = 10,
                      
                      h2("Animal Selection"),
                      
                      textInput("ndowid", "NDOW ID:", NULL),
                      checkboxInput("use.date", "Use Date Range", value = FALSE),
                      dateRangeInput("dates", "Date Range:",
                                     start = "2010-01-01",
                                     min = "2010-01-01"),
                      
                      textOutput("id.out"),
                      verbatimTextOutput(("dates.out"))
                      )
    )
  ),
  
  tabPanel("Data",
           hr(),
           dataTableOutput("collar.table"))
  
))