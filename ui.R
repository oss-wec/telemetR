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
            DT::dataTableOutput("animal.table", width = "100%", height = "auto"))),

   tabPanel("Map",
    div(class = "outer",
        tags$head(includeCSS("style.css")),

        leafletOutput("map", width = "100%", height = "100%"),

        absolutePanel(shinyjs::useShinyjs(),
                      id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, width = 330, height = "auto",
                      top = 110, bottom = "auto", left = "auto", right = 10,

                      h2("Animal Selection"),

                      textInput("ndowid", "NDOW ID:", NULL),
                      dateRangeInput("dates", "Date Range:",
                                     start = "2010-01-01",
                                     min = "2010-01-01"),
                      checkboxInput("use.date", "Use Date Range", value = FALSE),
                      actionButton("reset", "Reset Input")
                      )
    )
  ),

  tabPanel("Migration",
           h2("Migration Analysis"),
           p("Below are several exploratory data analysis and visualizations to help understand the collar data. 
             The first figure is the cumulative distance traveled (sig.dis). The second figure is the net 
             squared displacement (R2n). R2n is a measure of the distance between the first location and the current location squared ((p1, pi)^2). The method is described in Bunnefeld et al. 2011 as a method to quantify migration. 
             The third figure is a plot of the distance travelled between each successive point. The fourth figure is a histogram of the distances moved."),
           p("At the bottom is the map that includes every GPS collar location for the selected animal. 
             There is an option to estimate a generalized Brownian bridge movement model. This is an estimation of the animals utilization of the landscape. The polygons represent the 95% utilization distribution.
             Currently the Brownian bridge can only be run for on animal at a time."),
           actionButton("plotMigration", "Run Analysis"),
           checkboxInput("run.bbmm", "Calculate BBMM", value = FALSE),
           hr(),
           plotOutput("migrationAnalysis"),
           hr(),
           leafletOutput('allpoints', height = "500"),
           br(),
           p("A map of every GPS fix for the selected animals. To turn animals on and off use the radio buttons on the right.
             This serves as a visual inspection of the data for erraneous locations. Further analysis is available.")
           ),
  
  tabPanel("Data",
           h2("All GPS Data"),
           p("All GPS collar data for seleceted animals. To download the data in the table below, click the download button."),
           downloadButton("downloadData", "Download Data"),
           hr(),
           DT::dataTableOutput("collar.table"))
))
