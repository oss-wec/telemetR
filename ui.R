library(shiny)
library(shinyjs)
library(leaflet)

shinyUI(navbarPage("NDOW GPS Collar", id = "nav",
# PAGE 1, SUBSET DATA   
    tabPanel("Collared Animals", useShinyjs(), div(class = "pg1",
            fluidRow(h2("Filter Data"),
                     column(3,
                            selectInput("sl_species", "Species",
                                        unique(c("CBHS", "DBHS", "MTGT", "MULD", "RBHS", "RMEL"))
                                        ),
                            selectInput("sl_mgmtarea", "Management Area",
                                        choices = 1:29, selected = 19),
                            hr(),
                            p("The following input will further filter the individuals,
                              dipslayed on the map. These can be reset with the Reset Input button."),
                            textInput("tx_ndowid", "NDOW ID", NULL),
                            checkboxInput("ck_date", "Use Date Range", value = FALSE),
                            dateRangeInput("sl_dates", "Date Range:",
                                           start = "2010-01-01", min = "2010-01-01", 
                                           startview = "year"),
                            actionButton("ac_reset", "Reset Input"),
                            hr(),
                            htmlOutput("dataInfo")
                            ),
                     column(9,
                            leafletOutput("preview", height = 600)
                            )
            ),
            hr(),
            fluidRow(h2("Collared Animals"),
                     DT::dataTableOutput("animal.table", width = "100%", height = "auto")
                    )
            )),

# PAGE 2, SPATIAL ANALYSIS   
   tabPanel("Spatial",
    div(class = "outer", tags$head(includeCSS("style.css")),
        tags$head(includeCSS("style.css")),
        leafletOutput("map", width = "100%", height = "100%"),
        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE,
                      width = 330, height = 600, top = "110", bottom = "auto", 
                      left = 10, right = "auto",
                      h2("Spatial Analysis"),
                      p("Select inputs for spatial analysis."),
                      checkboxInput("ck_AllPoints", "Use All Points?", value = FALSE),
                      checkboxInput("ck_BBMM", "Estimate BBMM?", value = FALSE),
                      actionButton("ac_UpdateMap", "Update Map")
                      )
    )
  ),

# PAGE 3, MOVEMENT ANALYSIS  
  tabPanel("Movement", 
           sidebarLayout(
             sidebarPanel(h2("Movement Analysis"),
                          h3("Input Panel"),
                          selectInput("fig.type", "Figure Type",
                                      choices = c("point", "line", "histogram"),
                                      selected = "point"),
                          selectInput("y.input", "Y Axis", 
                                      choices = c("dist", "R2n", "sig.dist", "speed", "dt")),
                          actionButton("ac_RunAnalysis", "Run Anlysis")),
             mainPanel(
               plotOutput("move.plot", width = "100%"),
               tableOutput("move.table")
               )
             )),

# PAGE 4, DATA EXPORT  
  tabPanel("Data",
           h2("All GPS Data"),
           p("All GPS collar data for seleceted animals. To download the data in the table below, click the download button."),
           downloadButton("downloadData", "Download Data"),
           hr(),
           DT::dataTableOutput("collar.table"))
))
