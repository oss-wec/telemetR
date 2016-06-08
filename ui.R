library(shiny)
library(shinyjs)
library(leaflet)

shinyUI(tagList(
  useShinyjs(),
  navbarPage("telemetR", id = "nav",
# PAGE 1, SUBSET DATA   
    tabPanel('Preview', div(class = "pg1",
            fluidRow(column(3, h2('Filter Data'),
                            selectInput("sl_species", "Species",
                                        unique(c("CBHS", "DBHS", "MTGT", "MULD", "RBHS", "RMEL"))
                                        ),
                            selectInput("sl_mgmtarea", "Management Area",
                                        choices = 1:29, selected = 19),
                            hr(),
                            p("The following input will further filter the individuals,
                              dipslayed on the map. To reset the following input click the Reset Input button."),
                            selectizeInput('slz_ndowid', 'NDOW ID', choices = NULL, multiple = TRUE),
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
                      radioButtons('rd_nPoints', 'Path Smoothing', choices = c('Smooth', 'All Points'),
                                   selected = 'All Points', inline = TRUE),
                      selectizeInput("sl_HomeRange", "Homerange Estimation Method",
                                     choices = c('Display Points', "Minimum Convex Polygon", "Kernel Density", "Brownian Bridge"),
                                     selected = 'Display Points'),
                      textInput('tx_Contour', 'Contour Percentages', placeholder = '95', value = '95'),
                      actionButton("ac_UpdateMap", "Update Map"),
                      downloadButton('dl_Shape', 'Download Polygon'),
                      hr(),
                      textOutput('tx_Conts')
                      )
    )
  ),

# PAGE 3, MOVEMENT ANALYSIS
  tabPanel("Movement",
           fluidRow(column(3,
                           selectInput('fig.type', 'Figure Type',
                                       choices = c('point', 'line', 'histogram'),
                                       selected = 'line'),
                           selectInput("y.input", "Y Axis",
                                       choices = c("Distance", "NSD", "sigDist", "Speed", "dTime"),
                                       selected = 'NSD')
                           ),
                    column(3, br(), br(), br(), br(), br(),
                           actionButton('ac_RunAnalysis', 'Create Graphs', color = 'blue'))
                    ),
           fluidRow(
             plotOutput('move.plot', width = '100%', height = 600)
           ),
           hr(),
           fluidRow(
             column(3,
                    selectizeInput('slz_nsdID', 'Select ID', choices = NULL, multiple = TRUE)),
             column(4,
                    p('Choose animals to display in the figure below. Multiple selections are supported.
                      Net Squared Displacement is a metric used to classify movement strategies. The
                      are transformed to be between 0 and 1.'))
           ),
           fluidRow(
             highcharter::highchartOutput('nsdTimeSeries')
           )
          ),

# PAGE 4, DATA EXPORT
  tabPanel("Data",
           h2("All GPS Data"),
           p("All GPS collar data for seleceted animals. To download the data in the table below, click the download button."),
           downloadButton("downloadData", "Download Data"),
           hr(),
           DT::dataTableOutput("collar.table"))
))
)