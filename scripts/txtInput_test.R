library(shiny)
ui <- shinyUI(fluidPage(
  
  textInput("text", label = h3("Text input"), value = "Enter text..."),
  
  hr(),
  fluidRow(column(3, textOutput("value")))
  
))

server <- shinyServer(function(input, output) {
  
  output$value <- renderPrint({ 
    x <- as.character((as.numeric(strsplit(input$text, ", ")[[1]])))
    dput(x)
    })
  
})

shinyApp(ui, server)
