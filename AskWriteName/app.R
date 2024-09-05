if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

ui <- fluidPage(
  textInput("caption", "Πώς σε λένε;", "Το όνομά σου"),
  verbatimTextOutput("value")
)
server <- function(input, output) {
  output$value <- renderText({ input$caption })
}

shinyApp(ui, server)