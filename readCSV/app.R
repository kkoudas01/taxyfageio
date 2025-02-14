if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(DT)){
  install.packages("DT")
  library(DT)
}#> 
#> Attaching package: 'DT'
#> The following objects are masked from 'package:shiny':
#> 
#>     dataTableOutput, renderDataTable

# Define UI
ui <- shinyUI(fluidPage(
  
  fileInput('target_upload', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
  DT::dataTableOutput("sample_table")
)
)

if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(DT)){
  install.packages("DT")
  library(DT)
}

# Define server logic
server <- shinyServer(function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)
