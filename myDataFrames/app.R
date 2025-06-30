library(shiny)
library(readr)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  titlePanel("Διαχείριση και Επεξεργασία Πινάκων"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Επιλογή Αρχείων", multiple = TRUE, accept = c(".csv", ".xlsx")),
      textInput("na_string", "Μορφή NA", value = "NA"),
      textInput("delimiter", "Διαχωριστικό (για CSV)", value = ",")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Προβολή Πινάκων", 
                 DTOutput("table_display")),
        
        tabPanel("Επεξεργασία Πίνακα",
                 uiOutput("column_selector"),
                 textInput("filter_cond", "Συνθήκες Φίλτρου", value = ""),
                 actionButton("apply_filter", "Εφαρμογή Φίλτρου"),
                 DTOutput("filtered_table")),
        
        tabPanel("Δημιουργία Νέου Πίνακα",
                 selectInput("table1", "Πίνακας 1", choices = NULL),
                 selectInput("table2", "Πίνακας 2", choices = NULL),
                 actionButton("join_side_by_side", "Ένωση Δίπλα-Δίπλα"),
                 actionButton("join_top_bottom", "Ένωση Κάτω-Κάτω"),
                 selectInput("col_to_transform", "Στήλη για επεξεργασία", choices = NULL),
                 textInput("transformation", "Μετασχηματισμός", value = "x*2"),
                 actionButton("add_new_column", "Προσθήκη Νέας Στήλης"),
                 DTOutput("new_table")),
        
        tabPanel("Αναδιαμόρφωση Πίνακα",
                 selectInput("table_to_reshape", "Επιλέξτε Πίνακα", choices = NULL),
                 radioButtons("reshape_option", "Επιλογή Αναδιαμόρφωσης", 
                              choices = c("Πλατιάς -> Μακράς" = "wide_to_long", 
                                          "Μακράς -> Πλατιάς" = "long_to_wide")),
                 actionButton("apply_reshape", "Εφαρμογή"),
                 DTOutput("reshaped_table")),
        
        downloadButton("download_table", "Αποθήκευση Πίνακα")
      )
    )
  )
)

server <- function(input, output, session) {
  # Κώδικας για την ανάγνωση αρχείων και την εμφάνιση των πινάκων
}

shinyApp(ui, server)
