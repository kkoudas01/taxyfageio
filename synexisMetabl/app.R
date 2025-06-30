# Εγκατάσταση του πακέτου DT αν δεν υπάρχει ήδη
if (!require(DT)) {
  install.packages("DT")
  library(DT)}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
  library(shinydashboard)}
# Εγκατάσταση και φόρτωση των απαραίτητων βιβλιοθηκών
if (!require(shiny)) {
  install.packages("shiny")
  library(shiny)}

if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)}
if (!require(readr)) {
  install.packages("readr")
  library(readr)}
if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)}
if (!require(packHV)) {
  install.packages("packHV")
  library(packHV)
}


ui <- dashboardPage(
  dashboardHeader(title = "R🧸Κούδας"),skin="blue",
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Πίνακας δεδομένων", tabName = "data_table", icon = icon("table")),
      menuItem("Μέτρα θέσης", tabName = "measures_position", icon = icon("magnifying-glass-chart")),
      menuItem("Μέτρα διασποράς", tabName = "measures_dispersion", icon = icon("chart-area")),
      menuItem("Μέτρα ασυμμετρίας", tabName = "measures_skewness", icon = icon("balance-scale")),
      menuItem("Μέτρα συσχέτισης", tabName = "measures_correlation", icon = icon("exchange-alt")),
      menuItem("Επίδραση παράγοντα", tabName = "factor_effect", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Πίνακας δεδομένων
      tabItem(tabName = "data_table",
              h2("Πίνακας δεδομένων"),
              fluidRow(
                box(title = "Επιλέξτε δεδομένα", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("data_source", "Επιλέξτε δεδομένα:",selected= "Έλεγχος_Επίδοσης",
                                choices = c("Αρχείο", "mtcars", "iris", "Orange", "cars", "trees", "diamonds", "Έλεγχος_Επίδοσης")),
                    fileInput("file", "Εισάγετε αρχείο CSV ή Excel", accept = c(".csv", ".xlsx")),
                    uiOutput("column_selector")
                ),
                box(title = "Περιγραφή δεδομένων", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("data_description")
                )
              ),
              fluidRow(
                box(DTOutput("data_table"), width = 12)
              )
      ),
      
      # Μέτρα θέσης
      tabItem(tabName = "measures_position",
              h2("Μέτρα θέσης"),
              fluidRow(                box(title = "Επιλογές ιστογράμματος", status = "primary", solidHeader = TRUE, uiOutput("bar_selector"), width = 4),
                box(title = "Μέση τιμή - διάμεσος - τεταρτημόρια", status = "info", solidHeader = TRUE, width = 6,
                    textOutput("mean_output"),
                    textOutput("median_output"),
                    textOutput("quartile_output")
                ),
                box(plotOutput("histogram"), width = 6)
              )
      ),
      
      # Μέτρα διασποράς
      tabItem(tabName = "measures_dispersion",
              h2("Μέτρα διασποράς"),
              fluidRow(
                box(title = "Επιλογές ανισότητας Chebyshev", status = "primary", solidHeader = TRUE, width = 4,
                    numericInput("distance", "Απόσταση από τη μέση τιμή:", value = 1, step = 0.1)
                ),
                box(title = "Τυπική απόκλιση - Ανισότητα Chebyshev", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("std_output")
                )
              ),
              fluidRow(box(title = "Ενδοτεταρτημοριακό εύρος και ακραίες τιμές", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("iqr_output"),
                    textOutput("outliers_output")
                )
              ),
              fluidRow(box(title = "Επιλογές θηκογράμματος", status = "primary", solidHeader = TRUE, width = 4,
                           checkboxInput("show_outliers", "Εμφάνιση απομονωμένων σημείων στο θηκόγραμμα", TRUE)),
                box(plotOutput("boxplot"), width = 6)
              )
      ),
      
      # Μέτρα ασυμμετρίας
      tabItem(tabName = "measures_skewness",
              h2("Μέτρα ασυμμετρίας"),
              fluidRow(
                box(title = "Αποτελέσματα", status = "info", solidHeader = TRUE, width = 6,
                    textOutput("skewness_output")
                ),
                box(plotOutput("hist_boxplot"), width = 6)
              ),
              fluidRow(
                box(plotOutput("symmetry_plot"), width = 12)
              )
      ),
      
      # Μέτρα συσχέτισης
      tabItem(tabName = "measures_correlation",
              h2("Μέτρα συσχέτισης"),
              fluidRow(
                box(title = "Επιλογές", status = "primary", solidHeader = TRUE, uiOutput("column_selector2"), width = 4),
                box(
                  title = "Συντελεστές συσχέτισης", status = "info", solidHeader = TRUE, textOutput("cor_output")
                  , textOutput("spearman_output")
                  , textOutput("kendall_output"), width = 8)
              ),
              fluidRow( box(
                title = "Ευθειακή προσέγγιση", status = "info", solidHeader = TRUE, textOutput("regression_eq_output"), width = 4),
                box(plotOutput("scatter_plot"), width = 8)
              )
      ),
      
      # Επίδραση παράγοντα
      tabItem(tabName = "factor_effect",
              h2("Επίδραση παράγοντα"),
              fluidRow(
                box(title = "Επιλέξτε παράγοντα", status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("factor_selector"),
                    checkboxGroupInput("plot_options", "Επιλογές εμφάνισης:",
                                       choices = list("Ιστογράμματα" = "histograms", "Καμπύλες πυκνότητας" = "density"),
                                       selected = "density")
                ),
                box(
                  title = "Αποτελέσματα", status = "info", solidHeader = TRUE, textOutput("kruskal_output")
                  , textOutput("wilcoxon_r_output"), width = 8)
              ),
              fluidRow(box(plotOutput("histograms"), width = 8),
                box(plotOutput("boxplot_comparison"), width = 4)
              )
      )
    )
  )
)




server <- function(input, output, session) {
  Lessons <- c("Θρησκευτικά", "Αρχαία Γενικής", "Νεοελληνική Γλώσσα", "Νεοελληνική Λογοτεχνία",
               "Άλγεβρα", "Γεωμετρία", "Φυσική", "Χημεία", "Βιολογία", "Η/Υ", "Ιστορία",
               "Φιλοσοφία", "Πολιτική Παιδεία",  "Αρχαία Προσανατολισμού",
               "Κοινωνιολογία")
  A <- c(13,12,10,11,11,11,10,11,12,13,11,10,14,13,14)
  B <- c(13,13,12,13,13,13,12,12,12,16,12,12,14,13,14)
  exams <- c(4, 6.6, 7, 12, 6, 3, 1, 3, 3, 5.6, 5, 6, 1, 10, 10)
  choice <- c("Θεωρητικό", "Θεωρητικό","Θεωρητικό","Θεωρητικό","Θετικό","Θετικό","Θετικό","Θετικό","Θετικό","Θετικό",
              "Θεωρητικό","Θεωρητικό","Θεωρητικό","Θεωρητικό","Θεωρητικό")
  
  elegxos <- data.frame(Μαθήματα = Lessons, Α_Τετράμηνο = A, Β_Τετράμηνο = B, Εξετάσεις = exams, Κατεύθυνση = choice)
  
  
  # Προεπιλεγμένο dataset και στήλες μόλις ανοίξει η εφαρμογή
 
  
  output$elegxosTable <- renderTable({
    elegxos
  })
  
  data <- reactive({
    if (input$data_source == "Αρχείο") {
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        read.csv(input$file$datapath)
      } else if (ext == "xlsx") {
        readxl::read_excel(input$file$datapath)
      }
    } else {
      switch(input$data_source,
             "mtcars" = mtcars,
             "iris" = iris,
             "Orange" = Orange,
             "cars" = cars,
             "trees" = trees,
             "diamonds" = ggplot2::diamonds,
             "Έλεγχος_Επίδοσης" = elegxos)
    }
  })
  
  output$data_description <- renderText({
    switch(input$data_source,
           "mtcars" = paste("Το dataset 'mtcars' περιλαμβάνει δεδομένα για αυτοκίνητα με 11 μεταβλητές:",
                            "mpg (Κατανάλωση καυσίμου σε μίλια ανά γαλόνι),",
                            "cyl (Αριθμός κυλίνδρων),",
                            "disp (Χωρητικότητα κινητήρα σε κυβικά εκατοστά),",
                            "hp (Ιπποδύναμη),",
                            "drat (Σχέση μετάδοσης της πίσω άξονας),",
                            "wt (Βάρος αυτοκινήτου σε χιλιάδες λίβρες),",
                            "qsec (Χρόνος επιτάχυνσης 1/4 μιλίου σε δευτερόλεπτα),",
                            "vs (Μοτέρ V/S),",
                            "am (Μετάδοση, 0 = αυτόματη, 1 = χειροκίνητη),",
                            "gear (Αριθμός ταχυτήτων),",
                            "carb (Αριθμός καρμπυρατέρ)."),
           "iris" = paste("Το dataset 'iris' περιλαμβάνει δεδομένα για 150 λουλούδια από 3 διαφορετικά είδη:",
                          "Sepal.Length (Μήκος σέπαλου σε εκατοστά),",
                          "Sepal.Width (Πλάτος σέπαλου σε εκατοστά),",
                          "Petal.Length (Μήκος πέταλου σε εκατοστά),",
                          "Petal.Width (Πλάτος πέταλου σε εκατοστά),",
                          "Species (Είδος του λουλουδιού)."),
           "Orange" = paste("Το dataset 'Orange' περιλαμβάνει μετρήσεις ανάπτυξης πορτοκαλιών:",
                            "Tree (Αριθμός δέντρου),",
                            "Age (Ηλικία του δέντρου σε ημέρες),",
                            "circumference (Περίμετρος του δέντρου σε εκατοστά)."),
           "cars" = paste("Το dataset 'cars' περιλαμβάνει δεδομένα για την ταχύτητα και την απόσταση φρεναρίσματος:",
                          "speed (Ταχύτητα του αυτοκινήτου σε μίλια ανά ώρα),",
                          "dist (Απόσταση φρεναρίσματος σε πόδια)."),
           "trees" = paste("Το dataset 'trees' περιλαμβάνει δεδομένα για δέντρα:",
                           "Girth (Περίμετρος του δέντρου σε ίντσες),",
                           "Height (Ύψος του δέντρου σε πόδια),",
                           "Volume (Όγκος του δέντρου σε κυβικά πόδια)."),
           "diamonds" = paste("Το dataset 'diamonds' περιλαμβάνει δεδομένα για περισσότερα από 50.000 διαμάντια:",
                              "carat (Καράτια του διαμαντιού),",
                              "cut (Κόψιμο του διαμαντιού),",
                              "color (Χρώμα του διαμαντιού),",
                              "clarity (Διαύγεια του διαμαντιού),",
                              "depth (Βάθος του διαμαντιού σε ποσοστό),",
                              "table (Πλάκα του διαμαντιού σε ποσοστό),",
                              "price (Τιμή του διαμαντιού σε δολάρια),",
                              "x (Μήκος του διαμαντιού σε χιλιοστά),",
                              "y (Πλάτος του διαμαντιού σε χιλιοστά),",
                              "z (Ύψος του διαμαντιού σε χιλιοστά)."),
           "Αρχείο" = "Έχετε επιλέξει να φορτώσετε τα δικά σας δεδομένα από αρχείο.",
           "Έλεγχος_Επίδοσης" = "Ο κάτωθι πίνακας παρουσιάζει μέρος των βαθμών ενός, μάλλον όχι και τόσο καλού, μαθητή της Β’ Λυκείου, όπως αυτοί παρουσιάστηκαν  στη σελίδα Υλικό Φυσικής-Χημείας από τον κύριο Π. Μάλλιο. Ο εν λόγω καθηγητής κατέθεσε μια ερώτηση προς το ακροατήριο (https://forumning.wordpress.com/2016/10/21/%CF%80%CF%81%CE%AD%CF%80%CE%B5%CE%B9-%CE%B1%CF%85%CF%84%CF%8C%CF%82-%CE%BF-%CE%BC%CE%B1%CE%B8%CE%B7%CF%84%CE%AE%CF%82-%CE%BD%CE%B1-%CF%80%CF%81%CE%BF%CE%B1%CF%87%CE%B8%CE%B5%CE%AF-2/) «Θα έπρεπε ο μαθητής με αυτούς τους βαθμούς να προαχθεί;». Ερώτημα το οποίο συζητείται ακολούθως.")
  })
  
  
  
  
  output$column_selector <- renderUI({
    req(data())
    selectInput("column", "Επιλέξτε στήλη", choices = colnames(data()),
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Α_Τετράμηνο" else NULL)
  })
  
  output$column_selector2 <- renderUI({
    req(data())
    selectInput("column2", "Επιλέξτε δεύτερη στήλη", choices = colnames(data()),
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Β_Τετράμηνο" else NULL)
  })
  
  output$bar_selector <- renderUI({
    req(data())
    sliderInput("bins", "Αριθμός μπάρες ιστογράμματος:", min = 5, max = 50, value = 10)
  })
  
  
  
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  
  output$scatter_plot <- renderPlot({
    req(input$column, input$column2)
    col_data1 <- data()[[input$column]]
    col_data2 <- data()[[input$column2]]
    
    ggplot(data.frame(col_data1 = col_data1, col_data2 = col_data2), aes(x = col_data1, y = col_data2)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      labs(x = input$column, y = input$column2) +
      ggtitle(paste("Διάγραμμα διασποράς των τιμών", input$column, "και", input$column2)) +
      stat_smooth(method = "lm", se = FALSE, col = "blue", linetype = "dashed")
  })
  
  output$regression_eq_output <- renderText({
    req(input$column, input$column2)
    col_data1 <- data()[[input$column]]
    col_data2 <- data()[[input$column2]]
    fit <- lm(col_data2 ~ col_data1)
    equation <- paste("Ο τύπος της ευθείας γραμμικής παλινδρόμησης είναι: y =",
                      round(coef(fit)[2], 2), "* x +", round(coef(fit)[1], 2))
    equation
  })
  
  # Καρτέλα «Μέτρα θέσης»
  output$mean_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    mean_val <- mean(col_data, na.rm = TRUE)
    paste("Η μέση τιμή των τιμών", input$column, "είναι", round(mean_val, 2), ".")
  })
  
  output$median_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    median_val <- median(col_data, na.rm = TRUE)
    paste("Το 50% των τιμών", input$column, "είναι κάτω από", round(median_val, 2),"και το άλλο 50% πάνω από ",round(median_val, 2), ".")
  })
  
  output$quartile_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    quartiles <- quantile(col_data, probs = c(0.25, 0.75), na.rm = TRUE)
    paste("Το κεντρικό 50% των τιμών της", input$column, "είναι ανάμεσα στις τιμές", 
          round(quartiles[1], 2), "και", round(quartiles[2], 2), ".")
  })
  
  output$histogram <- renderPlot({
    req(input$column)
    col_data <- data()[[input$column]]
    bins <- input$bins
    ggplot(data.frame(col_data), aes(x = col_data)) +
      geom_histogram(bins = bins, fill = "blue", color = "black") +
      labs(x = input$column, y = "Συχνότητα") +
      ggtitle(paste("Ιστόγραμμα των τιμών της στήλης", input$column))
  })
  
  # Καρτέλα «Μέτρα διασποράς»
  output$std_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    std_val <- sd(col_data, na.rm = TRUE)
    mean_val <- mean(col_data, na.rm = TRUE)
    distance <- input$distance
    distance <- distance/std_val
    
    # Υπολογισμός πιθανότητας με βάση την ανισότητα Chebyshev
    chebyshev_prob <- 1 - 1 / (distance^2)
    if (distance <= 1) {
      chebyshev_prob <- 1  # Ειδική περίπτωση όταν η απόσταση είναι μικρότερη ή ίση με 1
    }
    
    # Δημιουργία του μηνύματος
    message <- paste("Η τυπική απόκλιση των τιμών", input$column, "είναι", round(std_val, 2), ".",
                     "Άρα είναι τουλάχιστον", round(chebyshev_prob * 100, 2), "% μία τιμή της", input$column,
                     "να βρίσκεται στο διάστημα", round(mean_val - distance * std_val, 2),
                     "και", round(mean_val + distance * std_val, 2), ".")
    message
  })
  
  
  output$iqr_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    iqr_val <- IQR(col_data, na.rm = TRUE)
    paste("Το κεντρικό 50% των τιμών", input$column, "εκτείνεται σε πλάτος", round(iqr_val, 2), ".")
  })
  
  output$outliers_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    Q1 <- quantile(col_data, 0.25, na.rm = TRUE)
    Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
    IQR_val <- IQR(col_data, na.rm = TRUE)
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    outliers <- col_data[col_data < lower_bound | col_data > upper_bound]
    if (length(outliers) > 0) {
      paste("Οι τιμές", paste(round(outliers, 2), collapse = ", "), "των", input$column, "είναι παράταιρες.")
    } else {
      paste("Δεν υπάρχουν απομονωμένες τιμές για τη στήλη", input$column, ".")
    }
  })
  
  output$boxplot <- renderPlot({
    req(input$column)
    col_data <- data()[[input$column]]
    ggplot(data.frame(col_data), aes(y = col_data)) +
      geom_boxplot(outlier.colour = ifelse(input$show_outliers, "red", NA)) +
      labs(y = input$column) +
      ggtitle(paste("Θηκόγραμμα των τιμών της στήλης", input$column))
  })
  
  # Καρτέλα «Μέτρα ασυμμετρίας»
  output$skewness_output <- renderText({
    req(input$column)
    col_data <- data()[[input$column]]
    
    if (!requireNamespace("moments", quietly = TRUE)) {
      install.packages("moments")
    }
    
    library(moments)
    
    skew_val <- moments::skewness(col_data, na.rm = TRUE)
    skewness_description <- if (abs(skew_val) <= 0.5) {
      "έχουμε συμμετρία"
    } else if (skew_val < -0.5) {
      "οι τιμές είναι μαζεμένες αριστερά"
    } else {
      "οι τιμές είναι μαζεμένες δεξιά"
    }
    paste("Ο συντελεστής ασυμμετρίας Pearson των τιμών", input$column, "είναι", round(skew_val, 2), 
          ", άρα", skewness_description, ".")
  })
  
  
  output$hist_boxplot <- renderPlot({
    req(input$column)
    col_data <- data()[[input$column]]
    if (!requireNamespace("packHV", quietly = TRUE)) install.packages("packHV")
    packHV::hist_boxplot(col_data)
  })
  
  output$symmetry_plot <- renderPlot({
    req(input$column)
    col_data <- data()[[input$column]]
    graf_symmetrias <- function(x) {
      n = length(x)
      n2 = n %/% 2
      sx = sort(x)
      mx = median(x)
      plot(mx - sx[1:n2], rev(sx)[1:n2] - mx,
           xlab = "Απόσταση Κάτω από τη Διάμεσο",
           ylab = "Απόσταση Πάνω από τη Διάμεσο")
      abline(a = 0, b = 1, lty = "dotted")
    }
    graf_symmetrias(col_data)
  })
  
  # Καρτέλα «Μέτρα συσχέτισης»
  output$cor_output <- renderText({
    req(input$column, input$column2)
    col_data1 <- data()[[input$column]]
    col_data2 <- data()[[input$column2]]
    cor_val <- cor(col_data1, col_data2, method = "pearson")
    correlation_strength <- if (abs(cor_val) >= 0.9) {
      "πολύ ισχυρή γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.7) {
      "πολύ ισχυρή γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.6) {
      "ισχυρή γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.4) {
      "ισχυρή γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.3) {
      "μέτρια γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.2) {
      "ασθενής γραμμική συσχέτιση"
    } else if (abs(cor_val) >= 0.1) {
      "αμελητέα γραμμική συσχέτιση"
    } else {
      "απουσία γραμμικής συσχέτισης"
    }
    paste("Ο συντελεστής γραμμικής συσχέτισης Pearson των τιμών", input$column, "και", input$column2, 
          "είναι", round(cor_val, 2), ", άρα έχουμε", correlation_strength, ".")
  })
  
  
  output$spearman_output <- renderText({
    req(input$column, input$column2)
    col_data1 <- data()[[input$column]]
    col_data2 <- data()[[input$column2]]
    
    
    spearman_val <- cor(col_data1, col_data2, method = "spearman")
    correlation_strength_sp <- if (abs(spearman_val) >= 0.9) {
      "τέλεια"
    } else if (abs(spearman_val) >= 0.7) {
      "πολύ ισχυρή"
    } else if (abs(spearman_val) >= 0.4) {
      "ισχυρή"
    } else if (abs(spearman_val) >= 0.3) {
      "μέτρια"
    } else if (abs(spearman_val) >= 0.2) {
      "ασθενής"
    } else if (abs(spearman_val) >= 0.1) {
      "αμελητέα"
    } else {
      "απουσία τάσης"
    }
    trend_sp <- if (spearman_val > 0) {
      "αυξητική τάση"
    } else if (spearman_val < 0) {
      "πτωτική τάση"
    } else {
      ""
    }
    paste("Ο συντελεστής μονοτονικής συσχέτισης Spearman των τιμών", input$column, "και", input$column2, 
          "είναι", round(spearman_val, 2), ", άρα έχουμε", correlation_strength_sp, trend_sp,".")
  })
  
  
  output$kendall_output <- renderText({
    req(input$column, input$column2)
    col_data1 <- data()[[input$column]]
    col_data2 <- data()[[input$column2]]
    kendall_val <- cor(col_data1, col_data2, method = "kendall")
    trend <- if (kendall_val > 0) {
      "αυξητική τάση"
    } else if (kendall_val < 0) {
      "πτωτική τάση"
    } else {
      "καμία τάση"
    }
    paste("Ο συντελεστής μονοτονικής συσχέτισης Kendall των τιμών", input$column, "και", input$column2,
          "είναι", round(kendall_val, 2), ", άρα έχουμε", trend, ".")
  })
  
  # Καρτέλα «Επίδραση παράγοντα»
  output$factor_selector <- renderUI({
    req(data())
    selectInput("factor_column", "Επιλέξτε παράγοντα", 
                choices = colnames(data()), 
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Κατεύθυνση" else NULL)
  })
  
  
  
  
  output$kruskal_output <- renderText({
    req(input$column, input$factor_column)
    col_data <- data()[[input$column]]
    factor_data <- data()[[input$factor_column]]
    
    # Έλεγχος Kruskal-Wallis
    kruskal_test <- kruskal.test(col_data ~ factor_data)
    p_value <- kruskal_test$p.value * 100  # Μετατροπή σε ποσοστό
    paste("Είναι", round(p_value, 2), "% πιθανό ο παράγοντας", input$factor_column, "να μην έχει επίδραση (έλεγχος Kruskal-Wallis).")
  })
  
  output$wilcoxon_r_output <- renderText({
    req(input$column, input$factor_column)
    col_data <- data()[[input$column]]
    factor_data <- data()[[input$factor_column]]
    
    if (!requireNamespace("rstatix", quietly = TRUE)) {
      install.packages("rstatix")
    }
    
    library(rstatix)
    
    df <- data.frame(Value.x = factor_data, Value.y = col_data)
    wilcox_r <- wilcox_effsize(Value.y ~ Value.x, data = df)
    r_value <- wilcox_r$effsize[1]
    effect_size <- if (abs(r_value) <= 0.2) {
      "μικρό"
    } else if (abs(r_value) <= 0.4) {
      "μεσαίο"
    } else {
      "μεγάλο"
    }
    paste("Το μέγεθος επίδρασης Wilcoxon είναι", round(r_value, 2), ", που υποδηλώνει", effect_size, 
          "μέγεθος επίδρασης.")
  })
  
  output$histograms <- renderPlot({
    req(input$column, input$factor_column)
    col_data <- data()[[input$column]]
    factor_data <- data()[[input$factor_column]]
    # Βάση για το ggplot
    p <- ggplot(data(), aes(x = col_data)) +
      ggtitle(paste("Ιστογράμματα των τιμών της στήλης", input$column, "κατά παράγοντα", input$factor_column))
    
    # Εμφάνιση των ιστογραμμάτων αν είναι επιλεγμένα
    if ("histograms" %in% input$plot_options) {
      p <- p + geom_histogram(aes(y = stat(density), fill = factor(factor_data)), position = "identity", alpha = 0.6)
    }
    
    # Εμφάνιση των καμπυλών πυκνότητας αν είναι επιλεγμένες
    if ("density" %in% input$plot_options) {
      p <- p + geom_density(aes(color = factor(factor_data)), size = 1)
    }
    
    # Επιστροφή γραφήματος
    p
  })

  
output$boxplot_comparison <- renderPlot({
  req(input$column, input$factor_column)
  col_data <- data()[[input$column]]
  factor_data <- data()[[input$factor_column]]
  
  ggplot(data(), aes(x = factor(factor_data), y = col_data, fill = factor(factor_data))) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(shape = 16, size = 1, position = position_jitter(0.2)) +
    labs(x = input$factor_column, y = input$column) +
    ggtitle(paste("Σύγκριση μεταβλητών της στήλης", input$column, "\nανά παράγοντα", input$factor_column))
})

  
  
}

shinyApp(ui = ui, server = server)
