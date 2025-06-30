# --- 1. Φόρτωση Απαραίτητων Πακέτων ---
# Λίστα με τα πακέτα που απαιτούνται για την εφαρμογή.
packages <- c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", 
              "readr", "readxl", "packHV", "moments", "rstatix")

# Αυτός ο βρόχος ελέγχει αν κάθε πακέτο είναι εγκατεστημένο.
# Αν όχι, το εγκαθιστά και στη συνέχεια το φορτώνει.
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# --- 2. Διεπαφή Χρήστη (User Interface - UI) ---
ui <- dashboardPage(
  dashboardHeader(title = "R🧸Κούδας", titleWidth = 250), skin="blue",
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Πίνακας Δεδομένων", tabName = "data_table", icon = icon("table")),
      menuItem("Μέτρα Θέσης", tabName = "measures_position", icon = icon("magnifying-glass-chart")),
      menuItem("Μέτρα Διασποράς", tabName = "measures_dispersion", icon = icon("chart-area")),
      menuItem("Μέτρα Ασυμμετρίας", tabName = "measures_skewness", icon = icon("balance-scale")),
      menuItem("Μέτρα Συσχέτισης", tabName = "measures_correlation", icon = icon("exchange-alt")),
      menuItem("Επίδραση Παράγοντα", tabName = "factor_effect", icon = icon("project-diagram"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Καρτέλα: Πίνακας δεδομένων
      tabItem(tabName = "data_table",
              h2("Ανάλυση Δεδομένων"),
              fluidRow(
                box(title = "Επιλογές Δεδομένων", status = "primary", solidHeader = TRUE, width = 4,
                    selectInput("data_source", "Πηγή Δεδομένων:", 
                                choices = c("Ενσωματωμένο: Έλεγχος Επίδοσης" = "Έλεγχος_Επίδοσης",
                                            "Ενσωματωμένο: mtcars" = "mtcars", "Ενσωματωμένο: iris" = "iris",
                                            "Ενσωματωμένο: diamonds" = "diamonds", "Ενσωματωμένο: Orange" = "Orange",
                                            "Ενσωματωμένο: cars" = "cars", "Ενσωματωμένο: trees" = "trees",
                                            "Μεταφόρτωση Αρχείου" = "Αρχείο")),
                    
                    conditionalPanel(
                      condition = "input.data_source == 'Αρχείο'",
                      fileInput("file", "Επιλέξτε Αρχείο CSV ή Excel:",
                                buttonLabel = "Αναζήτηση...", placeholder = "Δεν έχει επιλεγεί αρχείο",
                                accept = c(".csv", ".xlsx"))
                    ),
                    uiOutput("column_selector")
                ),
                box(title = "Περιγραφή Δεδομένων", status = "info", solidHeader = TRUE, width = 8,
                    htmlOutput("data_description")
                )
              ),
              fluidRow(
                box(title="Προεπισκόπηση Πίνακα", DTOutput("data_table"), width = 12)
              )
      ),
      
      # Καρτέλα: Μέτρα θέσης
      tabItem(tabName = "measures_position",
              h2("Μέτρα Θέσης"),
              fluidRow(
                box(title = "Επιλογές Ιστογράμματος", status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("bar_selector")
                ),
                box(title = "Μέση τιμή - διάμεσος - τεταρτημόρια", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("mean_output"),
                    textOutput("median_output"),
                    textOutput("quartile_output")
                )
              ),
              fluidRow(
                box(plotOutput("histogram"), width = 12)
              )
      ),
      
      # Καρτέλα: Μέτρα διασποράς
      tabItem(tabName = "measures_dispersion",
              h2("Μέτρα Διασποράς"),
              fluidRow(
                # ΑΛΛΑΓΗ: Χρήση uiOutput για δυναμική προεπιλογή
                box(title = "Επιλογές Ανισότητας Chebyshev", status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("chebyshev_options")
                ),
                box(title = "Τυπική απόκλιση - Ανισότητα Chebyshev", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("std_output")
                )
              ),
              fluidRow(
                box(title = "Ενδοτεταρτημοριακό εύρος και ακραίες τιμές", status = "info", solidHeader = TRUE, width=8,
                    textOutput("iqr_output"),
                    textOutput("outliers_output"),
                    checkboxInput("show_outliers", "Εμφάνιση ακραίων τιμών στο θηκόγραμμα", TRUE)
                ),
                box(plotOutput("boxplot"), width = 4)
              )
      ),
      
      # Καρτέλα: Μέτρα ασυμμετρίας
      tabItem(tabName = "measures_skewness",
              h2("Μέτρα Ασυμμετρίας"),
              fluidRow(
                box(title = "Αποτελέσματα", status = "info", solidHeader = TRUE, width = 6,
                    textOutput("skewness_output")
                ),
                box(plotOutput("hist_boxplot"), width = 6)
              ),
              fluidRow(
                box(title = "Διάγραμμα Συμμετρίας", plotOutput("symmetry_plot"), width = 12)
              )
      ),
      
      # Καρτέλα: Μέτρα συσχέτισης
      tabItem(tabName = "measures_correlation",
              h2("Μέτρα Συσχέτισης"),
              fluidRow(
                box(title = "Επιλογές", status = "primary", solidHeader = TRUE, uiOutput("column_selector2"), width = 4),
                box(title = "Συντελεστές συσχέτισης", status = "info", solidHeader = TRUE, width = 8,
                    textOutput("cor_output"),
                    textOutput("spearman_output"),
                    textOutput("kendall_output")
                )
              ),
              fluidRow(
                box(title = "Ευθειακή Προσέγγιση", status = "info", solidHeader = TRUE, width = 4,
                    textOutput("regression_eq_output")),
                box(plotOutput("scatter_plot"), width = 8)
              )
      ),
      
      # Καρτέλα: Επίδραση παράγοντα
      tabItem(tabName = "factor_effect",
              h2("Επίδραση Παράγοντα σε Μεταβλητή"),
              fluidRow(
                box(title = "Επιλογές Ανάλυσης", status = "primary", solidHeader = TRUE, width = 4,
                    uiOutput("factor_selector"),
                    checkboxGroupInput("plot_options", "Επιλογές Γραφήματος:",
                                       choices = list("Ιστογράμματα" = "histograms", "Καμπύλες Πυκνότητας" = "density"),
                                       selected = "density")
                ),
                box(
                  title = "Στατιστικοί Έλεγχοι", status = "info", solidHeader = TRUE, width = 8,
                  textOutput("kruskal_output"),
                  textOutput("wilcoxon_r_output")
                )
              ),
              fluidRow(
                box(plotOutput("histograms"), width = 8),
                box(plotOutput("boxplot_comparison"), width = 4)
              )
      )
    )
  )
)

# --- 3. Λογική του Server ---
server <- function(input, output, session) {
  
  elegxos <- data.frame(
    Μαθήματα = c("Θρησκευτικά", "Αρχαία Γενικής", "Νεοελληνική Γλώσσα", "Νεοελληνική Λογοτεχνία", "Άλγεβρα", "Γεωμετρία", "Φυσική", "Χημεία", "Βιολογία", "Η/Υ", "Ιστορία", "Φιλοσοφία", "Πολιτική Παιδεία", "Αρχαία Προσανατολισμού", "Κοινωνιολογία"),
    Α_Τετράμηνο = c(13,12,10,11,11,11,10,11,12,13,11,10,14,13,14),
    Β_Τετράμηνο = c(13,13,12,13,13,13,12,12,12,16,12,12,14,13,14),
    Εξετάσεις = c(4, 6.6, 7, 12, 6, 3, 1, 3, 3, 5.6, 5, 6, 1, 10, 10),
    Κατεύθυνση = c("Θεωρητικό", "Θεωρητικό","Θεωρητικό","Θεωρητικό","Θετικό","Θετικό","Θετικό","Θετικό","Θετικό","Θετικό", "Θεωρητικό","Θεωρητικό","Θεωρητικό","Θεωρητικό","Θεωρητικό"),
    stringsAsFactors = FALSE
  )
  
  data <- reactive({
    if (input$data_source == "Αρχείο") {
      req(input$file)
      df <- tryCatch({
        ext <- tools::file_ext(input$file$name)
        if (ext == "csv") { read.csv(input$file$datapath, stringsAsFactors = FALSE) } 
        else if (ext %in% c("xls", "xlsx")) { readxl::read_excel(input$file$datapath) } 
        else { validate(need(FALSE, "Μη υποστηριζόμενος τύπος αρχείου.")); NULL }
      }, error = function(e) { validate(need(FALSE, paste("Σφάλμα ανάγνωσης:", e$message))); NULL })
      return(df)
    } else {
      switch(input$data_source, "mtcars" = mtcars, "iris" = iris, "Orange" = Orange,
             "cars" = cars, "trees" = trees, "diamonds" = ggplot2::diamonds, "Έλεγχος_Επίδοσης" = elegxos)
    }
  })
  
  output$data_description <- renderText({
    switch(input$data_source,
           "mtcars" = paste("Το dataset 'mtcars' περιλαμβάνει δεδομένα για αυτοκίνητα με 11 μεταβλητές:", "mpg (Κατανάλωση καυσίμου),", "cyl (Αριθμός κυλίνδρων),", "disp (Χωρητικότητα κινητήρα),", "hp (Ιπποδύναμη),", "drat (Σχέση μετάδοσης),", "wt (Βάρος),", "qsec (Χρόνος 1/4 μιλίου),", "vs (Μοτέρ V/S),", "am (Μετάδοση),", "gear (Αριθμός ταχυτήτων),", "carb (Αριθμός καρμπυρατέρ)."),
           "iris" = paste("Το dataset 'iris' περιλαμβάνει δεδομένα για 150 λουλούδια από 3 διαφορετικά είδη:", "Sepal.Length (Μήκος σέπαλου),", "Sepal.Width (Πλάτος σέπαλου),", "Petal.Length (Μήκος πέταλου),", "Petal.Width (Πλάτος πέταλου),", "Species (Είδος)."),
           "Orange" = paste("Το dataset 'Orange' περιλαμβάνει μετρήσεις ανάπτυξης πορτοκαλιών:", "Tree (Αριθμός δέντρου),", "Age (Ηλικία),", "circumference (Περίμετρος)."),
           "cars" = paste("Το dataset 'cars' περιλαμβάνει δεδομένα για την ταχύτητα και την απόσταση φρεναρίσματος:", "speed (Ταχύτητα),", "dist (Απόσταση)."),
           "trees" = paste("Το dataset 'trees' περιλαμβάνει δεδομένα για δέντρα:", "Girth (Περίμετρος),", "Height (Ύψος),", "Volume (Όγκος)."),
           "diamonds" = paste("Το dataset 'diamonds' περιλαμβάνει δεδομένα για >50.000 διαμάντια:", "carat,", "cut,", "color,", "clarity,", "depth,", "table,", "price,", "x, y, z."),
           "Αρχείο" = "Έχετε επιλέξει να φορτώσετε τα δικά σας δεδομένα από αρχείο.",
           "Έλεγχος_Επίδοσης" = "Ο κάτωθι πίνακας παρουσιάζει μέρος των βαθμών ενός, μάλλον όχι και τόσο καλού, μαθητή της Β’ Λυκείου... «Θα έπρεπε ο μαθητής με αυτούς τους βαθμούς να προαχθεί;»"
    )
  })
  
  output$column_selector <- renderUI({
    req(data())
    numeric_cols <- names(which(sapply(data(), is.numeric)))
    validate(need(length(numeric_cols) > 0, "Δεν υπάρχουν αριθμητικές στήλες."))
    selectInput("column", "Επιλέξτε Μεταβλητή (Αριθμητική):", choices = numeric_cols,
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Α_Τετράμηνο" else numeric_cols[1])
  })
  
  output$column_selector2 <- renderUI({
    req(data(), input$column)
    numeric_cols <- names(which(sapply(data(), is.numeric)))
    other_cols <- setdiff(numeric_cols, input$column)
    validate(need(length(other_cols) > 0, "Χρειάζεται δεύτερη αριθμητική μεταβλητή."))
    selectInput("column2", "Επιλέξτε Δεύτερη Μεταβλητή:", choices = other_cols,
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Β_Τετράμηνο" else other_cols[1])
  })
  
  output$bar_selector <- renderUI({
    req(data(), input$column)
    sliderInput("bins", "Αριθμός Μπλοκ Ιστογράμματος:", min = 5, max = 50, value = 15)
  })
  
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE, pageLength = 10, language = list(url = '//cdn.datatables.net/plug-ins/1.10.19/i18n/Greek.json')), rownames = FALSE)
  })
  
  # --- Υπολογισμοί Καρτελών ---
  
  # Καρτέλα «Μέτρα Θέσης»
  output$mean_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    mean_val <- mean(col_data, na.rm = TRUE)
    paste("Η μέση τιμή των τιμών", input$column, "είναι", round(mean_val, 2), ".")
  })
  
  output$median_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    median_val <- median(col_data, na.rm = TRUE)
    paste("Το 50% των τιμών", input$column, "είναι κάτω από", round(median_val, 2),"και το άλλο 50% πάνω από ",round(median_val, 2), ".")
  })
  
  output$quartile_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    quartiles <- quantile(col_data, probs = c(0.25, 0.75), na.rm = TRUE)
    paste("Το κεντρικό 50% των τιμών της", input$column, "είναι ανάμεσα στις τιμές", round(quartiles[1], 2), "και", round(quartiles[2], 2), ".")
  })
  
  output$histogram <- renderPlot({
    req(input$column, data(), input$bins)
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), ""))
    ggplot(data.frame(col_data), aes(x = col_data)) +
      geom_histogram(bins = input$bins, fill = "#3c8dbc", color = "white", alpha = 0.7) +
      labs(x = input$column, y = "Συχνότητα") + ggtitle(paste("Ιστόγραμμα για τη στήλη:", input$column)) + theme_minimal()
  })
  
  # Καρτέλα «Μέτρα Διασποράς»
  # ΑΛΛΑΓΗ: Δημιουργία του UI element στον server για δυναμική προεπιλογή
  output$chebyshev_options <- renderUI({
    req(input$column, data())
    col_data <- data()[[input$column]]
    validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    
    std_val <- sd(col_data, na.rm = TRUE)
    
    numericInput("distance", "Απόσταση από τη μέση τιμή:", 
                 value = round(3 * std_val, 2), # Προεπιλογή: 3 τυπικές αποκλίσεις
                 step = 0.1)
  })
  
  output$std_output <- renderText({
    req(input$column, data(), input$distance)
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    std_val <- sd(col_data, na.rm = TRUE)
    mean_val <- mean(col_data, na.rm = TRUE)
    a <- input$distance # a: απόσταση από τη μέση τιμή
    k <- a / std_val    # k: απόσταση σε όρους τυπικών αποκλίσεων
    
    if (k > 1) {
      chebyshev_prob <- 1 - 1 / (k^2)
      message <- paste("Η τυπική απόκλιση είναι", round(std_val, 2), ".",
                       "Τουλάχιστον το", round(chebyshev_prob * 100, 2), "% των τιμών βρίσκεται στο διάστημα [",
                       round(mean_val - a, 2), ",", round(mean_val + a, 2), "].")
    } else {
      message <- "Η ανισότητα Chebyshev δεν δίνει χρήσιμες πληροφορίες για απόσταση μικρότερη ή ίση με μία τυπική απόκλιση."
    }
    message
  })
  
  output$iqr_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    iqr_val <- IQR(col_data, na.rm = TRUE)
    paste("Το κεντρικό 50% των τιμών", input$column, "εκτείνεται σε πλάτος", round(iqr_val, 2), ".")
  })
  
  output$outliers_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    Q1 <- quantile(col_data, 0.25, na.rm = TRUE); Q3 <- quantile(col_data, 0.75, na.rm = TRUE)
    IQR_val <- IQR(col_data, na.rm = TRUE)
    lower_bound <- Q1 - 1.5 * IQR_val; upper_bound <- Q3 + 1.5 * IQR_val
    outliers <- col_data[col_data < lower_bound | col_data > upper_bound]
    if (length(outliers) > 0) { paste("Οι τιμές", paste(round(outliers, 2), collapse = ", "), "είναι παράταιρες.") } 
    else { paste("Δεν υπάρχουν απομονωμένες τιμές.") }
  })
  
  output$boxplot <- renderPlot({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), ""))
    ggplot(data.frame(col_data), aes(x="", y = col_data)) +
      geom_boxplot(fill = "#00c0ef", outlier.colour = if(input$show_outliers) "red" else NA, outlier.size = 3) +
      coord_flip() + labs(y = input$column, x = "") + ggtitle(paste("Θηκόγραμμα:", input$column)) +
      theme_minimal() + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())
  })
  
  # Καρτέλα «Μέτρα Ασυμμετρίας»
  output$skewness_output <- renderText({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), "Επιλέξτε αριθμητική στήλη."))
    skew_val <- moments::skewness(col_data, na.rm = TRUE)
    skewness_description <- if (abs(skew_val) <= 0.5) { "έχουμε συμμετρία" } 
    else if (skew_val < -0.5) { "οι τιμές είναι μαζεμένες αριστερά" } 
    else { "οι τιμές είναι μαζεμένες δεξιά" }
    paste("Ο συντελεστής ασυμμετρίας Pearson είναι", round(skew_val, 2), ", άρα", skewness_description, ".")
  })
  
  output$hist_boxplot <- renderPlot({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), ""))
    packHV::hist_boxplot(col_data, col.hist = "#f39c12", main = paste("Ιστόγραμμα & Θηκόγραμμα:", input$column))
  })
  
  output$symmetry_plot <- renderPlot({
    req(input$column, data())
    col_data <- data()[[input$column]]; validate(need(is.numeric(col_data), ""))
    sx <- sort(na.omit(col_data)); mx <- median(sx); n2 <- length(sx) %/% 2
    plot(mx - sx[1:n2], rev(sx)[1:n2] - mx, xlab = "Απόσταση Κάτω από τη Διάμεσο", ylab = "Απόσταση Πάνω από τη Διάμεσο", main = "Διάγραμμα Συμμετρίας", pch = 19, col = "darkblue")
    abline(a = 0, b = 1, lty = "dotted", col = "red", lwd = 2)
  })
  
  # Καρτέλα «Μέτρα Συσχέτισης»
  output$cor_output <- renderText({
    req(input$column, input$column2, data())
    col1<-data()[[input$column]]; col2<-data()[[input$column2]]; validate(need(is.numeric(col1) && is.numeric(col2), "Επιλέξτε αριθμητικές στήλες."))
    cor_val <- cor(col1, col2, method = "pearson")
    strength <- case_when(abs(cor_val) >= 0.9 ~ "πολύ ισχυρή", abs(cor_val) >= 0.6 ~ "ισχυρή", abs(cor_val) >= 0.3 ~ "μέτρια", abs(cor_val) >= 0.2 ~ "ασθενής", TRUE ~ "αμελητέα")
    paste("Συντελεστής Pearson:", round(cor_val, 2), ", άρα έχουμε", strength, "γραμμική συσχέτιση.")
  })
  
  output$spearman_output <- renderText({
    req(input$column, input$column2, data())
    col1<-data()[[input$column]]; col2<-data()[[input$column2]]; validate(need(is.numeric(col1) && is.numeric(col2), "Επιλέξτε αριθμητικές στήλες."))
    spearman_val <- cor(col1, col2, method = "spearman")
    strength <- case_when(abs(spearman_val) >= 0.7 ~ "πολύ ισχυρή", abs(spearman_val) >= 0.4 ~ "ισχυρή", abs(spearman_val) >= 0.2 ~ "μέτρια", TRUE ~ "αδύναμη")
    trend <- if (spearman_val > 0) "αυξητική" else "πτωτική"
    paste("Συντελεστής Spearman:", round(spearman_val, 2), ", άρα έχουμε", strength, trend,"τάση.")
  })
  
  output$kendall_output <- renderText({
    req(input$column, input$column2, data())
    col1<-data()[[input$column]]; col2<-data()[[input$column2]]; validate(need(is.numeric(col1) && is.numeric(col2), "Επιλέξτε αριθμητικές στήλες."))
    kendall_val <- cor(col1, col2, method = "kendall")
    trend <- if (kendall_val > 0) "αυξητική" else if (kendall_val < 0) "πτωτική" else "καμία"
    paste("Συντελεστής Kendall:", round(kendall_val, 2), ", άρα έχουμε", trend, "τάση.")
  })
  
  # ΑΛΛΑΓΗ: Χρήση ονομάτων στηλών στην εξίσωση
  output$regression_eq_output <- renderText({
    req(input$column, input$column2, data())
    col1_data <- data()[[input$column]]
    col2_data <- data()[[input$column2]]
    validate(need(is.numeric(col1_data) && is.numeric(col2_data), "Επιλέξτε αριθμητικές στήλες."))
    
    fit <- lm(col2_data ~ col1_data)
    slope <- round(coef(fit)[2], 2)
    intercept <- round(coef(fit)[1], 2)
    
    intercept_sign <- ifelse(intercept < 0, "-", "+")
    
    paste0(input$column2, " = ", slope, " * ", input$column, " ", intercept_sign, " ", abs(intercept))
  })
  
  output$scatter_plot <- renderPlot({
    req(input$column, input$column2, data())
    validate(need(is.numeric(data()[[input$column]]) && is.numeric(data()[[input$column2]]), ""))
    ggplot(data(), aes_string(x = input$column, y = input$column2)) +
      geom_point(alpha = 0.6, color = "darkblue", size = 2) +
      geom_smooth(method = "lm", col = "red", se = FALSE) +
      labs(x = input$column, y = input$column2) + ggtitle("Διάγραμμα Διασποράς & Γραμμή Παλινδρόμησης") + theme_minimal()
  })
  
  # Καρτέλα «Επίδραση Παράγοντα»
  output$factor_selector <- renderUI({
    req(data())
    factor_cols <- names(which(!sapply(data(), is.numeric) | sapply(data(), is.factor)))
    validate(need(length(factor_cols) > 0, "Δεν υπάρχουν κατηγορικές μεταβλητές."))
    selectInput("factor_column", "Επιλέξτε Παράγοντα:", choices = factor_cols,
                selected = if (input$data_source == "Έλεγχος_Επίδοσης") "Κατεύθυνση" else factor_cols[1])
  })
  
  output$kruskal_output <- renderText({
    req(input$column, input$factor_column, data())
    col_data <- data()[[input$column]]; factor_data <- as.factor(data()[[input$factor_column]]); validate(need(is.numeric(col_data), ""))
    kruskal_test <- kruskal.test(col_data ~ factor_data); p_value <- kruskal_test$p.value
    paste("Είναι", round(p_value*100, 2), "% πιθανό ο παράγοντας να μην έχει επίδραση (Kruskal-Wallis).")
  })
  
  output$wilcoxon_r_output <- renderText({
    req(input$column, input$factor_column, data())
    df <- data.frame(value = data()[[input$column]], group = as.factor(data()[[input$factor_column]]))
    validate(need(is.numeric(df$value) && nlevels(df$group) == 2, "Υποστηρίζεται μόνο για παράγοντες με 2 επίπεδα."))
    wilcox_r <- wilcox_effsize(value ~ group, data = df); r_value <- wilcox_r$effsize[1]
    effect_size <- case_when(abs(r_value) >= 0.4 ~ "μεγάλο", abs(r_value) >= 0.2 ~ "μεσαίο", TRUE ~ "μικρό")
    paste("Μέγεθος επίδρασης Wilcoxon:", round(r_value, 2), "(", effect_size, "μέγεθος επίδρασης).")
  })
  
  output$histograms <- renderPlot({
    req(input$column, input$factor_column, data(), input$plot_options)
    validate(need(is.numeric(data()[[input$column]]), ""))
    
    p <- ggplot(data(), aes_string(x = input$column, color = paste0("as.factor(",input$factor_column,")"))) +
      labs(title = paste("Κατανομή ανά", input$factor_column), color = input$factor_column) + theme_minimal()
    
    if ("histograms" %in% input$plot_options) {
      p <- p + geom_histogram(aes(y = ..density.., fill = paste0("as.factor(",input$factor_column,")")), position = "identity", alpha = 0.4, bins = 30)
    }
    if ("density" %in% input$plot_options) {
      p <- p + geom_density(alpha = 1, size = 1)
    }
    p
  })
  
  output$boxplot_comparison <- renderPlot({
    req(input$column, input$factor_column, data()); validate(need(is.numeric(data()[[input$column]]), ""))
    ggplot(data(), aes_string(x = input$factor_column, y = input$column, fill = input$factor_column)) +
      geom_boxplot(alpha = 0.7, outlier.shape = NA) +
      geom_jitter(shape = 16, size = 1.5, position = position_jitter(0.2), alpha=0.6) +
      labs(x = input$factor_column, y = input$column) + ggtitle("Σύγκριση με Θηκογράμματα") + 
      theme_minimal() + theme(legend.position = "none")
  })
}

# --- 4. Εκκίνηση της Εφαρμογής ---
shinyApp(ui = ui, server = server)
