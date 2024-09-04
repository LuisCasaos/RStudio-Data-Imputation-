library(shiny)
library(mice)
library(DMwR)
library(missForest)
library(readxl)  # Para leer archivos .xls y .xlsx

ui <- fluidPage(
  titlePanel("Imputación de Datos Faltantes"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Seleccione un archivo CSV o Excel:",
                accept = c(".csv", ".xls", ".xlsx")),
      checkboxInput("transpose", "Transponer los datos", value = FALSE),
      selectInput("imputation_method", "Seleccione el método de imputación:",
                  choices = c("Media", "k-NN", "MICE", "Random Forest")),
      actionButton("impute", "Imputar Datos")
    ),
    
    mainPanel(
      fluidRow(
        column(6, h3("Datos Originales"), tableOutput("originalDataTable")),
        column(6, h3("Datos Imputados"), tableOutput("imputedDataTable"))
      )
    )
  )
)

server <- function(input, output) {
  
  # Función para leer el archivo cargado
  data <- reactive({
    req(input$file)  # Se asegura de que haya un archivo cargado
    ext <- tools::file_ext(input$file$name)
    
    if (ext == "csv") {
      data <- read.csv(input$file$datapath)
    } else if (ext == "xls" || ext == "xlsx") {
      data <- read_excel(input$file$datapath)
    }
    
    # Introducimos NA's en algunas filas de ejemplo (puedes quitar esto en producción)
    data[sample(1:nrow(data), min(20, nrow(data))), sample(1:ncol(data), min(2, ncol(data)))] <- NA
    data
  })
  
  # Función para realizar la imputación
  imputed_data <- eventReactive(input$impute, {
    req(data())  # Se asegura de que haya datos disponibles
    method <- input$imputation_method
    dataset <- data()
    
    if (input$transpose) {
      dataset <- t(dataset)
    }
    
    if (method == "Media") {
      for (col in names(dataset)) {
        if (any(is.na(dataset[[col]]))) {
          dataset[[col]] <- ifelse(is.na(dataset[[col]]), mean(dataset[[col]], na.rm = TRUE), dataset[[col]])
        }
      }
    } else if (method == "k-NN") {
      dataset <- knnImputation(dataset, k = 5)
    } else if (method == "MICE") {
      imputed <- mice(dataset, m = 1, method = 'pmm', maxit = 50, seed = 500)
      dataset <- complete(imputed)
    } else if (method == "Random Forest") {
      dataset <- missForest(dataset)$ximp
    }
    
    dataset
  })
  
  # Mostrar datos originales
  output$originalDataTable <- renderTable({
    dataset <- data()
    if (input$transpose) {
      dataset <- t(dataset)
    }
    dataset
  })
  
  # Mostrar datos imputados
  output$imputedDataTable <- renderTable({
    imputed_data()
  })
}

shinyApp(ui = ui, server = server)
