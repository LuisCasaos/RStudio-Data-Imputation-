library(shiny)
library(mice)
library(DMwR)
library(missForest)

ui <- fluidPage(
  titlePanel("Imputación de Datos Faltantes"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("imputation_method", "Seleccione el método de imputación:",
                  choices = c("Media", "k-NN", "MICE", "Random Forest")),
      actionButton("impute", "Imputar Datos")
    ),
    
    mainPanel(
      h3("Datos Originales"),
      tableOutput("originalDataTable"),
      h3("Datos Imputados"),
      tableOutput("imputedDataTable")
    )
  )
)

server <- function(input, output) {
  data <- reactive({
    iris[sample(1:nrow(iris), 20), c("Sepal.Length", "Sepal.Width")] <- NA  # Ejemplo con datos faltantes en múltiples columnas
    iris
  })
  
  imputed_data <- eventReactive(input$impute, {
    method <- input$imputation_method
    data <- data()
    
    if (method == "Media") {
      for (col in names(data)) {
        if (any(is.na(data[[col]]))) {
          data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = TRUE), data[[col]])
        }
      }
    } else if (method == "k-NN") {
      data <- knnImputation(data, k = 5)
    } else if (method == "MICE") {
      imputed <- mice(data, m = 1, method = 'pmm', maxit = 50, seed = 500)
      data <- complete(imputed)
    } else if (method == "Random Forest") {
      data <- missForest(data)$ximp
    }
    
    data
  })
  
  output$originalDataTable <- renderTable({
    data()  # Muestra los datos originales con valores faltantes
  })
  
  output$imputedDataTable <- renderTable({
    imputed_data()  # Muestra los datos después de la imputación
  })
}

shinyApp(ui = ui, server = server)
