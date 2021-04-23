library(shiny)

datasets <- "package:datasets"
num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")

ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # themes: darkly, sandstone, flatly, united
  
  tabsetPanel(id="inputMethods",

    tabPanel("Dataset",
      sidebarLayout(
        sidebarPanel(
          selectInput("dfSelected", label = "Select a dataset", choices = ls(datasets))
         ),
        mainPanel(
          verbatimTextOutput("dfSummary"),
          tableOutput("tableOut")      
        )
      )
    ),
    
    tabPanel("Eval",
      sidebarLayout(
        sidebarPanel(
         selectInput("evalVar", "Variable", choices = num_vars),
         numericInput("evalMin", "Minimum", value = 1),
        ),
        mainPanel(
         tableOutput("evalOutput")
        )
      )
    )
    
 
  )    

)

server <- function(input, output, session) {
  
  # File
  df <- reactive({ get(input$dfSelected, datasets) })
  output$dfSummary <- renderPrint({ summary(df()) })
  output$tableOut  <- renderTable({ df() })
  
  # Eval
  evalResultTable <- reactive(diamonds %>% filter(.data[[input$evalVar]] > .env$input$evalMin))
  output$evalOutput <- renderTable(head(evalResultTable()))
  
}

shinyApp(ui, server)
