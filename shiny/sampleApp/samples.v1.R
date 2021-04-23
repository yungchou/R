library(shiny)
library(ggplot2)

datasets <- "package:datasets"
num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")

ui <- fluidPage(
  
  # theme = bslib::bs_theme(bootswatch = "darkly"),
  # themes: darkly, sandstone, flatly, united
  
  tabsetPanel(id="inputMethods",

    tabPanel("Dataset",
      #"Ref: mastering-shiny.org/action-tidy.html",      
      sidebarLayout(
        sidebarPanel(
          selectInput("thisDf", label = "Select a dataset", choices = ls(datasets))
         ),
        mainPanel(
          verbatimTextOutput("dfSummary"),
          tableOutput("tableOut")      
        )
      )
    ),
    
 
    tabPanel("Eval",
      #"Ref: mastering-shiny.org/action-tidy.html",      
      sidebarLayout(
        sidebarPanel(
         selectInput("evalVar", "Variable", choices = num_vars),
         numericInput("evalMin", "Minimum", value = 1),
        ),
        mainPanel(
         tableOutput("evalOutput")
        )
      )
    ),
    
    tabPanel("Plot",
             #"Ref: mastering-shiny.org/action-tidy.html",      
             sidebarLayout(
               sidebarPanel(
                 selectInput("plotX", "X", choices = names(iris)),
                 selectInput("plotY", "Y", choices = names(iris))
               ),
               mainPanel(
                 plotOutput("plotIt")
               )
             )
    ),
    
    tabPanel("Upload",
             #"Ref: mastering-shiny.org/action-transfer.html",      
             sidebarLayout(
               sidebarPanel(
               ),
               mainPanel(
               )
             )
    ),
    
    tabPanel("Download",
             #"Ref: mastering-shiny.org/action-transfer.html",      
             sidebarLayout(
               sidebarPanel(
                 selectInput("thatDf", label = "Select a dataset", choices = ls(datasets)),
                 tableOutput("previewThatDf"),
               ),
               mainPanel(
                 downloadButton("theDownload", paste0("Download ",textOutput("fileName"),".tsv"))
               )
             )
    )
  )    

)

server <- function(input, output, session) {
  
  # File
  df <- reactive({ get(input$thisDf, datasets) })
  output$dfSummary <- renderPrint({ summary(df()) })
  output$tableOut  <- renderTable({ df() })
  
  # Eval
  evalResultTable <- reactive(diamonds %>% filter(.data[[input$evalVar]] > .env$input$evalMin))
  output$evalOutput <- renderTable(head(evalResultTable()))
  
  # Plot
  output$plotIt <- renderPlot({
    ggplot(iris, aes(.data[[input$plotX]], .data[[input$plotY]])) +
      geom_point(position = ggforce::position_auto()) +
      geom_smooth()
  }, res = 96)
  
  # Upload
  
  # Download
  theDownload <- reactive({
    out <- get(input$thatDf, datasets)
    if (!is.data.frame(out)) {
      validate(paste0("'", input$thatDf, "' is not a data frame"))
    }
    out
  })
  
  output$previewThatDf <- renderTable({
    head(theDownload())
  })
  
    
  output$theDownload <- downloadHandler(
    paste0(input$thatDf, ".tsv"),
    content = function(file){ vroom::vroom_write(theDownload(), file) }
  )
}

shinyApp(ui, server)
