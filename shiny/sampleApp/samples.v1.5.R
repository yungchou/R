library(shiny)
library(ggplot2, plotly)
library(dygraphs)
library(xts)    # To make the conversion data-frame / xts format

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
                 selectInput("thatDf", label = "Select a dataset", choices = ls(datasets))
               ),
               mainPanel(
                 downloadButton("theDownload", "Download the file"),
                 tableOutput("previewThatDf")
               )
             )
    ),
             
    tabPanel("App1",
      #"Ref: mastering-shiny.org/basic-reactivity.html",      
      fluidRow(
        column(4, 
               "Distribution 1",
               numericInput("n1", label = "n", value = 1000, min = 1),
               numericInput("mean1", label = "µ", value = 0, step = 0.1),
               numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4, 
               "Distribution 2",
               numericInput("n2", label = "n", value = 1000, min = 1),
               numericInput("mean2", label = "µ", value = 0, step = 0.1),
               numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
        ),
        column(4,
               "Frequency polygon",
               numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
               sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
        )
      ),
      fluidRow(
        column(9, plotOutput("hist")),
        column(3, verbatimTextOutput("ttest"))
      )
    ),
    
    tabPanel("Timer",
             #"Ref: mastering-shiny.org/basic-reactivity.html",      
             fluidRow(
               column(4, 
                      numericInput("lambda1", label = "lambda1", value = 3),
                      numericInput("lambda2", label = "lambda2", value = 5),
                      numericInput("n", label = "n", value = 1e4, min = 0)
               ),
               column(8,
                 fluidRow(plotOutput("reactive_hist")),
                 fluidRow(plotOutput("timer_hist"))
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
  
  # App1
  library(ggplot2)
  
  freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df <- data.frame(
      x = c(x1, x2),
      g = c(rep("x1", length(x1)), rep("x2", length(x2)))
    )
    
    ggplot(df, aes(x, colour = g)) +
      geom_freqpoly(binwidth = binwidth, size = 1) +
      coord_cartesian(xlim = xlim)
  }
  
  t_test <- function(x1, x2) {
    test <- t.test(x1, x2)
    
    # use sprintf() to format t.test() results compactly
    sprintf(
      "p value: %0.3f\n[%0.2f, %0.2f]",
      test$p.value, test$conf.int[1], test$conf.int[2]
    )
  }
  
  output$hist <- renderPlot({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
  }, res = 96)
  
  output$ttest <- renderText({
    x1 <- rnorm(input$n1, input$mean1, input$sd1)
    x2 <- rnorm(input$n2, input$mean2, input$sd2)
    
    t_test(x1, x2)
  })
  
  # Timer
  library(ggplot2)
  
  freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
    df <- data.frame(
      x = c(x1, x2),
      g = c(rep("x1", length(x1)), rep("x2", length(x2)))
    )
    
    ggplot(df, aes(x, colour = g)) +
      geom_freqpoly(binwidth = binwidth, size = 1) +
      coord_cartesian(xlim = xlim) +
      theme(legend.position = c(.9, .85))
      #theme(legend.position = 'bottom')
  }
  
  # Reactive without delay, i.e. no simulation effect
  rx1 <- reactive(rpois(input$n, input$lambda1))
  rx2 <- reactive(rpois(input$n, input$lambda2))
  output$reactive_hist <- renderPlot({
    freqpoly(rx1(), rx2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
  
  # The following code uses an interval of 500 ms 
  # so that the plot will update twice a second. 
  # This is fast enough to remind you that you're 
  # looking at a simulation, without dizzying you 
  # with rapid changes. 
  
  timer <- reactiveTimer(500)
  
  tx1 <- reactive({
    timer()
    rpois(input$n, input$lambda1)
  })
  tx2 <- reactive({
    timer()
    rpois(input$n, input$lambda2)
  })
  
  output$timer_hist <- renderPlot({
    freqpoly(tx1(), tx2(), binwidth = 1, xlim = c(0, 40))
  }, res = 96)
}

shinyApp(ui, server)
