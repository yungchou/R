pkgs <- c(
   "shiny","shinydashboard"
  ,"ggplot2","plotly","dygraphs"
  ,"tidyr","dplyr"
  ,"rattle","magrittr"
  ,"readr","readxl","xlsx"
  #,"xts"    # To make the conversion data-frame / xts format
  #  ,"usmap"
  #  ,"proto"
  #  ,"gsubfn"
  #  ,"stringr"
  #  ,"RCurl"
  #  ,"RJSONIO"
  #  ,"sqldf"
)
for (i in pkgs){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}
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
    
    tabPanel("Perfmon Log",
             #"Ref: mastering-shiny.org/action-tidy.html",
             sidebarLayout(
               sidebarPanel(
                 selectInput("holding", label = "Select a log", choices = c('a','b','c'))
               ),
               mainPanel(
                 #verbatimTextOutput("logSummary"),
                 tableOutput("logOutput")      
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
                 plotlyOutput("plotIt")
               )
             )
    ),
    
    tabPanel("Upload",
             #"Ref: mastering-shiny.org/action-transfer.html",      
             sidebarLayout(
               sidebarPanel(
                 fileInput("uploadFiles", NULL, buttonLabel = "Upload...", multiple = TRUE),
               ),
               mainPanel(
                  tableOutput("uploadedFiles")
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
        column(9, plotlyOutput("hist")),
        column(3, verbatimTextOutput("ttest"))
      )
    ),
    
    tabPanel("App2",
      #"Ref: mastering-shiny.org/action-transfer.html",      
      sidebarLayout(
       sidebarPanel(
         selectInput("app2Var", "Select variable", choices = names(mtcars)),
         sliderInput("app2Min", "Minimum value", 0, min = 0, max = 100),
         selectInput("app2Sort", "Sort by", choices = names(mtcars)),
         actionButton("app2Reset", "Reset")
       ),
       mainPanel( tableOutput("app2Data") )
      )
    ),
    
    tabPanel("Timer",
             #"Ref: mastering-shiny.org/basic-reactivity.html",      
             fluidRow(
               column(4, 
                      numericInput("lambda1", label = "lambda1", value = 3),
                      numericInput("lambda2", label = "lambda2", value = 5),
                      numericInput("nn", label = "nn", value = 1e4, min = 0)
               ),
               column(8,
                      fluidRow(plotlyOutput("reactive_hist")),
                      fluidRow(plotlyOutput("timer_hist"))
               )
             )
    ),
    
    tabPanel("sys.info()",
             #"Ref: mastering-shiny.org/basic-reactivity.html",      
             fluidRow(
               column(4, h2("Sys.info()")),
               column(8, tableOutput("sysinfo"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  # File
  {
  df <- reactive({ get(input$thisDf, datasets) })
  output$dfSummary <- renderPrint({ summary(df()) })
  output$tableOut  <- renderTable({ df() })
  
  # Eval
  evalResultTable <- reactive(diamonds %>% filter(.data[[input$evalVar]] > .env$input$evalMin))
  output$evalOutput <- renderTable(head(evalResultTable()))
  }
  
  # Perfmon Log
  {
    theLog <- read.csv('perfmon.log.sample.csv', header = TRUE)
    #theLog <- reactive({ get(input$thisLog, logs) })
    output$logSummary <- renderPrint({ summary(theLog()) })
    output$logOut  <- renderTable({ theLog() })
    
    # Eval
    evalResultLog <- reactive(diamonds %>% filter(.data[[input$logVar]] > .env$input$logMin))
    output$logOutput <- renderTable(head(evalResultLog()))
  }
  
  # Plot
  output$plotIt <- renderPlotly({
    ggplotly(
      ggplot(iris, aes(.data[[input$plotX]], .data[[input$plotY]])) +
        geom_point(position = ggforce::position_auto()) +
        geom_smooth()
    )})
      
  # Upload
  output$uploadedFiles <- renderTable(input$uploadFiles)
  
  # Download
  {
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
  
  # App1
  {
    library(ggplot2, plotly)
    
    freqpoly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
      df <- data.frame(
        x = c(x1, x2),
        g = c(rep("x1", length(x1)), rep("x2", length(x2)))
      )
      ggplotly(
        ggplot(df, aes(x, colour = g)) +
          geom_freqpoly(binwidth = binwidth, size = 1) +
          coord_cartesian(xlim = xlim)      
      )
  
    }
    
    t_test <- function(x1, x2) {
      test <- t.test(x1, x2)
      
      # use sprintf() to format t.test() results compactly
      sprintf(
        "p value: %0.3f\n[%0.2f, %0.2f]",
        test$p.value, test$conf.int[1], test$conf.int[2]
      )
    }
    
    output$hist <- renderPlotly({
      x1 <- rnorm(input$n1, input$mean1, input$sd1)
      x2 <- rnorm(input$n2, input$mean2, input$sd2)
      
      freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
    })
    
    output$ttest <- renderText({
      x1 <- rnorm(input$n1, input$mean1, input$sd1)
      x2 <- rnorm(input$n2, input$mean2, input$sd2)
      
      t_test(x1, x2)
    })
  }

  # App2
  {
    # Ref: mastering-shiny.org/action-tidy.html
    observeEvent(input$app2Reset, {
      updateSliderInput(inputId = "app2Var", value=names(mtcars)[1])
      updateSliderInput(inputId = "app2Min", value=0)
      updateSliderInput(inputId = "app2Sort", value=names(mtcars)[1])
    })
    
    observeEvent(input$app2Var, {
    rng <- range(mtcars[[input$app2Var]])
    updateSliderInput(
      session, "min", 
      value = rng[[1]], 
      min = rng[[1]], 
      max = rng[[2]]
    )
  })
    
    output$app2Data <- renderTable({
      mtcars %>% 
        filter(.data[[input$app2Var]] > input$app2Min) %>% 
        arrange(.data[[input$app2Sort]])
    })
    
  }
  
  # Timer
  {
    library(ggplot2)
    library(plotly)
    
    freq_poly <- function(x1, x2, binwidth = 0.1, xlim = c(-3, 3)) {
      df <- data.frame(
        x = c(x1, x2),
        g = c(rep("x1", length(x1)), rep("x2", length(x2)))
      )
      ggplotly(
        ggplot(df, aes(x, colour = g)) +
          geom_freqpoly(binwidth = binwidth, size = 1) +
          coord_cartesian(xlim = xlim) +
          #theme(legend.position = c(.9, .85))'
          #theme(legend.position = "none")  # remove all legends
          theme(legend.position = "top")
      )
    }
  }

  # Reactive without delay, i.e. no simulation effect
  {
    rx1 <- reactive(rpois(input$nn, input$lambda1))
    rx2 <- reactive(rpois(input$nn, input$lambda2))
    output$reactive_hist <- renderPlotly(
      freq_poly(rx1(), rx2(), binwidth = 1, xlim = c(0, 40))
    )
    
    # The following code uses an interval of 500 ms 
    # so that the plot will update twice a second. 
    # This is fast enough to remind you that you're 
    # looking at a simulation, without dizzying you 
    # with rapid changes. 
    
    timer <- reactiveTimer(500)
    
    tx1 <- reactive({
      timer()
      rpois(input$nn, input$lambda1)
    })
    tx2 <- reactive({
      timer()
      rpois(input$nn, input$lambda2)
    })
    
    output$timer_hist <- renderPlotly(
      freq_poly(tx1(), tx2(), binwidth = 1, xlim = c(0, 40))
    )
        
  }

  # Sys.info
  output$sysinfo <- renderTable({
    sysinfoDf<-data.frame(sys.info())
    sysinfoDf
  })
}

shinyApp(ui, server)
