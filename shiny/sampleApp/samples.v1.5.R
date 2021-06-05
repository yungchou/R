#-----------
# Libraries
#-----------
mylib <- c(
  "shiny","shinydashboard" #,"rattle"
  ,"ggplot2","plotly","dygraphs"
  ,"tidyr","dplyr"
  ,"readr","readxl","xlsx"
  #,"xts"    # To make the conversion data-frame / xts format
  #,"usmap"
  #,"proto"
  #,"gsubfn"
  #,"stringr"
  #,"RCurl"
  #,"RJSONIO"
  #,"sqldf"
  #,"magrittr"
)

#---------------------------
# Routine to load libraries
#---------------------------
{
  install.lib <- mylib[ !(mylib %in% installed.packages()[, "Package"]) ]
  for(item in install.lib) install.packages(item,dependences=TRUE)
  sapply(mylib,library,character=TRUE)
}

#------------
# REFERENCES
#------------
# Shiny HTML Tags: https://shiny.rstudio.com/articles/tag-glossary.html
# Bootstrap Style: http://bootstrapdocs.com/v3.3.6/docs/css/

#-----------------
# Referenced data
#-----------------
{
  datasets <- "package:datasets"
  num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")
}

#----------
# Frontend
#----------
ui <- fluidPage(

  
  # theme = bslib::bs_theme(bootswatch = "darkly"),
  # themes: darkly, sandstone, flatly, united
  
  tabsetPanel(id="inputMethods"
              
   ,tabPanel("Dataset", {
      #"Ref: mastering-shiny.org/action-tidy.html",      
      sidebarLayout(
        sidebarPanel(
          selectInput("thisDf", label = "Select a dataset", choices = ls(datasets))
         )
       ,mainPanel(
          verbatimTextOutput("dfSummary")
         ,tableOutput("tableOut")      
        )
      )
   })
   
   ,tabPanel("Eval", {
      #"Ref: mastering-shiny.org/action-tidy.html",      
      sidebarLayout(
        sidebarPanel(
          selectInput("evalVar", "Variable", choices = num_vars)
         ,numericInput("evalMin", "Minimum", value = 1)
        )
       ,mainPanel(
         tableOutput("evalOutput")
        )
      )
   })
   
   ,tabPanel("Plot Iris", {
      #"Ref: mastering-shiny.org/action-tidy.html",      
      sidebarLayout(
       sidebarPanel(
         selectInput("plotX", "X", choices = names(iris), selected=names(iris)[2])
        ,selectInput("plotY", "Y", choices = names(iris))
       )
      ,mainPanel(
         plotlyOutput("plotIt")
       )
      )
     })
   
   ,tabPanel("Upload", {
      #"Ref: mastering-shiny.org/action-transfer.html",      
      sidebarLayout(
       sidebarPanel(
#         fileInput("ufs", NULL, buttonLabel="Browse", multiple=TRUE, accept=c(".csv",".tsv"))
         fileInput("ufs", NULL, buttonLabel="Browse", multiple=FALSE, accept=c(".csv",".tsv"))
       )
      ,mainPanel(
         fluidRow(tableOutput("ufs"))
        ,fluidRow(verbatimTextOutput("ufSummary"))
        ,fluidRow(tableOutput("ufHead"))
       )
      )
    })
    
   ,tabPanel("Download", {
      #"Ref: mastering-shiny.org/action-transfer.html",      
      sidebarLayout(
       sidebarPanel(
         selectInput("thatDf", label = "Select a dataset", choices = ls(datasets), selected = "anscombe")
        ,downloadButton("theDownload", "Download the selected file", class = "btn-primary"),
       )
      ,mainPanel(
         tableOutput("previewThatDf")
       )
      )
    })
   
    # Reactive 
   ,{
     tabPanel("Reactive",
      #"Ref: mastering-shiny.org/basic-reactivity.html",      
      fluidRow(
        column(4,
               "Distribution 1",
               numericInput("n1", label = "n", value = 1000, min = 1),
               numericInput("mean1", label = "µ", value = 0, step = 0.1),
               numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
        ),column(4, 
               "Distribution 2",
               numericInput("n2", label = "n", value = 1000, min = 1),
               numericInput("mean2", label = "µ", value = 0, step = 0.1),
               numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
        ),column(4,
               "Frequency polygon",
               numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
               sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
        )
      ),
      fluidRow(
        column(9, plotlyOutput("hist")
      ),column(3, verbatimTextOutput("ttest"))
      )
    )
   }
   
    # Simulation
   ,{
     tabPanel("Simulation",
      #"Ref: mastering-shiny.org/basic-reactivity.html",      
      fluidRow(
        column(4,numericInput("lambda1", label = "lambda1", value = 3))
       ,column(4,numericInput("lambda2", label = "lambda2", value = 5))
       ,column(4,numericInput("nn", label = "nn", value = 1e4, min = 0))
        ),
      fluidRow(plotlyOutput("reactive_hist")),
      fluidRow(plotlyOutput("timer_hist"))
    )
   }
   
   ,tabPanel("Dynamic UI", {
      #"Ref: mastering-shiny.org/action-transfer.html",      
       sidebarLayout(
         sidebarPanel(
           selectInput("dynauiVar", "Select variable", choices = names(mtcars))
          ,sliderInput("dynauiMin", "Minimum value", 0, min = 0, max = 100)
          ,selectInput("dynauiSort", "Sort by", choices = names(mtcars))
          ,actionButton("dynauiReset", "Reset", class = "btn-primary")
           # http://bootstrapdocs.com/v3.3.6/docs/css/#buttons
        )
       ,mainPanel( tableOutput("dynauiData") )
      )
    })
   
   # REF
   ,{
   tabPanel("Ref", 
       fluidRow(
         column(4,
                h2("Reactive Test"),
                textInput("Test_R","Test_R"),
                textInput("Test_R2","Test_R2"),
                textInput("Test_R3","Test_R3"),
                tableOutput("React_Out")
         ),
         column(4,
                h2("Observe Test"),
                textInput("Test","Test"),
                textInput("Test2","Test2"),
                textInput("Test3","Test3"),
                tableOutput("Observe_Out")
         ),
         column(4,
                h2("Observe Event Test"),
                textInput("Test_OE","Test_OE"),
                textInput("Test_OE2","Test_OE2"),
                textInput("Test_OE3","Test_OE3"),
                tableOutput("Observe_Out_E"),
                actionButton("Go","Test")
         )
         
       ),
       fluidRow(
         column(8,
                h4("Note that observe and reactive work very much the same on the surface,
       it is when we get into the server where we see the differences, and how those
       can be exploited for diffrent uses.")
         ))
   )
   
  }
  
  )
  
)

#----------
# Back-end
#----------
server <- function(input, output, session) {
  
  # File
  {
    df <- reactive({ get(input$thisDf, datasets) })
    output$dfSummary <- renderPrint({ summary(df()) })
    output$tableOut  <- renderTable({ df() })
  }
  
  # Eval
  {
    evalResultTable <- reactive(diamonds %>% filter(.data[[input$evalVar]] > .env$input$evalMin))
    output$evalOutput <- renderTable(head(evalResultTable()))
  }
  
  # Plot
  {
    output$plotIt <- renderPlotly(
      ggplotly(
        ggplot(iris, aes(.data[[input$plotX]], .data[[input$plotY]])) 
        + geom_point(position = ggforce::position_auto()) 
        + geom_smooth()
      )  
    )
  }
      
  # Upload
  {
    output$ufs <- renderTable(input$ufs)
    
    uf <- reactive({ get(input$ufs) })
    output$ufHead  <- renderTable({ head(uf()) })
    
    ufData <- reactive({
      req(input$ufs)
      
      ext <- tools::file_ext(input$ufs$name)
      switch(ext,
             csv = vroom::vroom(input$ufs$datapath, delim = ","),
             tsv = vroom::vroom(input$ufs$datapath, delim = "\t"),
             validate("Invalid file; Please upload a .csv or .tsv file")
      )
    })
    
    output$ufSummary <- renderPrint(summary(ufData()))
    output$ufHead <- renderTable(head(ufData()))
    
  }
  
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
  
  # Reactive
  {
    freqpoly <- function(x1, x2, title='', binwidth = 0.1, xlim = c(-3, 3)) {
      df <- data.frame(
        x = c(x1, x2),
        y = c(rep("x1", length(x1)), rep("x2", length(x2)))
      )
      ggplotly(
        ggplot(df, aes(x, colour=y)) + labs(title = title) +
        geom_freqpoly(binwidth = binwidth, size = 1, alpha=0.5) + 
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
      freqpoly(x1, x2, title='Random Samples', binwidth = input$binwidth, xlim = input$range)
    })
    
    output$ttest <- renderText({
      x1 <- rnorm(input$n1, input$mean1, input$sd1)
      x2 <- rnorm(input$n2, input$mean2, input$sd2)
      t_test(x1, x2)
    })
  }
  
  # Simulation
  # Part One Static Plot
  {
    freq_poly <- function(x1, x2, title='', binwidth = 0.1, xlim = c(-3, 3)) {
      df <- data.frame(
        x = c(x1, x2),
        y = c(rep("x1", length(x1)), rep("x2", length(x2)))
      )
      ggplotly(
        ggplot(df, aes(x, colour=y)) + labs(title = title) +
          geom_freqpoly(binwidth = binwidth, size=1, alpha=0.3) +
          coord_cartesian(xlim = xlim) +
          #theme(legend.position = c(.9, .85))'
          #theme(legend.position = "none")  # remove all legends
          theme(legend.position="top")
      )
    }
    
    # Reactive without delay, i.e. no simulation effect
    rx1 <- reactive(rpois(input$nn, input$lambda1))
    rx2 <- reactive(rpois(input$nn, input$lambda2))
    output$reactive_hist <- renderPlotly(
      freq_poly(rx1(), rx2(),title="Static Plot", binwidth = 1, xlim = c(0, 40))
    )
   }
    
 # Part Two Dynamic Plot
  {
    
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
      freq_poly(tx1(), tx2(),title="Simulated Real-Time Plot",binwidth = 1, xlim = c(0, 40))
    )
    
  }

  # Dynamic UI
  {
    observeEvent(input$dynauiReset, {
      updateSliderInput(inputId = "dynauiVar", value=names(mtcars)[1])
      updateSliderInput(inputId = "dynauiMin", value=0)
      updateSliderInput(inputId = "dynauiSort", value=names(mtcars)[1])
    })
    
    observeEvent(input$dynauiVar, {
      rng <- range(mtcars[[input$dynauiVar]])
      updateSliderInput(
        session, "min", 
        value = rng[[1]], 
        min = rng[[1]], 
        max = rng[[2]]
      )
    })
    
    output$dynauiData <- renderTable({
      mtcars %>% 
        filter(.data[[input$dynauiVar]] > input$dynauiMin) %>% 
        arrange(.data[[input$dynauiSort]])
    })
    
  }

  # REF of Reactive, OBserve
  {
    # Create a reactive Evironment. Note that we can call the varaible outside same place
    # where it was created by calling Reactive_Var(). When the varaible is called by
    # renderTable is when it is evaluated. No real diffrence on the surface, all in the server.
    
    Reactive_Var<-reactive({c(input$Test_R, input$Test_R2, input$Test_R3)})
    
    output$React_Out<-renderTable({
      Reactive_Var()
    })
    
    # Create an observe Evironment. Note that we cannot access the created "df" outside 
    # of the env. A, B,and C will update with any input into any of the three Text Feilds.
    observe({
      A<-input$Test
      B<-input$Test2
      C<-input$Test3
      df<-c(A,B,C)
      output$Observe_Out<-renderTable({df})
    })
    
    #We can change any input as much as we want, but the code wont run until the trigger
    # input$Go is pressed.
    observeEvent(input$Go, {
      A<-input$Test_OE
      B<-input$Test_OE2
      C<-input$Test_OE3
      df<-c(A,B,C)
      output$Observe_Out_E<-renderTable({df})
    })
  }
  
}

#--------
# Run it
#--------
shinyApp(ui, server)
