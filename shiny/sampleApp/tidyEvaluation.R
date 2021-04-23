library(shiny,bslib,vroom)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2,ggforce)

animals <- c("dog", "cat", "mouse")

ui <- fluidPage(
  
  #theme = bslib::bs_theme(bootswatch = "darkly"),
  
  titlePanel("Yung's Sample Shiny App"),
  
  tabsetPanel(
    tabPanel("tab1", 
      sidebarLayout("fileInput",
        sidebarPanel(
          fileInput(
            "tab1File", "Data", buttonLabel = "Browse", accept = c(".csv",".tsv","txt"), multiple = TRUE
          ),      
        ),
        mainPanel(tableOutput("head")))
    ),
    tabPanel("tab2",
      sidebarLayout("selectInput",
        sidebarPanel(
          selectInput("tab2Var", "Variable", choices = num_vars,
                        num_vars <- c("carat", "depth", "table", "price", "x", "y", "z")),
            numericInput("tab2Min", "Minimum", value = 1),
        ),
        mainPanel(tableOutput("tab2Output")))
    ),
    tabPanel("tab3",
      sidebarLayout(
        sidebarPanel("selectInput",
          selectInput("tab3X", "X variable", choices = names(iris)),
          selectInput("tab3Y", "Y variable", choices = names(iris)),
        ),
        mainPanel(plotOutput("tab3Plot")))
    ),
  )
)

server <- function(input, output, session) {
  tab1Data <- reactive({
    req(input$tab1File)
    
    ext <- tools::file_ext(input$tab1File$name)
    switch(ext,
           csv = vroom::vroom(input$tab1File$datapath, delim = ","),
           tsv = vroom::vroom(input$tab1File$datapath, delim = "\t"),
           txt = vroom::vroom(input$tab1File$datapath, delim = ""),
           validate("Invalid file; Please upload a .csv, .tsv, or .txt file")
    )
  })
  
  output$head <- renderTable({
    head(tab1Data(), input$n)
  })
  
  tab2Data <- reactive(diamonds %>% filter(.data[[input$tab2Var]] > .env$input$tab2Min))
  output$tab2Output <- renderTable(head(tab2Data()))
  
  output$tab3Plot <- renderPlot({
    ggplot(iris, aes(.data[[input$tab3X]], .data[[input$tab3Y]])) + 
    geom_point(position = ggforce::position_auto()) + 
    geom_smooth()
  }, res = 96)

}

shinyApp(ui, server)
