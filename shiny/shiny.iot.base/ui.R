shinyUI(fluidPage(

  tags$h2("Stream Sensor Data",style="text-align:center;color:blue"),
     br(),
     br(),
  fluidRow(
        column(width=5, leafletOutput('mytest', height='500px')),
        column(width=5,
                leafletOutput("myleaflet",height = "500px")
        ),
        column(width=7,
                 plotOutput("timeseries_all",height = "500px"),
                  column(width=6, offset=5,
                         br(),
                         br(),
                 textOutput("Too_High")
                  ),
               column(width=6, offset=5,
                      textOutput("Failed_Sensors")
               )

        )
  ),
  fluidRow(
    column(6,C3GaugeOutput("gauge1"))
  )
))