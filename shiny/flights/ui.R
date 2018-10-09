fluidPage(
  titlePanel("NYC Flights 2014"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "origin",
                     label = "Departure airport",
                     choices = unique(flights$origin)),
      selectizeInput(inputId = "dest",
                     label = "Arrival airport",
                     choices = unique(flights$dest))
    ),
    mainPanel(plotOutput("count"))
  )
)