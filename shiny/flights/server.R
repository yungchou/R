function(input, output) {
  output$count <- renderPlot(
    flights %>%
      filter(origin == input$origin & dest == input$dest) %>%
      group_by(carrier) %>%
      count() %>%
      ggplot(aes(x = carrier, y = n)) +
      geom_col(fill = "lightblue") +
      ggtitle("Number of flights")
  )
}