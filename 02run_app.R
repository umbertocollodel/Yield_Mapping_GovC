# Start a shiny app showing the results


pageWithSidebar(
  headerPanel('ECB Monetary Surprises'),
  sidebarPanel(
    selectInput('time', 'Component Monetary Statement', names(df_surprises)),
    dateRangeInput('date',
                   label = 'GovC Meetings',
                   start = df_surprises$date[1] , end = tail(df_surprises$date[2],1)),
    ),
  mainPanel(
    plotOutput('plot1')
  )
)



server <- function(input, output, session) {
  output$hist <- renderPlot({
    means <- replicate(1e4, mean(runif(input$m)))
    hist(means, breaks = 20)
  }, res = 96)
}


shinyApp(ui, server)