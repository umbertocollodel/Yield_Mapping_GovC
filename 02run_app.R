# Start a shiny app showing the results


ui <- pageWithSidebar(
  headerPanel('ECB Monetary Surprises'),
  sidebarPanel(
    selectInput('time', 'Component Monetary Statement', names(df_surprises)),
    sliderInput('date',
                   label = 'GovC Meetings',
                   min  = 2001, 
                   max = 2023,
                   value = 2005)
                               
    ),
  mainPanel(
    plotOutput('plot1')
  )
)



server <- function(input, output, session) {
 

  
   # Clean for time serie plotting
  
  dfInput <- reactive({time_serie_df %>% 
    filter(id %in% input$time) %>% 
    filter(between(year(date),min(input$date),max(input$date)))})
  

  # Plot:
  
  
  output$plot1 <- renderPlot({
    df1 <- dfInput()
    df1 %>% 
      
    ggplot(aes(date,value, fill=Factor)) +
    geom_col(width = 0.5) +
    labs(title="",
         y="Standard Deviation",
         x="",
         fill="",
         caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them. An additional model employs the same methodology for the variation of 10 years spreads (IT-DE, SP-DE   
and FR-DE) against German bunds around GovC meetings. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y, IT-DE 10Y Spread).  
**Source**: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  
**Latest observation**: 15 June 2023.") +  
    theme_bw() +
    theme(plot.caption = element_text(hjust=0)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_manual(values=c("#35BBCA","#0191B4","#D3DD18","#FE7A15")) +
    theme( axis.text = element_text( size = 14 ),
           axis.text.x = element_text( size = 20 ),
           axis.title = element_text( size = 16 ),
           legend.position="bottom",
           legend.text = element_text(size=14),
           strip.text = element_text(size = 20)) +
    theme(plot.caption = element_markdown(hjust = 0,size=12))
})
}


shinyApp(ui, server)