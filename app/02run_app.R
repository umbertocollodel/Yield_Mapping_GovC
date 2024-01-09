# Prepare the environment (libraries): 

pkgs <- c("shiny","shinythemes","tidyverse","lubridate",
               "plotly")

lapply(pkgs, require, character.only = TRUE)




# Start a shiny app showing the results


ui <- fluidPage(
  theme = shinytheme("superhero"),
  headerPanel('ECB Monetary Surprises'),
  sidebarPanel(
    selectInput('time', 'Component', unique(time_serie_df$id)),
    radioButtons("granular","Granularity",c("Aggregate","Individual Factor")),
      conditionalPanel(
        condition = "input.granular == 'Individual Factor'",
        selectInput("factor", "Factor", as.character(unique(time_serie_df$Factor)))
      ),
    sliderInput('date',
                   label = 'Time Range: GovC Meetings',
                   min  = 2001, 
                   max = 2023,
                   value = c(2015,2018)),
    h5("Component: Press release calculates the difference between 13.15 and 13.45 values and extracts factors.
       Press conference calculates the difference between 14.30 and 15.30 valus and extracts factors.
       Monetary statetement combines the two."),
    h5("Granularity: aggreggate plots all factors in a stacked bar chart. Individual focuses 
       on individual factors time series. If individual selected, a window appears with the different choices.
       "),
    h5("Time range:
       ")
    
    ),
  mainPanel(
    plotlyOutput('plot1'),
    plotOutput("plot"),
    textOutput("note_plot",)
  
  )
)



server <- function(input, output, session) {
 
  time_serie_df <- read_rds("app_data/app_data.rds")
  
  
   # Clean for time serie plotting
  
  dfInput <- reactive({
    if(input$granular == "Individual Factor"){
    time_serie_df %>% 
    filter(id %in% input$time) %>% 
    filter(Factor %in% input$factor) %>% 
    filter(between(year(date),min(input$date),max(input$date)))
      }
    
   else{
      time_serie_df %>% 
      filter(id %in% input$time) %>% 
      filter(between(year(date),min(input$date),max(input$date)))}
    }
    )
  

  # Plot:
  
  output$plot1 <- renderPlotly({
    df1 <- dfInput()
    gg  <- df1 %>% 
      ggplot(aes(date,value, fill=Factor)) +
      geom_col(width = 0.5) +
      labs(title="",
           y="Standard Deviation",
           x="",
           fill="",
           caption = "") +  
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
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    theme(plot.caption = element_markdown(hjust = 0,size=12))
    ggplotly(gg)})
  
  
  output$plot <- renderPlot(load_df %>%
    bind_rows(.id="event") %>% 
    mutate(Term = factor(Term, levels = c("1M","3M","6M","1Y","2Y","5Y","10YY"))) %>% 
    pivot_longer(matches("Path|QE|Timing"),names_to = "Factor") %>% 
    mutate(Factor = factor(Factor, levels = c("Timing","Path","QE"))) %>% 
    mutate(event= factor(event,levels=c("Press Release","Press Conference"))) %>% 
    ggplot(aes(Term,value, group=event, col=event)) +
    geom_line(size=2) +
    facet_wrap(~Factor) +
    labs(title="",
         col="",
         y="",
         x="",
         fill="") +  
    theme_bw() +
    theme(plot.caption = element_text(hjust=0)) +
    theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
    theme( axis.text = element_text( size = 14 ),
           axis.text.x = element_text( size = 20 ),
           axis.title = element_text( size = 16, face = "bold" ),
           legend.text = element_text(size=14),
           # The new stuff
           strip.text = element_text(size = 20)) +
    theme(plot.caption = element_markdown(hjust = 0,size=12)),
    height = 350, width = 1113.5
  )
  

    
    
    output$note_plot <- renderText("The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them. An additional model employs the same methodology for the variation of 10 years spreads (IT-DE, SP-DE   
and FR-DE) against German bunds around GovC meetings. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y, IT-DE 10Y Spread). 

Source: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  

Latest observation: 15 June 2023.")
}



shinyApp(ui, server)