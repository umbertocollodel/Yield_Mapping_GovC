# Prepare the environment (libraries and data):  ----

# Packages: ----
Vectorize(require)(package = c("shiny", "ggplot2", "dplyr", "tidyr",
                               "lubridate","plotly","shinythemes",
                               "shinydashboard","ggtext"), character.only = TRUE)


# Factor decomposition dataframe:

time_serie_df <- readRDS("app_data/app_data.rds") %>% 
  mutate(value = round(value,2)) %>% 
  rename( sd = value)

# Loadings dataframe:

loadings_df <- readRDS("app_data/app_data_loadings.rds")

# UI ----


ui <- fluidPage(
  theme = shinytheme("superhero"),
  headerPanel('ECB Monetary Surprises'),
  sidebarPanel(
    selectInput('event','Event', c("All","GovC","Special Release")),
    selectInput('time', 'Component', unique(time_serie_df$id)),
    radioButtons("granular","Granularity",c("Aggregate","Individual Factor")),
      conditionalPanel(
        condition = "input.granular == 'Individual Factor'",
        selectInput("factor", "Factor", as.character(unique(time_serie_df$Factor)))
      ),
    sliderInput('date',
                   label = 'Time Range',
                   min  = 2001, 
                   max = 2023,
                   value = c(2020,2023)),
    downloadButton("download","Download Data"),
    h5("Event: ECB governing council or other extraordinary release from ECB."),
    h5("Component: press release refers to changes occured before/after the ECB GovC
       press release. In the case of Special releases, it is based on the next day values.
       Press conference refers to changes occured before/after the ECB GovC press
       conference. Given that special releases are not followed by any conference,
       the values are nought. Same reasonmentfor the whole onetary statement."),
    h5("Granularity: aggreggate plots all factors in a stacked bar chart. Individual focuses 
       on individual factors time series. If individual selected, a window appears with the different choices.
       "),
    h5("Time range: year range for all ECB Governing Council meetings.
       ")
    
    ),
  mainPanel(
    plotlyOutput('plot1'),
    plotlyOutput("plot"),
    textOutput("note_plot",)
  
  )
)

# Server ------

server <- function(input, output, session) {
 
  
  
   # Clean for time serie plotting -----
  
  dfInput <- reactive({
    
    if(input$event == "All"){
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
    
    else{
      if(input$granular == "Individual Factor"){
          time_serie_df %>% 
          filter(special %in% input$event) %>% 
          filter(id %in% input$time) %>% 
          filter(Factor %in% input$factor) %>% 
          filter(between(year(date),min(input$date),max(input$date)))
      }
      
      else{
        time_serie_df %>% 
          filter(special %in% input$event) %>% 
          filter(id %in% input$time) %>% 
          filter(between(year(date),min(input$date),max(input$date)))}
    }
  }
  
  
    )
  

  # Plot time serie factors: -----
  
  output$plot1 <- renderPlotly({
    df1 <- dfInput()
    gg  <- df1 %>% 
      ggplot(aes(date,sd, fill=Factor)) +
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
  
  # Plot factor loadings: ----
  
  
  output$plot <- renderPlotly({
    gg1 <- loadings_df %>% 
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
    theme(axis.text.x = element_text(angle = 270, hjust = 1)) +
    theme(plot.caption = element_markdown(hjust = 0,size=12)) 
    ggplotly(gg1)})
  

# Methodological note: -----  
    
    output$note_plot <- renderText("The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them. An additional model employs the same methodology for the variation of 10 years spreads (IT-DE, SP-DE   
and FR-DE) against German bunds around GovC meetings. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y, IT-DE 10Y Spread). 

Source: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  

Latest observation: 15 June 2023.")

# Download tab: ----

output$download <- downloadHandler(
  filename = function() {
    paste("monetary_surprises-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(dfInput(), file)
  }
)

}
# Run app: -----

shinyApp(ui, server)