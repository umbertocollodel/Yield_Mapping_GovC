######## The script produces and saves all plots related to th factor 
######## decomposition.



# Load final datasets (Press Release and Press Conference): -------

df_surprises <- names_list %>% 
  map(~read_rds(paste0("intermediate_data/",.x,".rds"))) %>% 
  set_names(names_list)


# Figure: Monetary surprises over time: ----

# Create df with whole monetary statement

ms_df <- df_surprises %>%
  bind_rows(.id = "id") %>%
  filter(!date %in% c("2020-03-18","2022-06-15")) %>% 
  mutate_at(vars(matches("Path|Timing|QE|Transmission")),as.numeric) %>% 
  mutate_at(vars(matches("Path|Timing|QE|Transmission")), ~ ifelse(id=="Press Conference" & date %in% missing_dates,0,.x)) %>% 
  group_by(date) %>% 
  summarise_at(vars(matches("Path|Timing|QE|Transmission")), sum) %>% 
  mutate(id = "Monetary Statement") %>% 
  select(id,everything()) %>% 
  mutate(special = ifelse(date %in% missing_dates,"Special Release","GovC"))



# Clean for time serie plotting

time_serie_df <- df_surprises %>%
  bind_rows(.id = "id") %>%
  mutate_at(vars(matches("Path|Timing|QE|Transmission")),as.numeric) %>%
  mutate_at(vars(matches("Path|Timing|QE|Transmission")), ~ ifelse(id=="Press Conference" & date %in% missing_dates,0,.x)) %>% 
  rbind(ms_df) %>% 
  pivot_longer(matches("Path|Timing|QE|Transmission"),names_to = "Factor") %>% 
  mutate(Factor = factor(Factor, levels = c("Timing","Path","QE","Transmission"))) %>% 
  mutate(id = factor(id,levels= c("Press Release","Press Conference","Monetary Statement"))) 


# Save the dataset:

saveRDS(time_serie_df,"code/app/app_data/app_data.rds")

# Plot:


time_serie_df %>% 
  filter(year(date) >= 2020) %>% 
  mutate(value=ifelse(id=="Press Conference" & date == "2020-03-18",0,value)) %>% 
  mutate(value=ifelse(id=="Press Conference" & date == "2022-06-15",0,value)) %>% 
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
  facet_wrap(~ id) +
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

# Export: 

ggsave("output/figures/monetary_surprises_20_23.png",
       width = 19,
       height = 7,
       dpi="retina")



# Figure: Transmission surprises over time: ------


# Plot:


time_serie_df %>%
  filter(Factor == "Transmission") %>%
  mutate(value=ifelse(id=="Press Conference" & date == "2020-03-18",0,value)) %>% 
  mutate(value=ifelse(id=="Press Conference" & date == "2022-06-15",0,value)) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(date,value)) +
  geom_col(width = 0.5,fill="#FE7A15") +
  labs(title="",
       y="Standard Deviation",
       x="",
       fill="",
       caption = "**Note**: The model decomposes movements in 10 years spreads against German bunds (IT-DE, SP-DE and FR-DE) into one policy factor.  
It takes one principal component from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and   
scales them. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (IT-DE 10Y Spread).   
**Source**: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  
**Latest observation**: 15 June 2023.") +
  facet_wrap(~ id) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme( axis.text = element_text( size = 14 ),
         axis.title = element_text( size = 16 ),
         legend.position="none",
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))


# Export: 

ggsave("output/figures/transmission_surprises_2000_2023.png",
       width = 17,
       height = 7,
       dpi="retina")





# Figure: Monetary Surprises for PEPP related policy announcements: ------

# Relevant dates

pepp_dates=c("2020-03-12","2020-03-18","2020-06-04","2020-12-10","2021-12-16",
             "2022-06-15","2023-12-14")

#Plot:

pepp_surprises_plots <- time_serie_df %>% 
  filter(date %in% pepp_dates) %>%
  mutate(value=ifelse(id=="Press Conference" & date == "2020-03-18",0,value)) %>% 
  mutate(value=ifelse(id=="Press Conference" & date == "2022-06-15",0,value)) %>% 
  split(.$id) %>% 
  map(~ .x %>%
        ggplot(aes(date,value, fill=Factor)) +
        geom_col(width = 0.5) +
        labs(title="",
             y="Standard Deviation",x="",
             fill="",
             caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE). It takes three principal components  
from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them. An additional model employs the same methodology for the variation of 10 years spreads (IT-DE, SP-DE   
and FR-DE) against German bunds around GovC meetings. The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y, IT-DE 10Y Spread).  
**Source**: Authors' calculation, Bloomberg, EA-MPD (Altavilla et al., 2019)  
**Latest observation**: 15 June 2023.") +  
        theme_bw() +
        facet_wrap(~ Factor,
                   nrow = 1) +
        theme(plot.caption = element_text(hjust=0)) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_manual(values=c("#35BBCA","#0191B4","#D3DD18","#FE7A15")) +
        theme( axis.text = element_text( size = 14 ),
               axis.text.x = element_text( size = 20 ),
               axis.title = element_text( size = 16),
               legend.position="bottom",
               legend.text = element_text(size=14),
               strip.text = element_text(size = 20)) +
        theme(plot.caption = element_markdown(hjust = 0,size=12))
  )
#Export:

pepp_surprises_plots %>% 
  iwalk(~ ggsave(paste0("output/figures/pepp_surprises_2020_2023_",.y,".png"),
                  .x,
       width = 17,
       height = 7,
       dpi="retina"))


# Figure: Single out on the transmission factor (not used for the moment) ----

time_serie_df %>% 
  filter(date %in% pepp_dates) %>% 
  filter(Factor == "Transmission") %>% 
  ggplot(aes(date,value, fill=Factor)) +
  geom_col(aes(alpha=id), width = 0.5, position = "dodge") +
  labs(title="Monetary Surprises to ECB Statements involving PEPP - Transmission during Press Release and Conference?",
       y="SD (Ref. Asset)",x="",fill="",alpha= "",caption = "Note: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) 
  and upward-sloping (QE). It takes three principal components from the variation in yields around GovC meetings, rotates and scales them. 
  The movements can be interpreted as similar to a 1 standard deviation movement in the reference asset for that factor (OIS 1m, OIS 1y, OIS 10y). 
  Latest observation: 15 June 2023.
") +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_fill_manual(values=c("#FE7A15")) +
  scale_alpha_manual(values = c(1,0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.position="bottom",
         # The new stuf
         strip.text = element_text(size = 20))





# Figure: Net of PEPP Components - QE and flexibility -----

time_serie_df %>% 
  filter(date %in% pepp_dates) %>%
  mutate(value=ifelse(id=="Press Conference" & date == "2020-03-18",0,value)) %>% 
  mutate(value=ifelse(id=="Press Conference" & date == "2022-06-15",0,value)) %>% 
  filter(date == "2021-12-16"| date == "2022-06-15") %>% 
  filter(Factor == "Transmission") %>% 
  group_by(date) %>% 
  reframe(total = sum(value), Factor) %>% 
  distinct() %>% 
  group_by(Factor) %>% 
  mutate(net = sum(total)) %>% 
  mutate(date = ifelse(date == "2021-12-16","PEPP QE Component","PEPP Flexibility Component")) %>% 
  mutate(Factor = "Total") %>% 
  ggplot(aes(Factor,total, fill=date)) +
  geom_col(width= 0.1,position = "stack") +
  geom_point(aes(y=net, fill=Factor), size=5, shape=18,col="#D3DD18") +
  labs(title="",
       y="Standard Deviation",x="",
       fill="",
       caption = "**Note**: The PEPP QE Component corresponds to the net of the Transmission factor
       identified for the ECB Press    
       Release and Press Conference of the 16th December 2021. The PEPP Flexibility Component corresponds to    
       the Transmission factor of the ad-hoc Press Release of the
       15th June 2022. The total is the difference between  
       the two.   
**Source**: Authors' calculations.") +  
  theme_bw() +
  ylim(-4,2) +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 1, hjust=0.5)) +
  scale_fill_manual(values=c("#FE7A15","#0191B4","#D3DD18")) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_blank(),
         axis.title = element_text( size = 16),
         legend.position="bottom",
         legend.text = element_text(size=14),
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12)) +
  guides(fill=guide_legend(override.aes=list(shape=NA)))


# Export: 

ggsave("output/figures/pepp_components.png",
       width = 14,
       height = 7,
       dpi="retina")


# Figure: Persistence of Transmission related shocks with LP on Sovereign Stress Index:  ----


# Countries of interest:

names=c("IT","SP","FR","GB","DE","U2")

# Clean the SovCISS index: 

sov_ciss <- read.csv("raw_data/SovCISS_complete.csv") %>% 
  as_tibble() %>% 
  select(1,6,8,10,11,15,20) %>% 
  rename_at(vars(starts_with("Composite")),~ str_extract(.,"ES|FR|IT|GB|U2|DE")) %>% 
  rename(date=DATE) %>% 
  mutate(date = format(as.Date(date),"%Y-%m"))

# Merge SovCiss data and transmission surprises


endogenous <- spread_df %>% 
  mutate(date = format(as.Date(date),"%Y-%m")) %>% 
  merge(sov_ciss) %>% 
  as_tibble() %>% 
  select(-date) %>% 
  mutate_all(as.numeric) 

# Create Local Projections formulae: 

formula=c("IT","ES","FR","DE","GB","U2") %>% 
  map(~ paste0(.x, " ~ Transmission + dplyr::lag(Transmission,1) + dplyr::lag(Transmission,2) + dplyr::lag(Transmission,3) + dplyr::lag(Transmission,4) + dplyr::lag(Transmission,5) + dplyr::lag(Transmission,6) + dplyr::lag(Transmission,7) + dplyr::lag(Transmission,8) + dplyr::lag(Transmission,9) + dplyr::lag(Transmission,10) + dplyr::lag(Transmission,11) + dplyr::lag(Transmission,12)"))




# Model and plot:

formula %>%
  map(~ lm(.x, endogenous)) %>% 
  map(~ .x %>% summary()) %>% 
  map(~ .x %>% tidy(conf.int=T, conf.level=0.68)) %>% 
  map(~ .x %>% slice(2:nrow(.))) %>% 
  set_names(names) %>% 
  bind_rows(.id = "Country") %>% 
  mutate(term = str_extract(term,"\\d+")) %>%
  mutate(term = ifelse(is.na(term),0,term)) %>% 
  mutate(term = fct_reorder(term, as.integer(term))) %>%
  mutate(Country = case_when(Country == "U2" ~ "Euro Area",
                             T ~ Country)) %>% 
  mutate(Country = factor(Country,levels = c("IT","SP","FR","DE","GB","Euro Area"))) %>% 
  ggplot(aes(estimate,group=1)) +
  geom_line(aes(term,estimate,group=1), col ="#FE7A15", size=1.5) +
  geom_ribbon(aes(ymin=`conf.low`,ymax=`conf.high`,x=term,group=1),fill ="grey", alpha=0.3) +
  geom_rect(data=. %>% filter(Country=="Euro Area"),aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fill='red', alpha=0.006) +
  geom_rect(data=. %>% filter(Country=="GB" | Country=="DE"),aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fill='blue', alpha=0.006) +
  labs(title="",
       y="",x="",fill="",
       caption = "**Note**: Impulse response functions to a unit Transmission shock. The factors are extracted around ECB Monetary Press Releases  
       only. Shaded areas correspond to 68% confidence bands. Euro area aggregate calculted with real GDP weights. Blue areas highlight countries  
       for which there is no significant response over the year. Red area is for the euro area aggregate.  
       **Sample**: 2002-01/2023-06.  
       **Source**: Authors' own calculations.
") +
  geom_hline(yintercept = 0, size=0.5, linetype="dashed") +
  theme_minimal() +
  scale_x_discrete(breaks=c(0,3,6,9,12))+
  scale_y_discrete(breaks=c(0.05,0,-0.05))+
  ylim(-0.04,0.08) +
  facet_wrap(~ Country,
             nrow = 2) +
  theme(plot.caption = element_text(hjust=0)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 16),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.position="bottom",
         # The new stuf
         strip.text = element_text(size = 14)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))





# Export:

ggsave(filename = "output/figures/irfs_sovereign_stress.png",
       bg="white",
       width = 14,
       height = 7,
       dpi="retina")


# Model and plot:

formula %>%
  map(~ lm(.x, endogenous)) %>% 
  map(~ .x %>% summary()) %>% 
  map(~ .x %>% tidy(conf.int=T, conf.level=0.68)) %>% 
  map(~ .x %>% slice(2:nrow(.))) %>% 
  set_names(names) %>% 
  bind_rows(.id = "Country") %>% 
  mutate(term = str_extract(term,"\\d+")) %>%
  mutate(term = ifelse(is.na(term),0,term)) %>% 
  mutate(term = fct_reorder(term, as.integer(term))) %>%
  mutate(Country = case_when(Country == "U2" ~ "Euro Area",
                             T ~ Country)) %>% 
  mutate(Country = factor(Country,levels = c("IT","SP","FR","DE","GB","Euro Area"))) %>% 
  group_by(Country) %>% 
  mutate_at(vars("estimate","conf.low","conf.high"),cumsum) %>% 
  ggplot(aes(estimate,group=1)) +
  geom_line(aes(term,estimate,group=1), col ="#FE7A15", size=1.5) +
  geom_ribbon(aes(ymin=`conf.low`,ymax=`conf.high`,x=term,group=1),fill ="#FE7A15", alpha=0.2) +
  geom_rect(data=. %>% filter(Country=="Euro Area"),aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fill='red', alpha=0.006) +
  geom_rect(data=. %>% filter(Country=="GB" | Country =="DE"),aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), fill='blue', alpha=0.006) +
  labs(title="SovCISS Response to Transmission Surprise",
       y="",x="",fill="",caption = "Note: Cumulative impulse response functions to a unit Transmission shock. The factors are extracted around ECB Monetary Press Releases only. Shaded areas correspond to
68% confidence bands. Euro area aggregate calculted with real GDP weights. \nSample: 2002-01/2023-06.
") +
  geom_hline(yintercept = 0, size=0.5, linetype="dashed") +
  theme_minimal() +
  scale_x_discrete(breaks=c(0,3,6,9,12))+
  facet_wrap(~ Country,
             nrow = 2) +
  theme(plot.caption = element_text(hjust=0)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 16),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.position="bottom",
         # The new stuf
         strip.text = element_text(size = 14))

# Export:

ggsave(filename = "output/figures/cumulative_irfs_sovereign_stress.pdf",
       width = 10,
       height = 6)









sov_ciss %>% 
  summary()


# Figure: Spreads since 2020 ------

# Calculate spreads. divide by tenor and average hourly observations over the day 


spreads_move_df <- read_xlsx("raw_data/additional_releases.xlsx") %>% 
  select(`Date...1`,matches("Italy|Spain|France|Germany")) %>% 
  rename(date = 1) %>% 
  pivot_longer(cols = matches("yr"), names_to = "country") %>% 
  separate(country,c("country","tenor"),sep = "\\s") %>% 
  mutate(day = as.Date(date)) %>% 
  group_by(day,country,tenor) %>%
  reframe(value = mean(value)) %>% 
  pivot_wider(names_from = "country",values_from = "value") %>% 
  mutate_at(vars(matches("Italy|Spain|France")), ~ . - Germany) %>% 
  pivot_longer(cols = matches("Italy|Spain|France|Germany"), names_to = "country") 
  
  

# Plot:

spreads_move_df %>% 
  filter(tenor == "10yr" & country != "Germany") %>% 
  filter(day <= "2023-06-15") %>%
  mutate(tenor = ifelse(tenor == "10yr","10Y Spreads vis-a-vis German Bunds",tenor)) %>% 
  ggplot(aes(day,value*100,col = country, group = country)) +
  geom_line() +
  geom_vline(xintercept = as.Date(pepp_dates),linetype="dashed") +
  labs(title="",
       y="Bps",
       x="",
       col ="",
       caption = "**Note**: Vertical dashed lines indicate dates relative to PEPP announcements.     
**Source**: Bloomberg   
**Latest observation**: 15 June 2023.") +  
  facet_wrap(~ tenor) +
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  scale_color_manual(values=c("#35BBCA","#0191B4","#D3DD18","#FE7A15")) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16 ),
         legend.position="bottom",
         legend.text = element_text(size=14),
         strip.text = element_text(size = 20)) +
  theme(plot.caption = element_markdown(hjust = 0,size=12))

# Export:

ggsave(filename = "output/figures/spreads_overview.png",
       bg="white",
       width = 14,
       height = 7,
       dpi="retina")
