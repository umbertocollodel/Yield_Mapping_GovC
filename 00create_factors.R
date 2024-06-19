###########  The script performes a PCA analysis on the components of
###########  the risk-free yield curve and extracts the relevant factors according
###########  to the loadings on different segments of the yield curve.
###########  It also adds another factor employing PCA on the IT,ES and FR spread
###########  with 10-years bunds. It exports the intermediate df.


# Set parameters: -----

names_list=c("Press Release","Press Conference")


# Performing PCA on OIS yields -----

## Checking the number of eigenvalues
ev <- eigen(cor(Xcon)) # get eigenvalues

fa.parallel(Xcon)  # Use


## Compute factors for both press release and press conference.


Nfacs <- 3  # This is for three factors. You can change this as needed.

fit <- list(Xrel,Xcon) %>% 
  map(~ .x %>% factanal(Nfacs, rotation="equamax",scores = "regression"))

  fit %>% 
    map(~ print(.x,digits=2, cutoff=0.3, sort=TRUE))


# Clean loadings ----

load_df <- fit %>% 
  map(~ round(.x$loadings[1:7,],3)) %>%
  map(~ data.frame(.x)) %>% 
  map(~ .x %>% rownames_to_column(var="Term")) %>% 
  map(~ .x %>% as_tibble()) %>% 
  set_names(names_list)
  
  
 names(load_df[[1]]) <- c("Term","Timing","QE","Path")
 names(load_df[[2]]) <- c("Term","QE","Path","Timing")
 

# Reshape for plotting:

loadings_final_df <- load_df %>%
  bind_rows(.id="event") %>% 
  mutate(Term = factor(Term, levels = c("1M","3M","6M","1Y","2Y","5Y","10YY"))) %>% 
  pivot_longer(matches("Path|QE|Timing"),names_to = "Factor") %>% 
  mutate(Factor = factor(Factor, levels = c("Timing","Path","QE"))) %>% 
  mutate(event= factor(event,levels=c("Press Release","Press Conference"))) 

# Save:

saveRDS(loadings_final_df,"code/app/app_data/app_data_loadings.rds")

 
# Figure: loadings OIS factor -----

  loadings_final_df %>% 
  ggplot(aes(Term,value, group=event, col=event)) +
  geom_line(size=2) +
  facet_wrap(~Factor) +
  labs(title="",
       col="",
       y="",
       x="",
       fill="",
       caption = "**Note**: The model decomposes movements in OIS (1m, 3m, 6m, 1y, 2y, 5y, 10y) into three policy factors: downward-sloping (Timing), hump-shaped (Path) and upward-sloping (QE).  
       It takes three principal components from the variation in yields around GovC meetings (Press Release and Press Conference), rotates and scales them.  
**Source**: Authors' calculation, EA-MPD (Altavilla et al., 2019)   
**Latest observation**: 15 June 2023.") +  
  theme_bw() +
  theme(plot.caption = element_text(hjust=0)) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5)) +
  theme( axis.text = element_text( size = 14 ),
         axis.text.x = element_text( size = 20 ),
         axis.title = element_text( size = 16, face = "bold" ),
         legend.text = element_text(size=14),
         # The new stuff
         strip.text = element_text(size = 20)) +
   theme(plot.caption = element_markdown(hjust = 0,size=12))
 
 
#Export:
 
 ggsave("output/figures/loadings_surprises.png",
        width = 16,
        height= 4,
        dpi = "retina")


# Calculate missing Timing, Path and QE (18th March 2020 and 15th June 2022) ----
 
# Set missing dates:

missing_dates = c("2022-06-15","2020-03-18")



# Create custom function to calculate and add new GovC risk-free factors to
# pre-existing dataframe (or any other special release for that matters)
 
#' add_risk_free
#' @description function to add Timing,Path and QE factor for dates other than
#' initial GoVC meetings. Basically, it applies the weights of the factor
#' decomposition to the new dates. Always use day of interest and next day.
#' @param loadings_df df with loadings
#' @param data_path path to new observations, character.
#' @param missing_dates_data character vector with all dates to calculate change on (day and next day)
#' @param n_dates numeric, number of new dates you interested into
#' @param missing dates character vector only with dates interested (not next day)
#' 
#' @return list with Press Release and Press Conference factor extraction 
#' ready to be added to original df. Transmission left as NA.
#' 
add_risk_free <- function(loadings_df, 
                          data_path,
                          missing_dates_data,
                          n_dates, 
                          missing_dates){
 
  # Loadings dataframe in flat format: 
  
  load_df_format <- load_df %>% 
    map( ~ .x %>% pivot_longer(matches("Timing|Qe|Path"))) %>% 
    map( ~ .x %>% select(-Term)) %>% 
    map(~ .x %>% split(.$name)) %>% 
    modify_depth(2,~ .x %>% pull(value)) %>% 
    flatten()
  
  
  # Calculate day-difference for components of risk-free curve for missin dates:
  
  new_obs <- read_xlsx(data_path,sheet = 2) %>% 
    rename(date = 1) %>%
    mutate(date = convert_to_date(date, character_fun = lubridate::dmy)) %>%
    filter(date %in% missing_dates_data) %>% 
    mutate_at(vars(matches("M|Y")),list(lag= ~ lag(.,1))) %>% 
    slice(2,4,6) %>% 
    mutate(`1M` = -(`1M` - `1M_lag`)*100,
           `3M` = -(`3M` - `3M_lag`)*100,
           `6M` = -(`6M` - `6M_lag`)*100,
           `1Y` = -(`1Y` - `1Y_lag`)*100,
           `2Y` = -(`2Y` - `2Y_lag`)*100,
           `5Y` = -(`5Y` - `5Y_lag`)*100,
           `10Y`= -(`10Y` - `10Y_lag`)*100,
           
    ) %>% 
    select(!contains("lag")) %>%
    rename(`10YY`=`10Y`) %>% 
    replicate(2,.,simplify = F) %>% 
    map( ~ as.matrix(.))
  
  # Name of the new dataframes: 
  
  names(new_obs) <- c("Press Release","Press Conference") 
  
  # Combine with complete df to rescale data, hence calculate factors:
  new_obs_factor <- new_obs %>%
    map2(list(X_rel,X_con), ~ .y %>% rbind(.x)) %>% 
    map(~ .x %>% mutate_at(vars(matches("M|Y")),as.numeric)) %>%
    map(~ .x %>% select(-date)) %>% 
    map(~ scale(.x)) %>%
    rep(each=3) %>%
    map2(load_df_format, ~ .x %*% solve(cor(.x)) %*% .y) %>% 
    map(~ .x %>% as_tibble() %>%  slice_tail(n = n_dates))  %>% 
    map2(rep(c("Path","QE","Timing"),2), ~ .x %>% setNames(.y)) %>% 
    map(~ .x %>% mutate(date = missing_dates))
  
  
  names(new_obs_factor) <- c(rep("Press Release",3),rep("Press Conference",3))
  
  # Reshape like original factor df:
  
  final_new_obs <-  lapply(split(new_obs_factor,names(new_obs_factor)),do.call, what = "cbind") %>% 
    map(~ .x %>% select(!contains("date"))) %>%
    map(~ .x %>% rename_all(~ str_remove(., "Press Release\\."))) %>% 
    map(~ .x %>% rename_all(~ str_remove(., "Press Conference\\."))) %>% 
    map(~ .x %>% mutate(date = missing_dates)) %>% 
    .[c("Press Release","Press Conference")] %>% 
    map(~ .x %>% mutate(Transmission = NA_integer_))
  
  return(final_new_obs)
}
 
 
 new_risk_free <- add_risk_free(load_df,
                                "raw_data/additional_releases.xlsx",
                                c("2020-02-18","2020-02-19",
                                  "2022-06-15","2022-06-16",
                                  "2024-04-11","2024-04-12",
                                  "2024-06-06","2024-06-07"),
                                4,
                                c("2024-06-06",
                                  "2024-04-11",
                                  "2022-06-15",
                                  "2020-03-18")
                                ) 
 

 
 
# Performing PCA on spreads: -----

Nfacs <- 1  
 
 
 fit_spread <- list(spread_rel, spread_con) %>% 
   map(~ .x %>% factanal(Nfacs,scores = "regression"))
 
 fit_spread %>% 
   map(~ print(.x,digits=2, cutoff=0.3, sort=TRUE))
 
# Clean loading spreads and calculate missing Transmission factor of 18th March 2020 (PEPP announcement)  and 15th June 2022----
 
 load_df_spreads <- fit_spread %>% 
   map(~ round(.x$loadings[1:3,],3)) %>%
   map(~ data.frame(.x)) %>% 
   map(~ .x %>% rownames_to_column(var="Country")) %>% 
   map(~ .x %>% as_tibble()) %>% 
   map(~ .x %>% pull(.x))
 
 missing_dates_pep <- c("2020-03-18","2020-03-17",
                        "2022-06-15")
 
 
 new_obs_pepp <- read_xlsx("raw_data/additional_releases.xlsx",sheet = 1) %>% 
   select(matches("10Y|Date...5")) %>%
   setNames(c("date","DE10Y","FR10Y","IT10Y","SP10Y")) %>%
   filter(date == ymd_hms("2022-06-14 17:00:00") | date == ymd_hms("2022-06-15 17:00:00")|
          date == ymd_hms("2020-03-18 17:00:00") | date == ymd_hms("2020-03-19 17:00:00")) %>% 
   mutate_at(vars(contains("10Y")), ~ . - DE10Y) %>% 
   mutate_at(vars(contains("10Y")),list(lag= ~ lag(.,1))) %>% 
   slice(2,4) %>% 
   mutate(FR10Y = (FR10Y - FR10Y_lag)*100,
          SP10Y = (SP10Y - SP10Y_lag)*100,
          IT10Y = (IT10Y - IT10Y_lag)*100) %>% 
   select(IT10Y, FR10Y, SP10Y) %>% 
   replicate(2,.,simplify = F) %>% 
   map2(list(c(-0.06,0.39,-1.37),
             c(-0.63,-1.79,-1.62)), ~ .x %>% rbind(.y)) %>% 
   map( ~ as.matrix(.)) 
   

 
  names(new_obs_pepp) <- c("Press Release","Press Conference") 
  
  pepp_obs <- new_obs_pepp %>% 
  map2(list(spread_rel,spread_con), ~ .y %>% rbind(.x)) %>% 
  map(~ scale(.x)) %>% 
  map2(load_df_spreads, ~ .x %*% solve(cor(.x)) %*% .y) %>% 
  map(~ .x %>% as_tibble() %>%  slice(nrow(.),nrow(.)-1,nrow(.)-2)) %>% 
  map(~ .x %>% rename(Transmission = 1)) %>% 
  map(~ .x %>% mutate(date = c("2023-12-14","2022-06-15","2020-03-18")))
    
   
 
 
 
# Clean scores spreads and add missing (18 March 2020 and 15th June 2022) -----

 
 df_spreads <- fit_spread %>% 
   map(~ .x$scores) %>% 
   map(~ .x %>% as_tibble()) %>% 
   map(~ .x %>% setNames(c("Transmission"))) %>% 
   map2(list(Drel_spread, Dcon_spread),~ .x %>% cbind(date =.y)) %>% 
   map2(pepp_obs,~ .x %>% rbind(.y))
  
 

  
# Monetary surprises: clean the final dataframe ----

# Merge dataframe with scores of yields and spreads

df_surprises <- fit %>% 
  map(~ .x$scores) %>% 
  map2(list(Drel,Dcon), ~ .x %>% cbind(.y)) %>% 
  map(~ .x %>% 
  as_tibble() %>% 
  rename(date = .y)) %>% 
  map2(df_spreads, ~ .x %>% merge(.y,all.y=T)) %>%
  map(~.x %>% as_tibble())
 
 # Change factor names 
 
 names(df_surprises[[1]]) <- c("date","Timing","QE","Path", "Transmission")
 names(df_surprises[[2]]) <- c("date","QE","Path","Timing", "Transmission")
 
 # Name the two datasets 
 
 names(df_surprises) <- c("Press Release","Press Conference") 
 
 
 
 df_surprises <- df_surprises %>% 
   map2(new_risk_free,~ rbind(.x,.y)) %>% 
   map( ~ .x %>% group_by(date)) %>%
   map( ~ .x %>% summarise(across(everything(), ~ .[!is.na(.)][1]))) %>% 
   map( ~ .x %>% ungroup()) %>% 
   map( ~ .x %>% mutate(special = ifelse(date %in% missing_dates,"Special Release","GovC"))) 
   
   
 
 
# Save intermediate data: -------
 
 
df_surprises %>% 
   iwalk(~ saveRDS(.x,file=paste0("intermediate_data/",.y,".rds")))
 
