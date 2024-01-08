################################################################################
## This file is part of the replication suite  of
## "Measuring Euro Area Monetary Policy"
## By Carlo Altavilla, Luca Brugnolini, Refet S. Gürkaynak, Roberto Motto, and
## Giuseppe Ragusa
##
## This file produces the data used for factor extraction and rotation
##
################################################################################



##---------------------- Hyperparameters  -------------------------------------
  start_from <- "2001-12-31"
  last_dates <- "2023-06-15"
  
  if (!require("pacman", quietly = TRUE))
    install.packages("pacman", repos = "cran.rstudio.com")
  
  ##---------------------- Load Packages  ---------------------------------------
  pacman::p_load(tidyverse, 
                 readxl, 
                 grid, 
                 gridExtra, 
                 sandwich, 
                 zoo,
                 lubridate, 
                 stargazer, 
                 lme4, 
                 stringr, 
                 reshape2, 
                 forcats,
                 lmtest,
                 rprojroot,
                 rstudioapi,
                 Hmisc, 
                 psych,
                 GPArotation,
                 forcats,
                 broom,
                 ggtext,
                 shiny,
                 shinydashboard,
                 leaflet,
                 plotly,
                 install = TRUE, update = F)
  
  ##---------------------- Setup path  ------------------------------------------
  
  base_path <- getwd()
  
  
  ##-----------------------------------------------------------------------------
  ## DON'T CHANGE THIS
  
  r_path <- paste0(base_path, "/code/rcode/")
  s_path <- paste0(base_path, "/raw_data/")
  d_path <- paste0(base_path, "/data/")
  

  ##--------------------- Helper function  --------------------------------------
 # Takes a dataframe, removes some dates
  
  select_eampd_assets <- function(x,date){
    x  %>% 
      as.tbl %>% filter(!(date %in% exclude_date))
  }
  
  ## ----------- Date Exclusions ------------------------------------------------
  exclude_date <- as_date(c("2001-09-17",  ## September eleven
                            "2008-10-08",  ## Rate cut 50% all banks (outlier)
                            "2008-11-06"   ## ADD DESCRIPTION ##
  ))
  
  ## ----------- Load raw data -------------------------------------------------
  
  # Press Release, Conference and whole monetary statement
  eampd_r <- read_xlsx(paste0(s_path,"00EA_MPD_update_july2023.xlsx"), sheet = 2) %>%
    mutate(Date = as.Date(date))
  eampd_c <- read_xlsx(paste0(s_path,"00EA_MPD_update_july2023.xlsx"), sheet = 3) %>%
    mutate(Date = as.Date(date))
  eampd_m <- read_xlsx(paste0(s_path,"00EA_MPD_update_july2023.xlsx"), sheet = 4) %>%
    mutate(Date = as.Date(date))
  
  # OIS
  rois_rel  <- eampd_r %>% select("date", starts_with("OIS")) 
  rois_con  <- eampd_c %>% select("date", starts_with("OIS")) 
  rois_tot  <- eampd_m %>% select("date", starts_with("OIS")) 
  
  # Bund
  rbund_rel <- eampd_r %>% select("date", starts_with("DE")) 
  rbund_con <- eampd_c %>% select("date", starts_with("DE"))
  rbund_tot <- eampd_m %>% select("date", starts_with("DE"))
  
  # Bond
  rbond_rel <- eampd_r %>% select("date", starts_with("IT"),starts_with("FR"),starts_with("ES"))
  rbond_con <- eampd_c %>% select("date", starts_with("IT"),starts_with("FR"),starts_with("ES"))
  rbond_tot <- eampd_m %>% select("date", starts_with("IT"),starts_with("FR"),starts_with("ES"))
  
  ## ---- Manual cleaning ---  
  ## 2-Feb-2000 in ois release, 200bp change in some assets 
  #rois_rel[rois_rel$Date == as.Date("2000-02-17"),] c(4,5, 6, 7, 12, 13)] <- 0 
  ## ADDING 12 ##
  #rois_tot[rois_rel$Date == as.Date("2000-02-17"), c(4,5, 6, 7, 12, 13)] <- 0 
  ## ADDING 12 ##
  
  ## ---- select only median market-based surprises and clean dates  ------------
  
  ois_rel <- select_eampd_assets(rois_rel,exclude_date)
  ois_con <- select_eampd_assets(rois_con,exclude_date)
  ois_tot <- select_eampd_assets(rois_tot,exclude_date)
  
  bond_rel <- select_eampd_assets(rbond_rel,exclude_date)
  bond_con <- select_eampd_assets(rbond_con,exclude_date)
  bond_tot <- select_eampd_assets(rbond_tot,exclude_date)
  
  bund_rel <- select_eampd_assets(rbund_rel,exclude_date)
  bund_con <- select_eampd_assets(rbund_con,exclude_date)
  bund_tot <- select_eampd_assets(rbund_tot,exclude_date)
  
  ## ---- Variables for factors extractions ------------------------------------
  # This part of the script produces a dataframe with OIS10Y after 2011, and DEU10Y
  # before that - data availability
  # Both for press release and conference
  
  #  ---- Release --------------------------------------------------------------
  Zrel <- ois_rel %>% 
    select(date,  OIS_1M, OIS_3M, OIS_6M, OIS_1Y, OIS_2Y)
  
  OIS <- ois_rel %>%
    filter(date >= as.Date("2011-08-04")) %>% 
    select(date, OIS_5Y, OIS_10Y)
  
  DEU <- bund_rel %>%
    filter(date < as.Date("2011-08-04")) %>% 
    select(date, DE5Y, DE10Y)
  
  # Create spread dataframe
  
  spread_rel <- bond_rel %>% 
    merge(bund_rel %>% select(date,DE10Y)) %>% 
    filter(complete.cases(.)) %>% 
    select_at(vars(ends_with("10Y"))) %>%
    mutate_all(~ . - DE10Y) %>% 
    select(-DE10Y) %>% 
    as.matrix()
  
  names(Zrel) <- sub("OIS_", "", names(Zrel))
  names(OIS) <- sub("OIS_", "", names(OIS))
  names(DEU) <- sub("DE", "", names(DEU))
  names(DEU) <- sub("10", "10Y", names(DEU))
  names(OIS) <- sub("10", "10Y", names(OIS))
  
  Yrel <- bind_rows(DEU, OIS) 
  X_rel <- inner_join(Zrel, Yrel, by = "date") %>% na.omit
  rm(Zrel, OIS, DEU, Yrel)
  
  #  ---- Conference ------------------------------------------------------------
  
  Zcon <- ois_con %>% select(date, OIS_1M, OIS_3M, OIS_6M, OIS_1Y, OIS_2Y) 

  OIS <- ois_con %>% filter(date >= as.Date("2011-08-04")) %>% 
    select(date, OIS_5Y, OIS_10Y)
  DEU <- bund_con %>% filter(date < as.Date("2011-08-04")) %>% 
    select(date, DE5Y, DE10Y)

  # Create spread dataframe
    
  spread_con <- bond_con %>% 
  merge(bund_con %>% select(date,DE10Y)) %>% 
  filter(complete.cases(.)) %>% 
  select_at(vars(ends_with("10Y"))) %>%
  mutate_all(~ . - DE10Y) %>% 
  select(-DE10Y) %>% 
  as.matrix()
    
  
  names(Zcon) <- sub("OIS_", "", names(Zcon))
  names(OIS) <- sub("OIS_", "", names(OIS))
  names(DEU) <- sub("DE", "", names(DEU))
  names(DEU)<- sub("10", "10Y", names(DEU))
  names(OIS)<- sub("10", "10Y", names(OIS))
  
  Ycon <- bind_rows(DEU, OIS)
  X_con <- inner_join(Zcon, Ycon , by = "date")  %>% na.omit
  rm(Zcon, OIS, DEU, Ycon)
  

  ## ---- Decide how many asset to keep for extracting factors ------------------
  
  Xrel <- X_rel %>%
    filter(date >= start_from) %>%
    select(-date) %>%
    as.matrix()
  
  Drel <- X_rel %>% 
    filter(date >= start_from) %>% 
    pull(date) %>% 
    as.character()
  
  Xcon <- X_con %>%
    filter(year(date) >= start_from) %>%
    select(-date) %>%
    as.matrix()
  
  Dcon <- X_con %>% 
    filter(date >= start_from) %>% 
    pull(date) %>% 
    as.character()
  
  
  Drel_spread <- bond_rel %>% 
    merge(bund_rel %>% select(date,DE10Y)) %>% 
    filter(complete.cases(.)) %>% 
    pull(date) %>% 
    as.character()
  
  
  Dcon_spread <- bond_con %>% 
    merge(bund_con %>% select(date,DE10Y)) %>% 
    filter(complete.cases(.)) %>% 
    pull(date) %>% 
    as.character()
    
  
  
  write.table(
    Xrel,
    paste0(d_path, "Xrel.csv"),
    sep = ",",
    col.names = F,
    row.names = F
  )
  write.table(
    Xcon,
    paste0(d_path, "Xcon.csv"),
    sep = ",",
    col.names = F,
    row.names = F
  )
  write.table(
    Drel,
    paste0(d_path, "Drel.csv"),
    sep = ",",
    col.names = F,
    row.names = F
  )
  write.table(
    Dcon,
    paste0(d_path, "Dcon.csv"),
    sep = ",",
    col.names = F,
    row.names = F
  )
  write.table(
    Dspread,
    paste0(d_path, "Dspread.csv"),
    sep = ",",
    col.names = F,
    row.names = F
  )
  
  

    