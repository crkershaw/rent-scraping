# Tables

# Configuration
date_string <- "2022-01-30"   # Enter the date you would like to use the data save name in format YYYY-MM-DD


# Basic Setup -------------------------------------------------------------

libraries <- c("rvest", "tidyverse", "gsubfn", "lubridate", "googleAuthR", "ggmap", "gmapsdistance", "tigris",
               "rjson", "httr", "rgdal", "rgeos", "broom", "tigris", "maptools", "ggrepel", "shadowtext")

new_packages <- libraries[!(libraries %in% installed.packages()[, "Package"])]
if(length(new_packages)>0){ 
  install.packages(new_packages) 
}

lapply(libraries, library, character.only = TRUE)


# Analysis ----------------------------------------------------------------
load(paste0("scrapes/",date_string," rent_scrape.Rdata"))


data_raw <- output_commute

weight_commute_mtsinai <- 20
weight_commute_timessquare <- 20
weight_commute_ktown <- 5
weight_commute_les <- 10

weight_commute <- 70
weight_price <- 30


data_adj <- data_raw %>%
  ungroup() %>%
  mutate(price_score = 1 / (monthly_price_usd / mean(monthly_price_usd))) %>%

  mutate(commute_mtsinai_score = 1 / (log(`commute_time_Mt-Sinai`) / mean(log(`commute_time_Mt-Sinai`))),
         commute_timessquare_score = 1 / (log(commute_time_TimesSquare) / mean(log(commute_time_TimesSquare))),
         commute_ktown_score = 1 / (log(commute_time_KTown) / mean(log(commute_time_KTown))),
         commute_les_score = 1 / (log(commute_time_LES) / mean(log(commute_time_LES))),
         commute_score = (
           commute_mtsinai_score * weight_commute_mtsinai + 
           commute_timessquare_score * weight_commute_timessquare +
           commute_ktown_score * weight_commute_ktown +
           commute_les_score * weight_commute_les
           ) / 
           sum(weight_commute_mtsinai + weight_commute_timessquare + weight_commute_ktown + weight_commute_les)
         ) %>%
           
  mutate(score_final = (
    ((price_score * weight_price) + (commute_score * weight_commute)) / 100
    )) %>%
  arrange(-score_final)

data_adj
