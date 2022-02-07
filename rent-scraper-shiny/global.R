library(shiny)
library(shinydashboard)
library(shinyjs)
library(plotly)
library(rjson)

source("mod_visualise/table_ui.R")
source("mod_visualise/graphs_ui.R")



rvalues <- reactiveValues()

axis_choices <- list(
  "Cost / Month" = "monthly_price_usd",
  "Mins to Mt Sinai" = "commute_time_Mt-Sinai",
  "Mins to Times Square" = "commute_time_TimesSquare",
  "Mins to KTown" = "commute_time_KTown",
  "Mins to LES" = "commute_time_LES",
  "Commute Score" = "commute_score",
  "Price Score" = "price_score",
  "Final Score" = "score_final"
)

# nyc_map_data <- readRDS("www/nyc_map_data")

# nyc_counties <- fromJSON(file="www/geojson-counties-fips.json")


date_string <- "2022-01-30" 
load(paste0("www/",date_string," rent_scrape.Rdata"))
# load(paste0("../scrapes/",date_string," rent_scrape.Rdata"))

mapbox_key <- readLines("api_key/mapbox_api_key.txt")
Sys.setenv('MAPBOX_TOKEN' = mapbox_key)

weight_commute_mtsinai <- 50
weight_commute_timessquare <- 50
weight_commute_ktown <- 50
weight_commute_les <- 50

weight_commute <- 50
weight_price <- 50

product <- 1
commutes_mintime <- 0
