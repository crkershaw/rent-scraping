#### Searching for a room in NYC ####


# 1. Spare Room Scraper
# This first script scrapes all spare room listings that meet your search criteria
# It then plugs into the google maps API to calculate commute times to your office location
# It outputs a table of all listings with this extra information as an RData file that is used by script 2


# Page URL Examples:
#   Page 0: "https://www.spareroom.com/roommate/?offset=0&search_id=500028946123&sort_by=age&mode=list"
#   Page 2: "https://www.spareroom.com/roommate/?offset=10&search_id=500028946123&sort_by=age&mode=list"
#   Page 3: "https://www.spareroom.com/roommate/?offset=20&search_id=500028946123&sort_by=age&mode=list"

# Configuration -----------------------------------

# Commute details
office = c("40.76812214708413+-73.92476890076448") # Destination for commute
commute_date <- "2022-04-18" # Date (YYYY-MM-DD) for commute length calculations - must be in the future, likely a weekday
commute_arr_time = "08:30:00" # Arrival time for commute length calculations

# Google API
api_key_location <- "api-key/gcloud_api_key.txt"

# Spare Room scrape config
# Create a search on spare room and copy the search id for the search here - allows you to set preferences like max rent, smoking/non-smoking etc
search_id <- "search_id=500050688210"
results_per_page <- 11 # Number of results displayed per page on Spare Room




# Basic Setup -------------------------------------------------------------
libraries <- c("rvest", "tidyverse", "gsubfn", "lubridate", "googleAuthR", "ggmap", "gmapsdistance", "tigris", "httr", "RSelenium")

new_packages <- libraries[!(libraries %in% installed.packages()[, "Package"])]
if(length(new_packages)>0){ 
  install.packages(new_packages) 
}

lapply(libraries, library, character.only = TRUE)

custom_functions = list.files("R/")
sapply(paste0("R/",custom_functions), source)


# Universal settings ------------------------------------------------------
base_url <- "https://www.spareroom.com/roommate/?"
end <- "&sort_by=age&mode=list"

date <- paste0(
  f_add_leading_0s(year(Sys.Date())),"-",
  f_add_leading_0s(month(Sys.Date())),"-",
  f_add_leading_0s(day(Sys.Date()))," "
  )

google_login <- read_lines(api_key_location)
register_google(key = google_login) # Used for geocode
set.api.key(google_login) # Used for gmapdistance

# Spare Room Scraper -----------------------------------------------------------------

# Calculating total search results

scrape_output <- f_scrape_spareroom_pages(
  base_url = base_url, 
  search_id = search_id, 
  end = end, 
  pageno = pageno, 
  results_per_page = results_per_page
)

output_formatted <- f_format_spareroom_scrape(scrape_output)



# Streeteasy scraper ------------------------------------------------------


# Close any existing selenium session
remDr$close()
cDrv$stop()

# Set chrome driver - allows us to use custom options necessary for masking we're a bot
cDrv <- chrome(
  port=4444L, 
  version="97.0.4692.71",
  verbose=TRUE
)

# Setting custom chrome options
eCaps <- list(
  chromeOptions = 
    list(args = 
           c(
             "--disable-dev-shm-usage", 
             # "--start-maximized",
             "disable-blink-features=AutomationControlled", # Hides we're a bot
             "--whitelisted-ips"
           )
         # prefs = list(
         #  # "download.default_directory" = "C:/XXX/YYY"
         # )
    )
)

# Starting browser
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "chrome",
  extraCapabilities=eCaps
)

remDr$open()

# Running scrape
se_scrape_output <- f_scrape_streeteasy_pages(
  remote_driver = remDr,
  base_url = "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:", 
  price_min = 1500, 
  price_max = 3000, 
  areas = "152,146,133,141,120,122,130,101,131,132,401,402", 
  bedrooms = "=1")

remDr$close()

se_output_formatted <- f_format_streeteasy_scrape(se_scrape_output)


# Pulling longitude and latitude ------------------------------------------

f_extract_postcode <- function(revgeo_output){
  
  for(i in revgeo_output$results[[1]]$address_components){
    if(i[["types"]][[1]] == "postal_code"){
      return(as.numeric(i[["long_name"]]))
    }
  }
}

output_longlat <- se_output_formatted %>%
  rowwise() %>%
  mutate(postcode_lat = ifelse(is.na(coordinates), geocode(as.character(postcode))$lat, as.numeric(trimws(str_split(coordinates, "\\+")[[1]][1]))),
         postcode_long = ifelse(is.na(coordinates), geocode(as.character(postcode))$lon, as.numeric(trimws(str_split(coordinates, "\\+")[[1]][2]))),
         postcode_latlong = paste0(postcode_lat,"+",postcode_long), # Note: assumes positive latitude
         postcode = ifelse(is.na(postcode), f_extract_postcode(revgeocode(c(postcode_long, postcode_lat), "all")), postcode)
         )


# Pulling commute time ----------------------------------------------------

# Scraped data commute time pull
temp_timedistance <- f_timedistance(output_longlat$postcode_latlong)

temp_time <- as_tibble(temp_timedistance$Time) 
colnames(temp_time) <- c("postcode_latlong","commute_time")

temp_distance <- as_tibble(temp_timedistance$Distance)
colnames(temp_distance) <- c("postcode_latlong","commute_distance")

output_commute <- output_longlat %>%
  left_join(.,temp_time,by="postcode_latlong") %>%
  left_join(.,temp_distance,by="postcode_latlong")


# Saving file -------------------------------------------------------------
save(output_commute,file=paste0("scrapes/",date,"rent_scrape.Rdata"))

