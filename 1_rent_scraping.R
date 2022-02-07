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
commute_destinations <- list(
  "Mt-Sinai" = c("40.76812214708413+-73.92476890076448"),
  "TimesSquare" = c("40.75799463546395+-73.9855166715542"),
  "KTown" = c("40.74777228293039+-73.98694260600726"),
  "LES" = c("40.71815922302397+-73.99379853211296")
)

commute_date <- "2022-04-18" # Date (YYYY-MM-DD) for commute length calculations - must be in the future, likely a weekday
commute_arr_time = "08:30:00" # Arrival time for commute length calculations

# Google API
api_key_location <- "api-key/gcloud_api_key.txt"

# Spare Room scrape config
# Create a search on spare room and copy the search id for the search here - allows you to set preferences like max rent, smoking/non-smoking etc
search_id <- "search_id=500050688210"
results_per_page <- 11 # Number of results displayed per page on Spare Room




# Basic Setup -------------------------------------------------------------
libraries <- c("rvest", "tidyverse", "gsubfn", "lubridate", "googleAuthR", "ggmap", "gmapsdistance", "tigris", "httr", "RSelenium", "wdman")

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

sr_scrape_output <- f_scrape_spareroom_pages(
  base_url = base_url, 
  search_id = search_id, 
  end = end, 
  pageno = pageno, 
  results_per_page = results_per_page
)

sr_output_formatted <- f_format_spareroom_scrape(sr_scrape_output)


# Streeteasy scraper ------------------------------------------------------

# Running docker
system("docker run -d -p 4444:4444 -p 7900:7900 --shm-size=\"2g\" selenium/standalone-chrome")


# Close any existing selenium session
remDr$close()
cDrv$stop()

# Running scrape
se_scrape_output <- f_scrape_streeteasy_pages(
  remote_driver = remDr,
  base_url = "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:", 
  price_min = 1500, 
  price_max = 3000, 
  areas = "152,146,133,141,120,122,130,101,131,132,401,402", 
  bedrooms = "=1",
  min_wait = 5,
  max_wait = 10)

remDr$close()

se_output_formatted <- f_format_streeteasy_scrape(se_scrape_output)



# Merging tables together -------------------------------------------------

output_formatted <- sr_output_formatted %>%
  full_join(se_output_formatted)


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
output_commute = output_longlat


for(i in c(1:length(commute_destinations))){
  coords = commute_destinations[i]
  name = names(commute_destinations)[i]
  
  print(paste0("Pulling commute times for ", name))
  
  temp_timedistance <- f_timedistance(origin = output_longlat$postcode_latlong, destination = coords)
  
  temp_time <- as_tibble(temp_timedistance$Time) 
  colnames(temp_time) <- c("postcode_latlong",paste0("commute_time_", name))
  
  temp_distance <- as_tibble(temp_timedistance$Distance)
  colnames(temp_distance) <- c("postcode_latlong",paste0("commute_distance_", name))
  
  output_commute <- output_commute %>%
    left_join(.,temp_time,by="postcode_latlong") %>%
    left_join(.,temp_distance,by="postcode_latlong")
}



# Saving file -------------------------------------------------------------
save(output_commute,file=paste0("scrapes/",date,"rent_scrape.Rdata"))

