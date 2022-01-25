#### Searching for a room in NYC ####


# 1. Spare Room Scraper
# This first script scrapes all spare room listings that meet your search criteria
# It then plugs into the google maps API to calculate commute times to the Mercer NYC Office
# It outputs a table of all listings with this extra information as an RData file that is used by script 2


# Page URL Examples:
#   Page 0: "https://www.spareroom.com/roommate/?offset=0&search_id=500028946123&sort_by=age&mode=list"
#   Page 2: "https://www.spareroom.com/roommate/?offset=10&search_id=500028946123&sort_by=age&mode=list"
#   Page 3: "https://www.spareroom.com/roommate/?offset=20&search_id=500028946123&sort_by=age&mode=list"

# Configuration -----------------------------------
commute_date <- "2022-04-18" # Date (YYYY-MM-DD) for commute length calculations - must be in the future, likely a weekday
commute_arr_time = "08:30:00" # Arrival time for commute length calculations
api_key_location <- "api-key/gcloud_api_key.txt"

# Create a search on spare room and copy the search id for the search here 
# This allows you to set preferences like max rent, smoking/non-smoking etc
search_id <- "search_id=500050688210"
results_per_page <- 11 # Number of results displayed per page on Spare Room

# Office location (used as destination for commute)
office = c("40.76812214708413+-73.92476890076448")

# Basic Setup -------------------------------------------------------------
libraries <- c("rvest", "tidyverse", "gsubfn", "lubridate", "googleAuthR", "ggmap", "gmapsdistance", "tigris")

new_packages <- libraries[!(libraries %in% installed.packages()[, "Package"])]
if(length(new_packages)>0){ 
  install.packages(new_packages) 
}

lapply(libraries, library, character.only = TRUE)

custom_functions = list.files("R/")
sapply(custom_functions, source)


# Universal settings ------------------------------------------------------
base_url <- "https://www.spareroom.com/roommate/?"
end <- "&sort_by=age&mode=list"
google_login <- read_lines(api_key_location)

# Scraper -----------------------------------------------------------------

# Calculating total search results
page_1 <- paste0(base_url,"offset=0","&",search_id,end)

num_results <- page_1 %>% 
  read_html() %>% 
  html_nodes("#results_header strong:nth-child(2)") %>% 
  html_text() %>% 
  as.numeric()

results_per_page <- 11

total_pages <- ceiling(num_results / results_per_page)
pages_vector <- seq(1,total_pages,1)

# Function to create table of data from a page
f_pagetable <- function(pageno){
  
  percent_complete <- paste0(round(pageno / total_pages,2)*100,"%  ")
  cat(percent_complete)
  
  offset <- paste0("offset=",(pageno-1)*10) # Calculating offset portion of url
  
  page_url <- paste0(base_url,offset,"&",search_id,end) # Setting page url
  page_code <- read_html(page_url) # Pulling page code
  
  results <- page_code %>% html_nodes(".listing-result")
  
  id <- results %>% html_attr("data-listing-id")
  title <- results %>% html_attr("data-listing-title")
  age <- results %>% html_attr("data-listing-days-old")
  available <- results %>% html_attr("data-listing-available")
  neighbourhood <- results %>% html_attr("data-listing-neighbourhood")
  postcode <- results %>% html_attr("data-listing-postcode")
  type <- results %>% html_attr("data-listing-property-type")
  rooms <- results %>% html_attr("data-listing-rooms-in-property")
  
  monthly_price <- page_code %>% html_nodes(".listingPrice") %>% html_text() %>% 
    .[!. %in% grep("\n",.,value=TRUE)] %>%
    gsub("/month","",.)
  monthly_min <- sapply(strsplit(monthly_price,"-"),`[`,1)
  monthly_max <- sapply(strsplit(monthly_price,"-"),`[`,2)
  
  link <- page_code %>% html_nodes("article") %>% html_node("header") %>% html_nodes("a") %>% html_attr("href")
  link <- paste0("https://www.spareroom.com",link)

  tibble(
    id = as.numeric(id),
    title = trimws(title),
    age = as.numeric(age),
    available = trimws(available),
    neighbourhood = trimws(neighbourhood),
    postcode = as.numeric(postcode),
    type = trimws(type),
    rooms = as.numeric(rooms),
    monthly_price_usd = monthly_price,
    monthly_min_usd = monthly_min,
    monthly_max_usd = monthly_max,
    link = link
  )
}

scrape_output <- map_df(pages_vector,f_pagetable)


output_formatted <- scrape_output %>%
  mutate(monthly_price_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_price_usd))),
         monthly_min_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_min_usd))),
         monthly_max_usd = as.numeric(ifelse(is.na(monthly_max_usd),
                                             monthly_min_usd,
                                             gsub("[^0-9]","",monthly_max_usd)))
         )

f_convertnum <- function(num){
  return(ifelse(num<10, paste0("0",as.character(num)), as.character(num)))
}

date <- paste0(f_convertnum(year(Sys.Date())),"-",f_convertnum(month(Sys.Date())),"-",f_convertnum(day(Sys.Date()))," ")


# Pulling longitude and latitude ------------------------------------------
register_google(key = google_login)

output_longlat <- output_formatted %>%
  mutate(postcode_lat = geocode(as.character(postcode))$lat,
         postcode_long = geocode(as.character(postcode))$lon,
         postcode_latlong = paste0(postcode_lat,"+",postcode_long)
         )


# Pulling commute time ----------------------------------------------------
set.api.key(google_login)

# Example home coordinates
home = c("40.431478+-80.0505401","40.8310224+-73.9095279","40.7043156+-73.9212858")


f_timedistance <- function(latlong){
  output = gmapsdistance(origin = latlong,
                destination = office,
                key = get.api.key(),
                mode = "transit",
                arr_date = commute_date,
                arr_time = commute_arr_time)
}

test_input <- tibble(latlong=home)
test_times <- f_timedistance(test_input$latlong)

test_output <- test_input %>%
  mutate(distance = unlist(test_times$Distance[2]),
         time = unlist(test_times$Time[2])
         )

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

