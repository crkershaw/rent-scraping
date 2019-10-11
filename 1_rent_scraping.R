#### Searching for a room in NYC ####
# Spare Room Scraper #

library(rvest)
library(tidyverse)
library(gsubfn)
library(lubridate)
library(googleAuthR)
library(gmapsdistance)
library(tigris)

# Page URL Examples -------------------------------------------------------

# Page 0: "https://www.spareroom.com/roommate/?offset=0&search_id=500028946123&sort_by=age&mode=list"
# Page 2: "https://www.spareroom.com/roommate/?offset=10&search_id=500028946123&sort_by=age&mode=list"
# Page 3: "https://www.spareroom.com/roommate/?offset=20&search_id=500028946123&sort_by=age&mode=list"


# Universal settings ------------------------------------------------------
base_url <- "https://www.spareroom.com/roommate/?"
search_id <- "search_id=500028946123" # Search for NYC, no smoking, max $2000 a month
end <- "&sort_by=age&mode=list"

# Calculating total search results
page_1 <- paste0(base_url,"offset=0","&",search_id,end)
num_results <- page_1 %>% read_html %>% html_nodes("#results_header strong:nth-child(2)") %>% html_text %>% as.numeric()
page_1 %>% read_html %>%
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
  mutate(monthly_min_usd = as.numeric(trimws(gsubfn(".",list("$"="",","=""),monthly_min_usd))),
         monthly_max_usd = as.numeric(ifelse(is.na(monthly_max_usd),
                                             monthly_min_usd,
                                             trimws(gsubfn(".",list("$"="",","=""),monthly_max_usd))
         ))
  )
date <- paste0(year(Sys.Date()),"-",month(Sys.Date()),"-",day(Sys.Date())," ")


# Logging into google -----------------------------------------------------
google_login <- read_lines("C:/Users/charlie-kershaw/Documents/R/google_api_key.txt")
register_google(key = google_login)


# Pulling longitude and latitude ------------------------------------------
output_longlat <- output_formatted %>%
  mutate(postcode_lat = geocode(as.character(postcode))$lat,
         postcode_long = geocode(as.character(postcode))$lon,
         postcode_latlong = paste0(postcode_lat,"+",postcode_long)
         )

output_longlat <- output_longlat %>%
  mutate(postcode_latlong = paste0(lat,"+",long))
# Pulling commute time ----------------------------------------------------
set.api.key(google_login)

# home = c("40.431478+-80.0505401","40.8310224+-73.9095279","40.7043156+-73.9212858")
office = c("40.756950+-73.982033")

# test <- tibble(latlong=home)

f_timedistance <- function(latlong){
  output = gmapsdistance(origin = latlong,
                destination = office,
                key = get.api.key(),
                mode = "transit",
                arr_date = "2019-09-02",
                arr_time = "08:30:00")
}

x <- f_timedistance(test$latlong)
test2 <- test %>%
  mutate(distance = unlist(x$Distance[2]),time = unlist(x$Time[2]))

temp_timedistance <- f_timedistance(output_longlat$postcode_latlong)

temp_time <- as.tibble(temp_timedistance$Time) 
colnames(temp_time) <- c("postcode_latlong","commute_time")
temp_distance <- as.tibble(temp_timedistance$Distance)
colnames(temp_distance) <- c("postcode_latlong","commute_distance")

output_commute <- output_longlat %>%
  left_join(.,temp_time,by="postcode_latlong") %>%
  left_join(.,temp_distance,by="postcode_latlong")

save(output_commute,file=paste0("scrapes/",date,"rent_scrape.Rdata"))
