

# https://streeteasy.com/1-bedroom-apartments-for-rent/nyc/status:open%7Cprice:1500-3000%7Carea:152,146,133,141,120,122,130,101,131,132,401,402
# https://streeteasy.com/for-rent/nyc/status:open%7Cprice:1500-3000%7Carea:152,146,133,141,120,122,130,101,131,132,401,402%7Cbeds%3C=1


# bedrooms <- "1-bedroom-apartments-for"
# studio <- "studios"
# "-for-rent"

# custom_headers <- c(
#   "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
#   "accept-encoding" = "gzip, deflate, br",
#   "accept-language" = "en-GB,en;q=0.9",
#   "sec-ch-ua" = "\"Not;A Brand\";v=\"99\", \"Google Chrome\";v=\"97\", \"Chromium\";v=\"97\"",
#   "sec-ch-ua-mobile" = "?0",
#   "sec-ch-ua-platform" = "Windows",
#   "sec-fetch-dest" = "document",
#   "sec-fetch-mode" = "navigate",
#   "sec-fetch-site" = "none",
#   "sec-fetch-user" = "?1",
#   "upgrade-insecure-requests" = 1
#   # "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.99 Safari/537.36)"
# )


# docker run -d -p 4444:4444 -p 7900:7900 --shm-size="2g" selenium/standalone-chrome


f_scrape_streeteasy_page <- function(page_code){

  results <- page_code %>% html_nodes(".listingCard")
  
  id <- page_code %>% html_nodes(".listingCard-globalLink") %>% html_attr("aria-labelledby") %>% gsub("info-", "", .)
  header <- page_code %>% html_elements("p.listingCardLabel.listingCard-upperShortLabel") %>% html_text() %>% trimws() %>% 
    .[!(. %in% c("Sponsored", "New Development"))]
  
  title <- header
  age <- NA
  available <- NA
  neighbourhood <- title %>% gsub("^.* in ","",.) %>% gsub("\n", "", .) %>% trimws()
  postcode <- NA
  type <- title %>% gsub(" in .*$", "", .)
  rooms <- NA
  monthly_price_usd = page_code %>% html_elements(".price") %>% html_text()
  link <- page_code %>% html_nodes(".listingCard-globalLink") %>% html_attr("href")
  address <- page_code %>% html_elements("a.listingCard-link") %>% html_text()
  coordinates <- page_code %>% html_nodes(".listingCard-globalLink") %>% html_attr("se:map:point")

  # print(results)
  # print(id)
  # print(title)
  # print(age)
  # print(available)
  # print(neighbourhood)
  # print(postcode)
  # print(type)
  # print(rooms)
  # print(monthly_price_usd)
  # print(link)
  # print(address)
  # print(coordinates)
  
  output_table <- tibble(
    id = as.numeric(id),
    title = trimws(title),
    age = as.numeric(age),
    available = trimws(available),
    neighbourhood = trimws(neighbourhood),
    address = address,
    postcode = as.numeric(postcode),
    coordinates = coordinates,
    type = trimws(type),
    rooms = as.numeric(rooms),
    monthly_price_usd = monthly_price_usd,
    monthly_min_usd = monthly_price_usd,
    monthly_max_usd = monthly_price_usd,
    link = link
  )
  
  # print(output_table)
  # Sys.sleep(runif(1,10,15))
  
  return(output_table)

}


# areas <- "152,146,133,141,120,122,130,101,131,132,401,402"
# base_url <- "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:"
# 
# price_min <- 1500
# price_max <- 3000
# bedrooms <- "=1"

# f_scrape_streeteasy_page(
#   base_url = "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:", 
#   price_min = 1500, 
#   price_max = 3000, 
#   areas = "152,146,133,141,120,122,130,101,131,132,401,402", 
#   bedrooms = "=1", 
#   pageno = 5)


f_scrape_streeteasy_page_sel <- function(remote_driver, base_url, price_min, price_max, areas, bedrooms, pageno){
  
  page_url <- paste0(base_url,price_min,"-",price_max,"%7Carea:",areas,"%7Cbeds%3C",bedrooms,"?page=",pageno) # Setting page url
  print(paste0("Url: ", page_url))
  
  remote_driver$navigate(page_url)
  remote_driver$screenshot(display=TRUE)
  source <- remote_driver$getPageSource() %>%
    .[[1]] %>%
    read_html()
  
  return(f_scrape_streeteasy_page(source))
  
}


f_spinup <- function(){
  
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
               "--start-maximized",
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
  
  return(remDr)
}

f_scrape_streeteasy_pages <- function(remote_driver, base_url, price_min, price_max, areas, bedrooms, results_per_page, min_wait, max_wait){
  
  # uastring <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/97.0.4692.71 Safari/537.36"
  # session <- session("https://streeteasy.com/for-rent/nyc", user_agent(uastring))
  # session$response$request$options$useragent
  print("Spinning up initial server")
  remDr <- f_spinup()
  remDr$open()
  
  # page_1 <- "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:1500-3000%7Carea:152,146,133,141,120,122,130,101,131,132,401,402%7Cbeds%3C=1?page=1"
  page_1 <- paste0(base_url,price_min,"-",price_max,"%7Carea:",areas,"%7Cbeds%3C",bedrooms,"?page=", 1) # Getting first page
  
  remDr$navigate(page_1)
  
  remDr$screenshot(display = TRUE)
  
  total_pages <- remDr$getPageSource() %>%
    .[[1]] %>%
    read_html() %>%
    html_nodes(".page") %>% 
    html_text() %>% 
    trimws() %>% 
    as.numeric() %>% 
    max(na.rm=T)
  
  
  remDr$close()
  print("Closed initial server")
  
  print(paste0("Scraping StreetEasy. Total pages: ", as.character(total_pages)))
  
  pages_vector <- seq(1,total_pages,1)
  
  # pages_vector <- seq(1,3) # Temporary override of number of pages
    
  output_table <- map_df(
    pages_vector, function(.x) {
      
      # Sys.sleep(runif(1, min_wait, max_wait))
      print("Spinning up new server")
      remDr <- f_spinup()
      print("Opening new server")
      remDr$open()
      
      output = f_scrape_streeteasy_page_sel(
        remote_driver = remDr,
        base_url = "https://streeteasy.com/for-rent/nyc/status:open%7Cprice:", 
        price_min = 1500, 
        price_max = 3000, 
        areas = "152,146,133,141,120,122,130,101,131,132,401,402", 
        bedrooms = "=1", 
        pageno = .x)
      
      print("Closing new server")
      remDr$close()
      
      
      return(output)
    }
  )
  
  remDr$close()
  
  return(output_table)
}


f_format_streeteasy_scrape <- function(scrape_table){
  scrape_output <- scrape_table %>%
    mutate(monthly_price_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_price_usd))),
           monthly_min_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_min_usd))),
           monthly_max_usd = as.numeric(ifelse(is.na(monthly_max_usd),
                                               monthly_min_usd,
                                               gsub("[^0-9]","",monthly_max_usd))),
           coordinates = gsub(",","+",coordinates) # Changes 40.7525,-73.9337 to 40.7525+-73.9337
    )
  
  return(scrape_output)
}
