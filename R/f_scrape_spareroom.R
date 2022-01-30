

# Function to create table of data from a page
f_scrape_spareroom_page <- function(base_url, search_id, end, pageno, total_pages){
  
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

# f_scrape_spareroom_page(
#   base_url = "https://www.spareroom.com/roommate/?",
#   search_id = "search_id=500050688210",
#   end = "&sort_by=age&mode=list",
#   pageno = 1)

f_scrape_spareroom_pages <- function(base_url, search_id, end, pageno, results_per_page){
  
  page_1 <- paste0(base_url, "offset=0", "&", search_id, end)
  
  num_results <- page_1 %>% 
    read_html() %>% 
    html_nodes("#results_header strong:nth-child(2)") %>% 
    html_text() %>% 
    as.numeric()
  
  total_pages <- ceiling(num_results / results_per_page)
  pages_vector <- seq(1,total_pages,1)
  
  output_table <- map_df(
    pages_vector, function(.x) {
      f_scrape_spareroom_page(
        base_url = base_url, 
        search_id = search_id, 
        end = end, 
        pageno = .x,
        total_pages = total_pages
      )
    }
  )
  
  return(output_table)
}

# f_scrape_spareroom_pages(
#   base_url = "https://www.spareroom.com/roommate/?",
#   search_id = "search_id=500050688210",
#   end = "&sort_by=age&mode=list",
#   pageno = 1,
#   results_per_page = 11
# )


f_format_spareroom_scrape <- function(scrape_table){
  scrape_output <- scrape_table %>%
    mutate(monthly_price_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_price_usd))),
           monthly_min_usd = as.numeric(trimws(gsub("[^0-9]","",monthly_min_usd))),
           monthly_max_usd = as.numeric(ifelse(is.na(monthly_max_usd),
                                               monthly_min_usd,
                                               gsub("[^0-9]","",monthly_max_usd)))
    )
  
  return(scrape_output)
}