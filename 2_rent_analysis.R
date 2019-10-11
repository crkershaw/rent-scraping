#### Searching for a room in NYC ####


# 2. Analysis of scraped spare room data
# This second script takes the table of scraped data from the first script and analyses it
# It produces descriptive graphs to examine neighbourhoods by commute time and price
# It also saves a map of New York with this information overlaid onto it

# Basic Setup -------------------------------------------------------------

library(ggmap)
library(googleAuthR)
library(rjson)
library(httr)
library(rgdal)
library(rgeos)
library(broom)
library(tidyverse)
library(tigris)
library(maptools)
library(ggrepel)
library(shadowtext)

CustomTheme <- theme_bw()+
  theme(
    legend.text = element_text(colour="grey47"),
    legend.title = element_text(colour="grey47", face = "bold"),
    legend.background = element_rect(colour="grey", fill = "grey94"),
    panel.background = element_rect("white"),
    plot.background = element_rect("white"),
    panel.border = element_rect("grey",fill=NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=0.5,colour="#2F8197", face = "bold", size = 14),
    plot.subtitle = element_text(colour="grey47"),
    axis.title = element_text(colour="grey47", face = "bold"),
    axis.text = element_text(colour="grey47"),
    strip.background = element_rect(colour="grey70",fill="lightblue1"),
    strip.text = element_text(colour="grey47", face = "bold")
  )


# Inputs ------------------------------------------------------------------
google_login <- read_lines("C:/Users/charlie-kershaw/Documents/R/google_api_key.txt")

date_string <- "2019-7-12"   # Enter the date you would like to use the data for in format YYYY-M-D (as per data file)

# Analysis ----------------------------------------------------------------
load(paste0("scrapes/",date_string," rent_scrape.Rdata"))

register_google(key = google_login)

# Cutting out too far
output_out <- output_commute %>%
  mutate(commute_time = commute_time / 60) %>%
  filter(commute_time <= 45)

# Assinging points by distance and cost
output_analysis <- output_out %>%
  mutate(price_2 = (monthly_max_usd ** 2) / median(monthly_max_usd ** 2, na.rm=TRUE),
         time_2 = commute_time ** 2 / median(commute_time ** 2, na.rm=TRUE),
         timeprice_2 = price_2 * time_2)


# Descriptives ------------------------------------------------------------

# Grouped by neighbourhood
output_neighbourhood <- output_out %>%
  group_by(neighbourhood) %>%
  summarise(median_commute_time = median(commute_time,na.rm = TRUE),median_rent_max = median(monthly_max_usd,na.rm = TRUE),number=n())

# Median cost by neighbourhood
output_neighbourhood %>%
  ggplot(.,aes(x=reorder(neighbourhood,-median_rent_max),y=median_rent_max,fill=number))+
  geom_col()+
  coord_flip()+
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ggtitle("Neighbourhood by Median Rent")

# Median commute by neighbourhood
output_neighbourhood %>%
  ggplot(.,aes(x=reorder(neighbourhood,-median_commute_time),y=median_commute_time,fill=number))+
  geom_col()+
  coord_flip()+
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ggtitle("Neighbourhood by Median Commute")

# Cost vs Commute by neighbourhood
output_neighbourhood %>%
  ggplot(.,aes(median_rent_max,median_commute_time))+
  geom_point(aes(size=number)) +
  geom_smooth() +
  CustomTheme +
  geom_label_repel(data=subset(output_neighbourhood,median_commute_time <=25),aes(label = neighbourhood))



# Summary info by postcode
summary_postcode <- output_out %>%
  mutate(long_lat = paste0(long,"-",lat)) %>%
  group_by(long_lat) %>%
  summarise(long=max(long),
            lat=max(lat),
            postcode=max(postcode),
            median_monthly = median(monthly_max_usd,na.rm=TRUE),
            min_monthly = min(monthly_min_usd,na.rm=TRUE),
            max_monthly = max(monthly_max_usd,na.rm=TRUE),
            med_commute_time = median(commute_time),
            med_commute_distance = median(commute_distance),
            num = n())


# Setting up map ----------------------------------------------------------
nyc_map <- get_map(location = c(lon = -74.00, lat = 40.71), maptype = "terrain", zoom = 11)
ggmap(nyc_map)


# Zip Code Data
lookup_code("New York", "New York")
zipcodes <- zctas(cb = TRUE, starts_with = c("10","11"))

plot_data <- tidy(zipcodes, region = "GEOID10") %>%
  mutate(id = as.numeric(id)) %>%
  rename(long_zip = long) %>%
  rename(lat_zip = lat) %>%
  left_join(., summary_postcode, by = c("id" = "postcode")) %>%
  filter(!is.na(long_lat))

# Neighbourhoods Data
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
summary(nyc_neighborhoods)
nyc_neighborhoods_df <- tidy(nyc_neighborhoods)

rent_map <- ggmap(nyc_map) + 
  geom_polygon(data=plot_data, aes(x=long_zip, y=lat_zip, group=group, fill = med_commute_time),colour="blue",alpha=0.7) +
  scale_fill_gradient2(low="green",high="red",mid="white",midpoint = 25,limits=c(0,45))+
  #geom_point(data=summary_postcode,aes(long,lat,size=median_monthly),alpha = 0.5)+
  xlim(c(-74.05,-73.9))+
  ylim(c(40.63,40.85))+
  geom_shadowtext(data=summary_postcode,aes(long,lat,label = ceiling(median_monthly)),size=3, colour = "black", bg.colour = "white")

rent_map


# Saving the map ----------------------------------------------------------

set.seed(142)
png(paste0("outputs/",date_string," Rent map.png"), width=900,height=1400,res=144)
rent_map
dev.off()
