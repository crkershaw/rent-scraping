# NYC Rent Scraping
This is a set of scripts for scraping rental data for New York City. For any given Spare Room search, the script goes through every page of results and extracts key information about each listing.

It then uses the Google Maps API to pull longitude and latitude for their postcodes, and uses this to calculate commute time to work in midtown.

The second script then takes this and performs analysis by neighbourhood, plotting a series of charts to compare neighbourhoods as well as a map that colours postcodes by commute time and overlays their average cost.

## Script 1 - Rent Scraping
The first script scrapes spare room, using any search URL that you want.

### Inputs
Search ID (Create the search on spare room, then find the search ID in the the URL)

Number of results per page - 11 by default, update if this changes

Google API Login

### Outputs
Rdata file of table of all scraped results, with commute times included

## Script 2 - Rent Analysis

### Inputs
RData file of table of scraped results from script 1

Google API Login

### Outputs
Numerous plots comparing different areas by rent price and commute time (not saved)

Map of New York, split by postcodes with rental prices overlaid and coloured by commute time (from postcode center)

## Logging into the Google Maps API
### Enabling the Distance Matrix API
You can find the instructions for enabling the [Directions](https://developers.google.com/maps/documentation/directions/cloud-setup?hl=en), [Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/cloud-setup), and the [GeoCode API](https://developers.google.com/maps/documentation/javascript/geocoding?hl=en) used to calculate commute times, 
and the [Maps Static API](https://developers.google.com/maps/documentation/maps-static/overview) used to visualise the map. 

You must enable the APIs, and create a API Key credential for it. Then save the api key in the api-key folder (or somewhere else and adjust the link to it).