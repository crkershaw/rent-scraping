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
