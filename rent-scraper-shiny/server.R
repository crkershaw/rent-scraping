
# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data_rent <- output_commute %>%
    select(id, title, link, neighbourhood, address, postcode, postcode_lat, postcode_long, coordinates, type, monthly_price_usd, 
           `commute_time_Mt-Sinai`, commute_time_TimesSquare, commute_time_KTown, commute_time_LES) %>%
    mutate(`commute_time_Mt-Sinai` = round(`commute_time_Mt-Sinai` / 60,0),
           commute_time_TimesSquare = round(commute_time_TimesSquare / 60,0),
           commute_time_KTown = round(commute_time_KTown / 60,0),
           commute_time_LES = round(commute_time_LES / 60,0))

  
  
  source("mod_visualise/table_server.R", local = TRUE)
  source("mod_visualise/graphs_server.R", local = TRUE)
  
}
