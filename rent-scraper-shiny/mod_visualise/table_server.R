# 
# output$renttable <- renderDataTable({
#   
#   data_rent %>%
#     select(id, title, neighbourhood, type, monthly_price_usd, 
#            `commute_time_Mt-Sinai`, commute_time_TimesSquare, commute_time_KTown, commute_time_LES,
#            link) %>%
#     rename(
#       "ID" = "id",
#       "Listing Title" = "title",
#       "Neighbourhood" = "neighbourhood",
#       "Type" = "type",
#       "Link" = "link",
#       "USD / Month" = "monthly_price_usd",
#       "Mins to Mt Sinai" = "commute_time_Mt-Sinai", 
#       "Mins to Times Square" = "commute_time_TimesSquare", 
#       "Mins to KTown" = "commute_time_KTown", 
#       "Mins to LES" = "commute_time_LES"
#     ) 
# })