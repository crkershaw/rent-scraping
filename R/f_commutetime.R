

f_timedistance <- function(origin, destination){
  output = gmapsdistance(origin = origin,
                         destination = destination,
                         key = get.api.key(),
                         mode = "transit",
                         arr_date = commute_date,
                         arr_time = commute_arr_time)
}

# Usage Example --------------------------
# home = c("40.431478+-80.0505401","40.8310224+-73.9095279","40.7043156+-73.9212858") # Example home coordinates
# 
# test_times <- f_timedistance(home)
# 
# test_output <- tibble(latlong=home) %>%
#   mutate(distance = unlist(test_times$Distance[2]),
#          time = unlist(test_times$Time[2])
#   )