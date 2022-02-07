

# Table -------------------------------------------------------------------

data_rent_score <- reactive({
  
  data_raw <- data_rent

  f_commutescore <- function(column, mintime, prod){
    # 1 / (max(0,(column - mintime)) ** prod / mean(max((column - mintime), 0) ** prod ))
    1 / (column ** prod / mean(column ** prod ))
  }
  
  weight_commute_mtsinai <- input$weight_comm_1
  weight_commute_ktown <- input$weight_comm_2
  weight_commute_timessquare <- input$weight_comm_3
  weight_commute_les <- input$weight_comm_4
  
  weight_commute <- input$weight_commute
  weight_price <- input$weight_price 
  
  data_weighted <- data_raw %>%
    ungroup() %>%
    mutate(price_score = 1 / (monthly_price_usd / mean(monthly_price_usd))) %>%
    
    mutate(
      commute_mtsinai_score = f_commutescore(`commute_time_Mt-Sinai`, commutes_mintime, product),
      commute_timessquare_score = f_commutescore(commute_time_TimesSquare, commutes_mintime, product),
      commute_ktown_score = f_commutescore(commute_time_KTown, commutes_mintime, product),
      commute_les_score = f_commutescore(commute_time_LES, commutes_mintime, product),
      # commute_mtsinai_score = 1 / (`commute_time_Mt-Sinai` ** product / mean(`commute_time_Mt-Sinai` ** product )),
      # commute_timessquare_score = 1 / (commute_time_TimesSquare ** product  / mean(commute_time_TimesSquare ** product )),
      # commute_ktown_score = 1 / (commute_time_KTown ** product  / mean(commute_time_KTown ** product )),
      # commute_les_score = 1 / (commute_time_LES ** product  / mean(log(commute_time_LES ** product ))),
      commute_score = (
       commute_mtsinai_score * weight_commute_mtsinai + 
       commute_timessquare_score * weight_commute_timessquare +
       commute_ktown_score * weight_commute_ktown +
       commute_les_score * weight_commute_les
     ) / sum(weight_commute_mtsinai + weight_commute_timessquare + weight_commute_ktown + weight_commute_les)
    ) %>%
    
    mutate(score_final = (
      ((price_score * weight_price) + (commute_score * weight_commute)) / 100
    )) %>%
    arrange(-score_final)
  
  rvalues$data_rent_score <- data_weighted
})


# Scatter Chart -----------------------------------------------------------


output$select_graph_y <- renderUI({
  selectInput("select_graph_y", label = "Graph Y Axis", 
              choices = axis_choices,
              selected = axis_choices[1]
              
  )
})

output$select_graph_x <- renderUI({
  selectInput("select_graph_x", label = "Graph X Axis", 
              choices = axis_choices,
              selected = axis_choices[2]
              
  )
})


output$graph_rent_scatter <- renderPlotly({
  
  data_rent_score()

  graph_data <- rvalues$data_rent_score

  y_axis <- list(title = names(axis_choices)[axis_choices == input$select_graph_y],
                 range = c(0, max(graph_data[[input$select_graph_y]])*1.1),
                           hoverformat = "0f")

  x_axis <- list(title = names(axis_choices)[axis_choices == input$select_graph_x],
                 range = c(0, max(graph_data[[input$select_graph_x]])*1.1),
                 hoverformat = "0f")

  plot_ly(graph_data, x = ~ get(input$select_graph_x), source = "A") %>%
    add_trace(
      y = ~ get(input$select_graph_y),
      name = ~neighbourhood,
      type = "scatter",
      mode = "markers",
      color = ~neighbourhood,
      hoverlabel = list(namelength=0),
      text = ~ paste0(
        "ID: ", id, 
        "\nNeighbourhood: ", neighbourhood, 
        "\nCost: ", monthly_price_usd, 
        "\nMins to Mt Sinai: ", `commute_time_Mt-Sinai`,
        "\nMins to Times Square: ", `commute_time_TimesSquare`,
        "\nMins to KTown: ", `commute_time_KTown`,
        "\nMins to LES: ", `commute_time_LES`),
      hovertemplate = paste("%{text}",
                            "<br><i>%{x}</i>")
    ) %>%
    config(displayModeBar = F) %>%
    layout(yaxis = y_axis,
           xaxis = x_axis,
           hovermode = "closest")


})



# Map Chart ---------------------------------------------------------------

output$select_map_color <- renderUI({
  selectInput("select_map_color", label = "Color", 
              choices = axis_choices,
              selected = axis_choices[1]
              
  )
})


output$graph_rent_map <- renderPlotly({
  
  data_rent_score()
  
  graph_data <- rvalues$data_rent_score

  fig <- plot_mapbox(mode = 'scattermapbox')
  fig <- fig %>%
    add_markers(
      data = graph_data,
      x=~postcode_long,
      y=~postcode_lat,
      text = ~paste0(
        "ID: ", id,
        "\nNeighbourhood: ", neighbourhood,
        "\nCost: ", monthly_price_usd,
        "\nMins to Mt Sinai: ", `commute_time_Mt-Sinai`,
        "\nMins to Times Square: ", `commute_time_TimesSquare`,
        "\nMins to KTown: ", `commute_time_KTown`,
        "\nMins to LES: ", `commute_time_LES`),
      color = ~get(input$select_map_color),
      size = I(20)
    )
  
  
  fig <- fig %>% layout(
    mapbox=list(
      style="carto-positron",
      zoom =12,
      center=list(lon= -73.95, lat=40.76))
  )
  
  fig
})



# Weightings --------------------------------------------------------------

# Commute weights
output$weight_comm_1 <- renderUI({
  sliderInput("weight_comm_1", "Weight: Mins to Mt Sinai", min=0, max=100, value = weight_commute_mtsinai)
})

output$weight_comm_2 <- renderUI({
  sliderInput("weight_comm_2", "Weight: Mins to Times Square", min=0, max=100, value = weight_commute_timessquare)
})

output$weight_comm_3 <- renderUI({
  sliderInput("weight_comm_3", "Weight: Mins to KTown", min=0, max=100, value = weight_commute_ktown)
})

output$weight_comm_4 <- renderUI({
  sliderInput("weight_comm_4", "Weight: Mins to LES", min=0, max=100, value = weight_commute_les)
})

# Overall weights
output$weight_commute <- renderUI({
  column(6, sliderInput("weight_commute", "Weight: Commutes", min=0, max=100, value = weight_commute))
})

output$weight_price <- renderUI({
  column(6, sliderInput("weight_price", "Weight: Cost", min=0, max=100, value = weight_price))
})


# Table -------------------------------------------------------------------


output$renttable <- renderDataTable({
  
  rvalues$data_rent_score %>%
    select(id, title, neighbourhood, type, monthly_price_usd, 
           `commute_time_Mt-Sinai`, commute_time_TimesSquare, commute_time_KTown, commute_time_LES,
           link,
           commute_score, price_score, score_final) %>%
    mutate(
      commute_score = round(commute_score, 2),
      price_score = round(price_score, 2),
      score_final = round(score_final, 2)
    ) %>%
    rename(
      "ID" = "id",
      "Listing Title" = "title",
      "Neighbourhood" = "neighbourhood",
      "Type" = "type",
      "Link" = "link",
      "USD / Month" = "monthly_price_usd",
      "Mins to Mt Sinai" = "commute_time_Mt-Sinai", 
      "Mins to Times Square" = "commute_time_TimesSquare", 
      "Mins to KTown" = "commute_time_KTown", 
      "Mins to LES" = "commute_time_LES",
      "Score: Commute" = "commute_score",
      "Score: Price" = "price_score",
      "Score: Final" = "score_final"
    ) 
})

