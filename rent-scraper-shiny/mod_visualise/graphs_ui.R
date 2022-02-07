tab_graphs <- tabItem(
  
  tabName = "tab_graphs",
  
  tags$div(id = "graphs",
           fluidPage(
             
             h2("Weightings"),
             p("Weighting of commute vs price"),
             fluidRow(
               uiOutput("weight_price"),
               uiOutput("weight_commute")
             ),
             
             p("Travel destination weightings"),
             fluidRow(
               column(3, uiOutput("weight_comm_1")),
               column(3, uiOutput("weight_comm_2")),
               column(3, uiOutput("weight_comm_3")),
               column(3, uiOutput("weight_comm_4"))
             ),
             
             h2("Rent Data Graph"),
             fluidRow(
               column(3, uiOutput("select_graph_x")),
               column(3, uiOutput("select_graph_y"))
             ),
             plotlyOutput("graph_rent_scatter"),
             
             h2("Rent Data Map"),
             uiOutput("select_map_color"),
             plotlyOutput("graph_rent_map"),
             
             h2("Table of results"),
             dataTableOutput("renttable")
           )
  )
)

