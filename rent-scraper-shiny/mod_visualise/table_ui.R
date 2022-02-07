tab_table <- tabItem(
  
  tabName = "tab_table",
  
  tags$div(id = "table",
           fluidPage(
             p("Table output"),
             dataTableOutput("renttable")
           )
           
  )
)