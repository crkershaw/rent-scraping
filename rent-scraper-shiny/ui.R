# Defining header ---------------------------------------------------------
ui_header <- dashboardHeader(title="Rent Visualiser"
                             # tags$li(
                             #   class = "dropdown",
                             #   actionLink("toggleSidebar", "Toggle sidebar"), 
                             #   style = "align:left; color: #fff; text-align: center; display: inline-block; width: 130px")
                             )


# Defining sidebar --------------------------------------------------------

# Defining menu items
ui_sidebar_table <- div(id = "div_tab_table", class = "sidebar-menu",
                       menuItem(
                         text = "Analysis",
                         tabName = "tab_table_main",
                         icon = icon("globe"),
                         menuSubItem("Graphs", tabName = "tab_graphs")
                         # menuSubItem("Table", tabName = "tab_table"),
                       )
                       
)

# Pulling sidebar together
ui_sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "menu_sidebar",
              ui_sidebar_table
  )
)

# Defining dashboard body -------------------------------------------------

ui_dashboardbody <- dashboardBody(
  
  shinyjs::useShinyjs(), # Required to call this to use shiny js functions
  
  # Retrieves dimensions of browser window to allow popup window to vary in width
  tags$head(tags$script('var dimension = [0, 0];
                            $(document).on("shiny:connected", function(e) {
                            dimension[0] = window.innerWidth;
                            dimension[1] = window.innerHeight;
                            Shiny.onInputChange("dimension", dimension);
                            });
                            $(window).resize(function(e) {
                            dimension[0] = window.innerWidth;
                            dimension[1] = window.innerHeight;
                            Shiny.onInputChange("dimension", dimension);
                            });')), 

  actionButton("browser", "Click to Debug"),
  tags$script("$('#browser').hide();"),
  
  actionButton("stopapp", "Click to Stop App"),
  tags$script("$('#stopapp').hide();"),
  
  # Setting CSS tags --------------------------------------------------------
  
  tags$head(
    includeCSS("www/CSS.css")
  ),
  
  tabItems(
    tab_graphs
    # tab_table,

  )
)


# Pulling UI together -----------------------------------------------------

ui <- dashboardPage(
  title = "Rent Visualiser",
  ui_header,
  ui_sidebar,
  ui_dashboardbody
)