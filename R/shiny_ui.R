shiny_ui <- function(){
  return(
    shinydashboard::dashboardPage(
      skin = "black",
      shinydashboard::dashboardHeader(title = "gapackage"),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "menu",
          shinydashboard::menuItem("Parameters", tabName = "tab_parameters", icon = icon("table"))
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "tab_parameters",
            uiOutput(outputId = "parameter_list_ui")
          )
        )
      )
    )
  )
}
