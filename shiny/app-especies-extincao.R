library(shinydashboard)
library(shiny)


# inserir série histórica?

#triggers de avisos para entrada em cada aba. Como notificar cada vez que abre uma aba nova?

header <- dashboardHeader(
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "Verifique o menu lateral!"
    )
  )
) 
sidebar <- dashboardSidebar(
  sidebarMenu(
    # Create two `menuItem()`s, "Dashboard" and "Inputs"
    menuItem("Apresentação", tabName = "apresentacao"),
    menuItem("Mapa", tabName = "mapa"),
    menuItem("Comparações", tabName = "testes-hip"), #testes de hipotese
    menuItem("Busca de espécie", tabName = "wiki"),
    menuItem("Banco de dados", tabName = "database") #dizer de onde vem, mostrar em tabela
  )
)

body <- dashboardBody(
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "apresentacao"),
    tabItem(tabName = "mapa"),
    tabItem(tabName = "testes-hip"),
    tabItem(tabName = "wiki"),
    tabItem(tabName = "database")
  )
)


# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output) {}

shinyApp(ui, server)