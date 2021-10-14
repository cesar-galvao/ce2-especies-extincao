library(shinydashboard)
library(shiny)
library(tidyverse)


data <- readRDS("../data/especies_2020_pronto.RDS")
# inserir série histórica?

#input de lista
#página com endereço reactive

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
    menuItem("Busca de espécie", tabName = "wiki",
             selectInput("filtro1", ) #selecao dos filtros
             
    ),
    menuItem("Banco de dados", tabName = "database") #dizer de onde vem, mostrar em tabela
  )
)

body <- dashboardBody(
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "apresentacao"),
    tabItem(tabName = "mapa"),
    tabItem(tabName = "testes-hip"),
    tabItem(tabName = "wiki", #incluir filtro para grupo, categoria ameaca, bioma em caixa superior
            htmlOutput("wiki_page")),
    tabItem(tabName = "wiki",
            #Caixa com menu dos filtros para grupo, categoria ameaca, bioma em caixa superior
            htmlOutput("wiki_page")
    ),
    tabItem(tabName = "database")
  )
)


# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)

server <- function(input, output) {
  
  # processamento dos filtros
  
  #gera html pra usar no getPage
  
  #gerar link com base nos inputs
  getPage<-function() {
    return(tags$iframe(src = "https://en.wikipedia.org/wiki/Cognitive_bias"
                       , style="width:100%;",  frameborder="0"
                       ,id="iframe"
                       , height = "500px"))
  }
  output$wiki_page<-renderUI({
    x <- input$test
    getPage()
  })
}

shinyApp(ui, server)