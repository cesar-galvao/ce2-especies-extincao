library(shinydashboard)
library(shiny)
library(tidyverse)



# PROBLEMAS ----

# 1. Se tiver botoes de filtro no busca de especie, nao renderiza html
# 2. uiOutput nao renderiza os filtros realizados no reactive server

# tentar calculos dos filtros usando eventReactive


#---------------------------------#

data <- readRDS("../data/especies_2020_pronto.RDS") %>%
  filter(fauna_flora != "")

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
    menuItem("Busca de espécie", tabName = "wiki",icon = icon("globe-americas"),
             radioButtons("fauna_flora", label = "Fauna ou Flora", choices = c("Fauna", "Flora"), selected = "Fauna"),
             uiOutput("out_bioma")
             # uiOutput("ameaca"),
             # uiOutput("grupo"),
             # uiOutput("especie")
             
             #opcoes tem que ser todos do busca especie + Todos
             
    ),
    menuItem("Banco de dados", tabName = "database",icon = icon("globe-americas")) #dizer de onde vem, mostrar em tabela
  )
)

body <- dashboardBody(
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "apresentacao"),
    tabItem(tabName = "mapa"),
    tabItem(tabName = "testes-hip"),
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
  
  

# Pagina wiki menu interativo ----
  
  #filtered data
  busca_especie <- reactive({
    dplyr::filter(data,
           fauna_flora %in% input$fauna_flora,
           grupo %in% grupo(),
           bioma %in% bioma(),
           principais_ameacas %in% principal_ameaca())
  })
  
  # get filter from inputs
  grupo <- reactive({
    if (input$grupo == "Todos") unique(data$grupo) else input$grupo
  })
  bioma <- reactive({
    if (input$bioma == "Todos") unique(data$bioma) else input$bioma
  })
  principal_ameaca <- reactive({
    if (input$ameaca == "Todas") unique(data$principais_ameacas) else input$ameaca
  })
  
  #get available categories
  var_grupo <- reactive({
    dplyr::filter(busca_especie(), grupo %in% grupo()) %>%
      pull(Country) %>%
      unique()
  })
  var_bioma <- reactive({
    dplyr::filter(busca_especie(), bioma %in% bioma()) %>%
      pull(Country) %>%
      unique()
  })
  var_ameaca <- reactive({
    dplyr::filter(busca_especie(), principais_ameacas %in% principal_ameaca()) %>%
      pull(Country) %>%
      unique()
  })

  var_especie <- reactive({
    busca_especie() %>%
      pull(especie_simplificado) %>%
      unique()
  })

  # gerando opcoes de output
  output$out_bioma <- renderUI({
    selectInput(inputId = "bioma", label = "Bioma", choices = c("Todos", var_bioma()), selected = "Todos")
  })

  output$ameaca <- renderUI({
    selectInput("ameaca", label = "Principal ameaça", choices = c("Todas", var_ameaca()), selected = "Todas")
  })

  output$grupo <- renderUI({
    selectInput("grupo", label = "Grupo", choices = c("Todos", var_grupo()), selected =  "Todos")
  })

  output$especie <- renderUI({
    selectInput("especie", label = "Espécie", choices = c(var_especie()))
  })


  #gera html pra usar no getPage com base no var_especie

  especie_link <- reactive ({
    paste("https://pt.wikipedia.org/wiki/",
          str_replace_all(input$especie, pattern = " ", replacement = "_"),
          sep = "")
  })

  #gerar link com base nos inputs
  getPage<-function() {
    return(tags$iframe(src = "https://en.wikipedia.org/wiki/Mecistogaster_pronoti"
                       , style="width:100%;",  frameborder="0"
                       ,id="iframe"
                       , height = "500px"))
  }
  output$wiki_page<-renderUI({
    getPage()
  })
  
  
}

shinyApp(ui, server)