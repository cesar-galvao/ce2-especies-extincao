library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)



# PROBLEMAS ----

# 1. Se tiver botoes de filtro no busca de especie, nao renderiza html
# 2. uiOutput nao renderiza os filtros realizados no reactive server

#---------------------------------#

data <- readRDS("../data/especies_2020_pronto.RDS") %>%
  filter(fauna_flora != "")

original <- readRDS("../data/tratado_nao_sep.RDS")

# inserir série histórica?

#input de lista
#página com endereço reactive

#triggers de avisos para entrada em cada aba. Como notificar cada vez que abre uma aba nova?


# COMPONENTES DO UI ----

header <- dashboardHeader( title = "Visualização de expécies em extinção",
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
    menuItem("Busca de espécie", tabName = "search",icon = icon("search"),
             radioButtons("fauna_flora", label = "Fauna ou Flora", choices = c("Fauna", "Flora"), selected = "Fauna"),
             uiOutput("out_bioma"),
             uiOutput("out_grupo"),
             uiOutput("especie")
             
    ),
    menuItem("Página Wiki", tabName = "wiki_page",icon = icon("globe-americas")), #dizer de onde vem, mostrar em tabela
    menuItem("Banco de dados", tabName = "database",icon = icon("table"))
  )
)

body <- dashboardBody(
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "apresentacao"),
    tabItem(tabName = "mapa"),
    tabItem(tabName = "testes-hip"),
    tabItem(tabName = "search"#,
            #Caixa com menu dos filtros para grupo, categoria ameaca, bioma em caixa superior
            #htmlOutput("wiki_page")
            
    ),
    tabItem(tabName = "wiki_page", htmlOutput("wiki_page")),
    tabItem(tabName = "database",
            fluidRow(
              box(title = "Dados agregados", solidHeader = TRUE, width = "6",
                  DT::dataTableOutput("table"))
              )
    )
  )
)


# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)


# COMPONENTES DO SERVIDOR ----

server <- function(input, output) {
  
  

## Pagina wiki menu interativo ----
  
  
  # get filter from inputs

  sel_bioma <- data %>% distinct(bioma) %>% pull
  
  
  # gerando opcoes de output
  output$out_bioma <- renderUI({
    selectInput(inputId = "bioma", label = "Bioma", choices = sel_bioma)
  })
  

  #filtered data
  busca_especie <- reactive({
    dplyr::filter(data,
                  fauna_flora == input$fauna_flora,
                  bioma %in% input$bioma)
  })
  
  var_grupo <- reactive({
    busca_especie() %>%
      dplyr::filter(bioma %in% input$bioma)%>%
      pull(grupo) %>%
      unique()
  })
  
  output$out_grupo <- renderUI({
    selectInput(inputId = "grupo", label = "Grupo", choices = var_grupo())
  })

  var_especie <- reactive({
    busca_especie() %>%
      dplyr::filter(grupo %in% input$grupo)%>%
      pull(especie_simplificado) %>%
      unique()
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
  
  output$texto <- renderText("My text")

  #gerar link com base nos inputs
  getPage<-function(x) {
    return(tags$iframe(src = x, 
                       style="width:100%;",  frameborder="0",
                       id="iframe",
                       height = "500px"))
  }
  output$wiki_page<-renderUI({
    getPage(especie_link())
  })
  
  ## Base de dados ----
  
  output$table <- DT::renderDataTable({
    original%>%
      select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca,
             bioma, principais_ameacas)
  })
  # output$table <- renderTable({
  #     tabela <- original%>%
  #       select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca,
  #              bioma, principais_ameacas)
  #     tabela
  #   })
  
}

shinyApp(ui, server)