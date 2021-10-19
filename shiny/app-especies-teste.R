library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)



# PROBLEMAS ----

# alinhamento e distancia entre botoes

#---------------------------------#

data <- readRDS("../data/especies_2020_pronto.RDS") %>%
  filter(fauna_flora != "")

original <- readRDS("../data/tratado_nao_sep.RDS")


# COMPONENTES DO UI ----

header <- dashboardHeader(title = "Dash CE2",
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

    menuItem("Filtros do dashboard", tabName = "search",icon = icon("search"),
             radioButtons("fauna_flora", label = "Fauna ou Flora", choices = c("Fauna", "Flora"), selected = "Fauna"),
             uiOutput("out_bioma"),
             uiOutput("out_grupo"),
             uiOutput("especie")
             
    ),
    menuItem("Mapa", tabName = "mapa"), #bioma ou estado: 
      #painel mapa com: quantidade de espécies fauna/flora,
      #principais ameaças
      #tabela lateral com quantidade de espécies, pelo filtro geral, em cada categoria de risco
    menuItem("Comparações", tabName = "testes-hip"), #testes de hipotese
    menuItem("Página Wiki", tabName = "wiki_page",icon = icon("globe-americas")), #dizer de onde vem, mostrar em tabela
    menuItem("Banco de dados", tabName = "database",icon = icon("table")),
    menuItem("Recursos", tabName = "recursos", icon = icon("cog"),
             actionButton(inputId = "dload_dados", label = "Dados limpos", icon = icon("download"),
                          onclick ="window.open('https://github.com/cesar-galvao/ce2-especies-extincao/raw/main/data/tratado_nao_sep.RDS', '_blank')"),
             actionButton(inputId = "our_repo", label = "Nosso repositório", icon = icon("github"),
                          onclick ="window.open('https://github.com/cesar-galvao/ce2-especies-extincao', '_blank')"))
  )
)

body <- dashboardBody(
  tags$head(tags$style(HTML('.box {margin: 25px;}'
  ))),
  tabItems(
    # Add two tab items, one with tabName "dashboard" and one with tabName "inputs"
    tabItem(tabName = "apresentacao"),
    tabItem(tabName = "search"#,
            #Caixa com menu dos filtros para grupo, categoria ameaca, bioma em caixa superior
            #htmlOutput("wiki_page")
            
    ),
    tabItem(tabName = "mapa"),
    tabItem(tabName = "testes-hip"),
    tabItem(tabName = "wiki_page", htmlOutput("wiki_page")),
    tabItem(tabName = "database",
            title = "Dados agregados", solidHeader = TRUE, width = "11",
                  DT::dataTableOutput("table")
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
                       height = "720px"
                       ))
  }
  output$wiki_page<-renderUI({
    getPage(especie_link())
  })
  
  ## Base de dados ----
  
  output$table <- DT::renderDataTable({
    original%>%
      select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca,
             bioma, principais_ameacas) %>%
      DT::datatable(rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      autoWidth = FALSE, scrollX = TRUE))
  })
  # output$table <- renderTable({
  #     tabela <- original%>%
  #       select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca,
  #              bioma, principais_ameacas)
  #     tabela
  #   })
  
}

shinyApp(ui, server)