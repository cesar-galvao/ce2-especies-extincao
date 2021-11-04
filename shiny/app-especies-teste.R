library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(geobr)
library(plotly)



# PROBLEMAS ----

# alinhamento e distancia entre botoes

#---------------------------------#

data <- readRDS("../data/especies_2020_pronto.RDS") %>%
  filter(fauna_flora != "")

original <- readRDS("../data/tratado_nao_sep.RDS")


# ameacas <- data %>% 
#   group_by(name_biome, principais_ameacas)%>%
#   summarise(n = n())%>%
#   top_n(5)%>%
#   select(-n)
# 
# vec_ameacas <- character()
# 
# 
# for (i in levels(as.factor(ameacas$name_biome))){
#   temp <- character()
#   for (j in 1:nrow(ameacas)){
#     if (ameacas$name_biome[j] == i){
#       temp <- c(temp, ameacas$principais_ameacas[j])
#     }
#   temp <- cat(temp, sep = ", ")
#   print(temp)
#   vec_ameacas <- c(vec_ameacas, temp)
#   }
# }
# 
# ameacas_tib <- tibble(
#   name_biome = distinct(ameacas$name_biome),
#   principais_ameacas = 
#     
# )


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
    menuItem("Apresentação", tabName = "apresentacao", icon = icon("file-alt")),

    menuItem("Filtros do Wiki", tabName = "search",icon = icon("search"),
             radioButtons("fauna_flora", label = "Fauna ou Flora", choices = c("Fauna", "Flora"), selected = "Fauna"),
             uiOutput("out_bioma"),
             uiOutput("out_grupo"),
             uiOutput("especie")
             
    ),
    menuItem("Relatório", tabName = "relatorio", icon = icon("window-maximize")), #bioma ou estado: 
      #painel mapa com: quantidade de espécies fauna/flora,
      #principais ameaças
      #tabela lateral com quantidade de espécies, pelo filtro geral, em cada categoria de risco
   # menuItem("Comparações", tabName = "testes-hip"), #testes de hipotese
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
    tabItem(tabName = "apresentacao", includeMarkdown("../r_docs/apresentacao.Rmd")),
    tabItem(tabName = "search"#,
            #Caixa com menu dos filtros para grupo, categoria ameaca, bioma em caixa superior
            #htmlOutput("wiki_page")
            
    ),
    tabItem(tabName = "relatorio",
            fluidRow(
              textOutput("comentarios")
            ),
            fluidRow(
              box(
                width = 5, status = "info", solidHeader = TRUE,
                title = "Espécies em extinção",
                DT::dataTableOutput("table_ext")),
              box(
                width = 5, status = "info", solidHeader = TRUE,
                title = "Extinções por bioma",
                plotOutput("graf_ext"))
              )
            ),
    #tabItem(tabName = "testes-hip"),
    tabItem(tabName = "wiki_page", htmlOutput("wiki_page")),
    tabItem(tabName = "database",
            title = "Dados agregados", solidHeader = TRUE, width = "11",
                  DT::dataTableOutput("table")
              )
    )
  )

## apresentação ----
# montar markdown em outro arquivo
# includeMarkdown("include.md") na aba correspondente



# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)


# COMPONENTES DO SERVIDOR ----

server <- function(input, output) {

  output$comentarios <- renderText({
    "Há 19 espécies registradas como possivelmente extintas (PEX) ou já extintos na natureza (EW).
Dentre os 5 biomas brasileiros, o que mais tem ocorrência de animais em extinção é a Mata Atlântica e posteriormente o Cerrado. Além disso, há 19 espécies possivelmente extintos (PEX) ou já extintos (EX), sendo 17 espécies exclusivas do Brasil."
  })

## Menu pagina wiki ----
  
  
  # get filter from inputs

  sel_bioma <- data %>% distinct(bioma) %>% pull 
  
  
  # gerando opcoes de output
  output$out_bioma <- renderUI({
    selectInput(inputId = "bioma", label = "Bioma", choices = sel_bioma)
  })
  


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
  
  
 


  
  ## pagina wiki ----
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

  output$table_ext <- DT::renderDataTable({
    data%>%
      filter(fauna_flora != "")%>%
      filter(sigla_categoria_de_ameaca == "CR PEW"|
               sigla_categoria_de_ameaca == "EW"|
               sigla_categoria_de_ameaca == "CR PEX") %>%
      group_by(sigla_categoria_de_ameaca, especie_simplificado, nome_comum, especie_exclusiva_do_brasil) %>%
      count() %>%
      rename('Sigla ameaça' = sigla_categoria_de_ameaca, 'Espécie' = especie_simplificado,
             'Autóctone' = especie_exclusiva_do_brasil, 'Nome Comum' = nome_comum) %>%
      select(-n)%>%
      DT::datatable(rownames = TRUE,
                    extensions = 'Buttons',
                    options = list(
                      autoWidth = FALSE, scrollX = TRUE))
  })
  
  output$graf_ext <- renderPlot({
    extinto <- c("CR PEW", "EW", "CR PEX")

    bioma_analise <- data
    bioma_analise$sigla_categoria_de_ameaca <- recode(bioma_analise$sigla_categoria_de_ameaca,
                                                      "CR PEW" = "Extinto", "EW" = "Extinto", "CR PEX" = "Extinto")

    bioma_analise_copia <- bioma_analise %>%
      filter(sigla_categoria_de_ameaca == "Extinto") %>% #filtrar quem tá extinto
      group_by(sigla_categoria_de_ameaca, bioma, especie_simplificado) %>% #agrupar por bioma
      distinct(sigla_categoria_de_ameaca, especie_simplificado)%>%
      ungroup()%>%
      group_by(sigla_categoria_de_ameaca, bioma)%>%
      count()
    
    bioma_analise_copia %>%
      ggplot(aes(x = bioma, y = n))+
      geom_bar(stat = 'identity', fill = '#3c8dbc')+
      labs(x = "", y = "")+
      ylim(0,18)+
      geom_text(aes(label = n), vjust = -.5)+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank())


  })
  
  
  ## mapa ----  
  # por bioma: qt de espec ameaçadas usando filtros abaixo 
  ##            principais ameacas
  # processar dados reactive
  # puxar filtro do menu
  # 1. Gerar objeto
  # 2. Render
  # 3. gerar UI
  
  ## apresentacao ----
  
  # output$apresentacao<-renderUI({includeHTML("apresentacao.html")})
  
}






shinyApp(ui, server)