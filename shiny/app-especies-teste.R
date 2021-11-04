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
  filter(fauna_flora != "")%>%
  mutate(name_biome = bioma,
         name_biome = if_else(name_biome == "Ilha oceanica", "Sistema Costeiro", name_biome),
         name_biome = if_else(name_biome == "Marinho", "Sistema Costeiro", name_biome))

original <- readRDS("../data/tratado_nao_sep.RDS")

biomas <- read_biomes()




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

    menuItem("Filtros do dashboard", tabName = "search",icon = icon("search"),
             radioButtons("fauna_flora", label = "Fauna ou Flora", choices = c("Fauna", "Flora"), selected = "Fauna"),
             uiOutput("out_bioma"),
             uiOutput("out_grupo"),
             uiOutput("out_grupo_mapa"),
             uiOutput("especie")
             
    ),
    menuItem("Mapa", tabName = "mapa", icon = icon("map-marker-alt")), #bioma ou estado: 
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
    tabItem(tabName = "mapa", plotlyOutput("mapa")),
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
  
  
  ## mapa grupo de esp'ecie ----
  
  grupo_mapa <- reactive({
    data %>%
      pull(grupo) %>%
      unique()#%>%
      #c("Todos")
  }) ######## juntar categorias sob PLANTAS ------- #
  
  output$out_grupo_mapa <- renderUI({
    selectInput("in_grupo_mapa", label = "Grupo do mapa", choices = grupo_mapa())
  })
  



output$mapa <- renderPlotly({
  
  #data
  # if (input$in_grupo_mapa != "Todos"){
    #no caso de filtrar grupo
    data_grupo <- #reactive({
      subset(
        data, grupo == input$in_grupo_mapa
      ) %>%
      select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca, bioma,
             principais_ameacas, name_biome)%>%
      group_by(name_biome, grupo, sigla_categoria_de_ameaca)%>%
      summarise(n = n()) %>%
      rename("sigla" = sigla_categoria_de_ameaca) %>%
      ungroup()
    
    data_grupo <- left_join(biomas, data_grupo, by = "name_biome")
  #   
  #   #})
  # } else { 
  #   #no caso de nao filtrar grupo
  #   data_grupo <- #reactive({
  #     data%>%
  #     select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca, bioma,
  #            principais_ameacas, name_biome, geom)%>%
  #     group_by(grupo, sigla_categoria_de_ameaca, geom)%>%
  #     summarise(n = n()) %>%
  #     mutate(name_biome = "Todos")
  #     #})
  #   }
  
  #plot
    mapa_bioma <- data_grupo %>% ggplot() + #geom_sf vem do pacote sf
      geom_sf(aes(fill = name_biome), size = .1, color = "grey5")+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.text = element_blank(),
            legend.position = "none",
            axis.ticks = element_blank())
    
    ggplotly(mapa_bioma, tooltip = c("name_biome", "grupo", "sigla", "n"))
    
    })

  #modelo de condicional ----
  # output$table <- renderTable({
  #   data <- gapminder
  #   data <- subset(
  #     data,
  #     lifeExp >= input$life[1] & lifeExp <= input$life[2]
  #   )
  #   # Don't subset the data if "All" continent are chosen
  #   if (input$continent != "All") {
  #     data <- subset(
  #       data,
  #       continent == input$continent
  #     )
  #   }
  #   data
  # })

  #gera html pra usar no getPage com base no var_especie

  
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