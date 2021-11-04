library(geobr)
library(tidyverse)

data <- readRDS("./data/especies_2020_pronto.RDS") %>%
  filter(fauna_flora != "")

biomas <- read_biomes()


data <- data %>%
  mutate(name_biome = bioma,
         name_biome = if_else(name_biome == "Ilha oceanica", "Sistema Costeiro", name_biome),
         name_biome = if_else(name_biome == "Marinho", "Sistema Costeiro", name_biome))

biomas <- biomas %>%
  select(name_biome, geom)%>%
  mutate(dummy = 1)

data_mapa <- left_join(data, biomas,  by = "name_biome") %>%
  select(fauna_flora, grupo, familia, especie_simplificado, sigla_categoria_de_ameaca, bioma,
         principais_ameacas, name_biome, geom)
#no caso de filtrar grupo
data_mapa_esp <- data_mapa%>%
  group_by(name_biome, grupo, sigla_categoria_de_ameaca)%>%
  summarise(n = n())
  

# filtrar por grupo ou "all" - novo filtro: grupo do mapa

# Quantidade de espécies em extinção em cada categoria
# 5 principais ameaças para aquele grupo
# lembrar de filtrar para espécies únicas



mapa_bioma <- data_grupo %>% ggplot() + #geom_sf vem do pacote sf
  geom_sf(aes(fill = name_biome), size = .1, color = "grey5")+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        axis.ticks = element_blank())

#teremos que substituir ilha oceanica e marinho com sistema costeiro.