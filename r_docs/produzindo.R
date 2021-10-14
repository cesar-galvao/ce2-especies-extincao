#pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)

#definindo diretório e baixando os dados 
getwd()
setwd("/Volumes/Arquivos/ce2-especies-extincao/data")
data <- read.csv("especies_2020_pronto.csv", fileEncoding = "Latin1")
data <- clean_names(data)

data[,2:15] <- map_df(data[, 2:15], as.character)
data[,2:15] <- map_df(data[, 2:15], str_squish)

#vendo o banco de dados
glimpse(data) 

#vendo os animais em extinção
ameaca <- data %>%
  filter(sigla_categoria_de_ameaca == "CR PEW"|
           sigla_categoria_de_ameaca == "EW"|
           sigla_categoria_de_ameaca == "CR PEX")

bioma_analise <- data %>%
  group_by(sigla_categoria_de_ameaca, bioma) %>%
  filter(sigla_categoria_de_ameaca == "CR PEW"|
           sigla_categoria_de_ameaca == "EW"|
           sigla_categoria_de_ameaca == "CR PEX") %>%
  distinct(sigla_categoria_de_ameaca, bioma = TRUE)
  count(bioma)

glimpse(data)

  