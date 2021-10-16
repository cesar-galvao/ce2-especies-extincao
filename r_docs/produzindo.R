#pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)

#definindo diretório e baixando os dados 
data <- data <-readRDS("especies_2020_pronto.RDS") 

#vendo o banco de dados
glimpse(data) 


##### vendo a quantidade de animais em extinção ######
ameaca <- data %>%
  filter(sigla_categoria_de_ameaca == "CR PEW"|
           sigla_categoria_de_ameaca == "EW"|
           sigla_categoria_de_ameaca == "CR PEX") %>%
  group_by(sigla_categoria_de_ameaca, especie_simplificado, nome_comum) %>%
  count()

##### Quantidade de espécies em extinção por região/bioma ######
extinto <- c("CR PEW", "EW", "CR PEX") 

#transformando as siglas CR PEW, CR PEX E EX em uma só, "Extinto"
#para isso, usou-se a função recode. Copiou-se o df para uma variavel para não alterar a 
# o df original. O recode ele usa um vector como um argumento, e por isso usamos na coluna espeficica
#e não dentro do %>% 

bioma_analise <- data 
bioma_analise$sigla_categoria_de_ameaca <- recode(bioma_analise$sigla_categoria_de_ameaca,
                                                  "CR PEW" = "Extinto", "EW" = "Extinto", "CR PEX" = "Extinto")

bioma_analise_copia <- bioma_analise %>%
  filter(sigla_categoria_de_ameaca == "Extinto") %>% #filtrar quem tá extinto
  group_by(sigla_categoria_de_ameaca, bioma) %>% #agrupar por bioma
  count() #ver a quantidade de extintos por bioma

#Graus de risco por região/bioma 
grau_de_risco_bioma <- bioma_analise %>%
  group_by(bioma, sigla_categoria_de_ameaca) %>%
  count()

#explicação: pegou-se a função with e passou para os argumetnos o grau de risco e essa funçao opera
#sob o primeiro argumento, e a operacao escolhida foi order: primeiro em bioma e depois n. 
grau_de_risco_bioma[with(grau_de_risco_bioma, order(bioma, -n)),]

#Fatores de risco por região/bioma

fatores_de_risco <- bioma_analise %>%
  group_by(bioma, principais_ameacas) %>%
  count()

fatores_de_risco[with(fatores_de_risco, order(bioma, -n)),]

#vendo o maior grau de risco por bioma
fatores_de_risco %>%
  group_by(bioma) %>%
  top_n(1)

  