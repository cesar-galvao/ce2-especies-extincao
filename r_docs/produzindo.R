#pacotes
library(tidyverse)
library(dplyr)
library(ggplot2)
library(janitor)

#definindo diretório e baixando os dados 
data <-readRDS("./data/especies_2020_pronto.RDS")%>%
  filter(fauna_flora != "") #tinha uma linha toda vazia, mandei remover

#vendo o banco de dados
glimpse(data) 


##### vendo a quantidade de animais em extinção ######
ameaca <- data %>%
  filter(sigla_categoria_de_ameaca == "CR PEW"|
           sigla_categoria_de_ameaca == "EW"|
           sigla_categoria_de_ameaca == "CR PEX") %>%
  group_by(sigla_categoria_de_ameaca, especie_simplificado, nome_comum, especie_exclusiva_do_brasil) %>%
  count()

#-----------------------#

#Aqui ocorreu a contagem de quantas vezes uma espécie aparece no nosso banco
#que ja tem as espécies aparecendo múltiplas vezes
#Minha sugestão, contando todas as categorias:

ameaca <- data %>%
  distinct(sigla_categoria_de_ameaca, especie_simplificado)%>%
  group_by(sigla_categoria_de_ameaca)%>%
  count()

#-----------------------#



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
  group_by(sigla_categoria_de_ameaca, bioma, especie_simplificado) %>% #agrupar por bioma
  distinct(sigla_categoria_de_ameaca, especie_simplificado)%>%
  ungroup()%>%
  group_by(sigla_categoria_de_ameaca, bioma)%>%
  count() #ver a quantidade de extintos por bioma

bioma_analise_copia %>%
  ggplot(aes(x = bioma, y = n))+
  geom_bar(stat = 'identity', fill = '#3c8dbc')+
  labs(x = "", y = "")+
  ylim(0,65)+
  geom_text(aes(label = n), vjust = -.5)+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank())


#-----------------------#

# Como conversamos, nao acho ideal tratarmos tudo como extinto, já que são categorias
# diferentes. Acho que podemos deixar com os nomes que realmente têm.
# Achei legal o uso do recode, eu nao conhecia e vou começar a usar.
# 
# Acho que a melhor abordagem é parecida com a que vc usou abaixo, que é tb
# parecida com a primeira que eu sugeri.

grau_de_risco_bioma <- data %>%
  distinct(bioma, sigla_categoria_de_ameaca, especie_simplificado)%>%
  group_by(bioma, sigla_categoria_de_ameaca)%>%
  count() %>%
  arrange(bioma, desc(n))

# Consegue pensar em uma sugestão de visualização pra isso?

#-----------------------#



#Graus de risco por região/bioma 
grau_de_risco_bioma <- bioma_analise %>%
  group_by(bioma, sigla_categoria_de_ameaca) %>%
  count()

#explicação: pegou-se a função with e passou para os argumetnos o grau de risco e essa funçao opera
#sob o primeiro argumento, e a operacao escolhida foi order: primeiro em bioma e depois n. 
grau_de_risco_bioma[with(grau_de_risco_bioma, order(bioma, -n)),]



#-----------------------#
# Acho que o mesmo tipo de processamento pode ser feito para os fatores de risco.
# O problema é processar como se cada linha do nosso banco fosse uma observação,
# isso é, uma espécie. No nosso caso, observação nao é equivalente a uma espécie
# única, mas no banco original sim.
#-----------------------#


#Fatores de risco por região/bioma

fatores_de_risco <- bioma_analise %>%
  group_by(bioma, principais_ameacas) %>%
  count()

fatores_de_risco[with(fatores_de_risco, order(bioma, -n)),]

#vendo o maior grau de risco por bioma
fatores_de_risco %>%
  group_by(bioma) %>%
  top_n(1)



nivel_de_protecao <- bioma_analise %>%
  group_by(bioma, nivel_de_protecao_na_estrategia_nacional) %>%
  count()

nivel_de_protecao %>%
  group_by(bioma) %>%
  top_n(1)

  