### PACOTES NECESSARIOS ----
library(tidyverse)
library(data.table)
library(janitor)

### LEITURA DO ARQUIVO ----

# ler o arquivo considerando as aspas como parte do texto, 
#cada obs é uma linha, uma única variável
data <- read.csv('./data/arquivos intermediarios/especies_2020.csv', sep = '\n', encoding = 'UTF-8', quote = "\"", skip = 1, header = FALSE)


### SIMPLIFICACAO DA GRAFIA ----

data2 <- data %>%
  mutate(#alterado = str_remove_all(V1, pattern = ","), #remove virgulas indesejadas
         simples = str_replace_all(V1, pattern = "\"", replacement = "\'"),
         #troca aspas duplas do texto por simples
         simples = str_replace_all(simples, ";", "; "), #padroniza espacos de ;
         simples = str_replace_all(simples, ":|/|\\(|\\)|-", " "), #remove simbolos indesejados
         simples = str_squish(simples)) %>% #padroniza quantidade de espacos e trim edges
  select(simples) #deixa so essa variavel

data2$simples <- iconv(data2$simples,from="UTF-8",to="ASCII//TRANSLIT")
#remove diacriticos
data2$novos <- NA



### SUBSTITUICAO DOS ; ESPECIFICOS POR , ----

for (i in 1:dim(data2[1])) {
  #extrai as strings no padrao e substitui ; por , em uma df nova
  repo <- str_extract_all(data2[i,1], pattern = "\'[a-zA-Z; ]+\'") %>% as.data.frame()
  repo[,1] <- str_replace_all(repo[,1], pattern = ";", replacement = ".")
  

  #cria uma df com marcadores no lugar da string extraída
  replace <- data2[i,]
  for (j in 1:dim(repo)[1]){
    replace <- str_replace(replace, pattern = "\'[a-zA-Z; ]+\'",
                replacement = paste("repo", as.character(j), sep = ""))
  }
  
  
  # pra cada linha de repo, ie intervalos a serem substituidos
  for(k in 1:dim(repo)[1]){ #substitua o termo "repo(j)" pela linha j de repo
    substituto <- repo[j,1]
    replace <- str_replace_all(replace, 
                               pattern = paste("repo", as.character(k), sep = ""),
                               replacement = repo[k,1])
  }
  data2[i,2] <- replace[1]
}

#cria uma nova variável com o texto final ----
data2$substituicoes <- data2[,1]
data2$substituicoes[which(is.na(data2[,2]) == FALSE)] <- data2[which(is.na(data2[,2]) == FALSE),2]

#adquire nomes das variaveis do banco final ----
nomes <- read.csv('./data/arquivos intermediarios/especies_2020.csv', sep = ';', encoding = 'UTF-8', header = FALSE, nrows = 1)
nomes <- as.character(nomes[1,]) %>%
  str_remove_all(pattern = ",")%>%
  str_trim()

#cria banco final para povoamento ----
especies_2020_limpo <- data.frame(subs = data2$substituicoes) %>%
  separate(subs, into = nomes, sep = ";")

#ajustamento do 1 caso que nao se ajusta ----
cobra <- "Fauna; Repteis; Dipsadidae; Hydrodynastes melanogigas; Cobra dagua grande do tocantins;
Em Perigo EN; EN; Cerrado; 'Assentamento Humano cidades, Outras Atividades Economicas Energia'; Sim; 
Sim; Nao; 2; Sim; Informacao nao disponivel"

cobra_df <- data.frame(subs = cobra) %>%
  separate(subs, into = nomes, sep = ";")

#insercao do caso final ----
especies_2020_limpo[1436,] <- cobra_df[1,]

#remocao final de aspas simples ----
especies_2020_pronto <- map_df(especies_2020_limpo, str_remove_all, pattern = "\\'")

especies_2020_pronto <- map_df(especies_2020_pronto, as.character)
especies_2020_pronto <- map_df(especies_2020_pronto, str_squish)

#producao de arquivo final rda e csv ----

final <- especies_2020_pronto %>% #edixar explicitas as separacoes
  clean_names()%>%
  mutate(estados_de_ocorrencia = str_remove_all(estados_de_ocorrencia, pattern = "[:punct:]")) %>%
  separate_rows(bioma, sep = ", ") %>% 
  separate_rows(bioma, sep = " , ") %>%
  separate_rows(bioma, sep = ". ") %>%
  separate_rows(principais_ameacas, sep = ", ") %>% 
  separate_rows(principais_ameacas, sep = " , ") %>%
  separate_rows(principais_ameacas, sep = "\\. ") %>%
  separate_rows(estados_de_ocorrencia, sep = " ")


# outras atividades economicas: remover termo, dividir linhas

saveRDS(final, './data/especies_2020_pronto.RDS')
write.csv(final, './data/especies_2020_pronto.csv')


rm(cobra_df, data, data2, especies_2020_limpo, especies_2020_pronto, repo, cobra,
   i, j, k, nomes, replace, substituto)

