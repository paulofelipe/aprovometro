rm(list = ls())

start <- Sys.time()

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(stringr)
library(jsonlite)
library(data.table)


# Funções -----------------------------------------------------------------
firstup <- function(x) {
  
  x <- ifelse(nchar(x) > 1,
              paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x)))),
              tolower(x))  
  x
}

corrige_temas <- function(x) {
  y <- str_split(x, pattern = " ")
  y <- lapply(y, firstup)
  lapply(y, paste, collapse = " ") %>% unlist() %>% str_trim()
}

# Dados propostas ----------------------------------------------------
dados_propostas <- fread('produced_data/propostas.csv')

# Dados tramitações --------------------------------------------------
dados_tramitacoes <- fread('produced_data/tramitacoes.csv')

# Dados com predições ------------------------------------------------
chance_final <- fread('produced_data/dados_chance.csv')

# Demais dados
dados_area_tematica_relacional <- dados_propostas %>% 
  select(NOM_PROPOSICAO, AREAS_TEMATICAS_APRESENTACAO) %>% 
  mutate(tema = str_replace(AREAS_TEMATICAS_APRESENTACAO, ', ', '='),
         tema = str_split(tema, ",")) %>% 
  unnest(tema) %>%
  mutate(tema = str_replace(tema, '=', ', ')) %>% 
  distinct() %>% 
  select(NOM_PROPOSICAO, tema)

dados_comissoes_relacional <- dados_tramitacoes %>% 
  mutate(COMISSAO = word(DES_ORGAO)) %>% 
  select(NOM_PROPOSICAO, COMISSAO, DES_ORGAO) %>% 
  mutate(TIPO = ifelse(startsWith(COMISSAO, 'P'), 1, 0), #COMISSOES ESPECIAIS
         TIPO = ifelse(COMISSAO == 'PLEN', 0, TIPO), 
         TIPO = ifelse(COMISSAO %in% c('MPV00101', 'GTFICHA', 'GTTAXI', 'CPIPETRO'), 1, TIPO), #COMISSOES ESPECIAIS
         COMISSAO = ifelse(TIPO == 1, 'ESPECIAL', COMISSAO)) %>% #COMISSOES ESPECIAIS
  select(NOM_PROPOSICAO, COMISSAO) %>% 
  distinct()

proposicao <- dados_propostas %>% 
  select(NOM_PROPOSICAO, SIG_TIPO_PROPOSICAO, ANO_PROPOSICAO, NUM_PROPOSICAO, DATAPRESENTACAOPROPOSICAO) %>% 
  distinct()

proposicao <- left_join(chance_final, proposicao)


# Alterar nomes das colunas -----------------------------------------------
proposicao <- proposicao %>% 
  mutate(tipoImg = paste0("assets/img/", tolower(SIG_TIPO_PROPOSICAO), ".png")) %>% 
  select(siglaTipo = SIG_TIPO_PROPOSICAO,
         tipoImg,
         numero = NUM_PROPOSICAO,
         ano = ANO_PROPOSICAO,
         dataApres = DATAPRESENTACAOPROPOSICAO,
         chance,
         qtd_tramitacoes,
         velocidade,
         NOM_PROPOSICAO)

dados_comissoes_relacional <- dados_comissoes_relacional %>% 
  rename(comissao = COMISSAO)

dados_area_tematica_relacional <- dados_area_tematica_relacional %>% 
  mutate(tema = corrige_temas(tema))

head(proposicao)
head(dados_comissoes_relacional)
head(dados_area_tematica_relacional)


proposicoes <- unique(proposicao$NOM_PROPOSICAO)

lista_tema <- vector("list", length(proposicoes))
lista_comissao <- vector("list", length(proposicoes))
j <- 1
for(i in proposicoes){
  print(j)
  lista_tema[[j]] <- dados_area_tematica_relacional %>% 
    filter(NOM_PROPOSICAO == i) %>% 
    select(tema)
  
  lista_comissao[[j]] <- dados_comissoes_relacional %>% 
    filter(NOM_PROPOSICAO == i) %>% 
    select(comissao)
  
  j <- j + 1
}


proposicao %>%
  tbl_df %>% 
  add_column(temas = lista_tema,
             comissoes = lista_comissao) %>% 
  arrange(-chance) %>% 
  head() %>% 
  select(siglaTipo, tipoImg, numero, ano, temas, everything(), -NOM_PROPOSICAO) %>% 
  toJSON(pretty = FALSE) %>% 
  writeLines('produced_data/aprovometro.json')

end <- Sys.time()

end - start
