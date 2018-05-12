library(tidyverse)
library(httr)
library(XML)
library(stringr)

sigla <- "PL"
numero <- 1037
ano <- 2011
url <- paste0('www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterAndamento?sigla=',sigla,
              '&numero=',numero,
              '&ano=',ano,
              '&dataIni=&codOrgao=')
teste <- GET(url)

teste %>% 
  content(type = "text", encoding = "UTF-8") %>% 
  htmlParse() %>% 
  xpathSApply("//ultimaacao/tramitacao") %>% 
  xmlToDataFrame() 

# Função para recuperar tramitações para uma PL
pega_tram <- function(data_url){
  x <- data_url %>% 
    content(type = "text", encoding = "UTF-8") %>% 
    htmlParse() %>% 
    xpathSApply("//*/andamento/tramitacao") %>% 
    xmlToDataFrame() 
  if(nrow(x) > 0){
    x <- x %>% 
    select(DES_ORGAO = orgao,
           ORDEM_TRAM = ordemdetramitacao, DATA_TRAM = data,
           DES_TRAM = descricao) %>% 
      mutate_if(is.factor, as.character)
  }
  
  y <- data_url %>% 
    content(type = "text", encoding = "UTF-8") %>% 
    htmlParse() %>% 
    xpathSApply("//ultimaacao/tramitacao") %>% 
    xmlToDataFrame() %>% 
    select(DES_ORGAO = orgao,
           ORDEM_TRAM = ordemdetramitacao, DATA_TRAM = data,
           DES_TRAM = descricao) %>% 
    mutate_if(is.factor, as.character)
    x <- bind_rows(x, y) %>% 
      arrange(ORDEM_TRAM)
}

# Função para recuperar a ementa da PL
pega_ementa <- function(data_url){
  
  data_url %>% 
    content(type = "text", encoding = "UTF-8") %>% 
    htmlParse() %>% 
    xpathSApply("//ementa") %>% 
    xmlToDataFrame() %>% 
    rename(ementa = text) %>% 
    mutate_if(is.factor, as.character)
}

# Função para recuper a ultima ação

pega_situacao <- function(data_url){
  
  data_url %>% 
    content(type = "text", encoding = "UTF-8") %>% 
    htmlParse() %>% 
    xpathSApply("//*/situacao") %>% 
    xmlToDataFrame() %>% 
    rename(ultima_situacao = text) %>% 
    mutate_if(is.factor, as.character)
}

pega_dados_pl <- function(NOM_PROPOSICAO){
  Sys.sleep(0.001)
  tipo <- str_split(NOM_PROPOSICAO, " ", simplify = TRUE)[,1]
  numero <- str_extract_all(NOM_PROPOSICAO, "[0-9]{1,}", simplify = TRUE)[,1]
  ano <- str_extract_all(NOM_PROPOSICAO, "[0-9]{1,}", simplify = TRUE)[,2]
  
  url <- paste0('www.camara.leg.br/SitCamaraWS/Orgaos.asmx/ObterAndamento?sigla=',tipo,
                '&numero=',numero,
                '&ano=',ano,
                '&dataIni=&codOrgao=')
  status_code <- 500
  while(status_code != 200){
    try({
      data_pl <- GET(url)
      status_code <- data_pl$status_code
    })
  }
  
  
  tramitacoes <- pega_tram(data_pl) %>% 
    mutate(NOM_PROPOSICAO = NOM_PROPOSICAO) %>% 
    select(NOM_PROPOSICAO, everything())
  
  ementa <- pega_ementa(data_pl) %>% 
    mutate(NOM_PROPOSICAO = NOM_PROPOSICAO) %>% 
    select(NOM_PROPOSICAO, everything())
  
  situacao <- pega_situacao(data_pl) %>%
    mutate(NOM_PROPOSICAO = NOM_PROPOSICAO) %>% 
    select(NOM_PROPOSICAO, everything())
    
  return(list(tramitacoes = tramitacoes,
              ementa = ementa,
              situacao = situacao))
}
