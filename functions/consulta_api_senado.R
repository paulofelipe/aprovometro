library(tidyverse)
library(jsonlite)
library(httr)
library(progress)
library(stringr)

# Recupera proposições que foram remetidas ao Senado Federal
propostas_camara_no_sf <- function(ano_base = 2013){
  i <- 1
  tabela <- data.frame()
  
  while(TRUE){
    url <- paste0("http://legis.senado.gov.br/dadosabertos/materia/plc/", i,"/", ano_base)
    dados <- httr::GET(url)
    dados <- fromJSON(content(dados, type = "text", encoding = "UTF-8"))
    
    if(length(dados$DetalheMateria$Materia) == 0) break
    
    codigo <- as.numeric(dados$DetalheMateria$Materia$IdentificacaoMateria$CodigoMateria)
    numero <- as.numeric(dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$NumeroMateria)
    numero <- numero[1]
    ano <- as.numeric(dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$AnoMateria)
    ano <- ano[1]
    tipo <- dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$SiglaSubtipoMateria
    tipo <- tipo[1]
    
    if(!is.null(ano) & !is.null(numero) & !is.null(tipo)){
      data_tmp <- data.frame(tipo = tipo,
                             numero = numero,
                             ano = ano,
                             codigo_sf = codigo,
                             stringsAsFactors = FALSE)
      
      tabela <- bind_rows(tabela, data_tmp)
      rm(data_tmp)
    }
    i <- i + 1
  }
  
  i <- 1
  while(TRUE){
    url <- paste0("http://legis.senado.gov.br/dadosabertos/materia/pec/", i,"/", ano_base)
    dados <- httr::GET(url)
    dados <- fromJSON(content(dados, type = "text", encoding = "UTF-8"))
    
    if(length(dados$DetalheMateria$Materia) == 0) break
    
    codigo <- as.numeric(dados$DetalheMateria$Materia$IdentificacaoMateria$CodigoMateria)
    numero <- as.numeric(dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$NumeroMateria)
    numero <- numero[1]
    ano <- as.numeric(dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$AnoMateria)
    ano <- ano[1]
    tipo <- dados$DetalheMateria$Materia$OutrosNumerosDaMateria$OutroNumeroDaMateria$IdentificacaoMateria$SiglaSubtipoMateria
    tipo <- tipo[1]
    
    if(!is.null(ano) & !is.null(numero) & !is.null(tipo)){
      data_tmp <- data.frame(tipo = tipo,
                             numero = numero,
                             ano = ano,
                             codigo_sf = codigo,
                             stringsAsFactors = FALSE)
      
      tabela <- bind_rows(tabela, data_tmp)
      rm(data_tmp)
    }
    i <- i + 1
  }
  
  tabela
}

# Recupera tramitações de uma proposta
pega_tram_sf <- function(id){
  url <- paste0("http://legis.senado.leg.br/dadosabertos/materia/movimentacoes/", id)
  
  status_code <- 500
  while(status_code != 200){
    try(dados <- GET(url))
    status_code <- dados$status_code
  }
  
  dados <- fromJSON(content(dados, type = "text", encoding = "UTF-8"))
  dados <- dados$MovimentacaoMateria$Materia$Tramitacoes$Tramitacao$IdentificacaoTramitacao
  DES_ORGAO <- dados$OrigemTramitacao$Local$SiglaLocal
  dados <- dados %>% 
    select(DATA_TRAM = DataTramitacao,
           ORDEM_TRAM = CodigoTramitacao,
           NumeroOrdemTramitacao,
           DES_TRAM = TextoTramitacao) %>% 
    mutate(DES_ORGAO = paste0("SF - ", DES_ORGAO),
           CODIGO = id) %>% 
    arrange(DATA_TRAM, ORDEM_TRAM, NumeroOrdemTramitacao)
  dados
}

