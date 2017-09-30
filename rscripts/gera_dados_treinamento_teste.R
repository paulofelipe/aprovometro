rm(list = ls())

library(tidyverse)
library(data.table)
library(lubridate)

source('functions/processa_dados.R')

data_max <- fread('produced_data/tramitacoes.csv', select = c("DATA_TRAM"))
data_max <- data_max[,DATA_TRAM := ymd(DATA_TRAM)][,DATA_TRAM]
data_max  <- max(data_max)

dados_treinamento <- gera_base(data_inicio = data_max %m-% months(48),
                               data_fim = data_max)

save(dados_treinamento, file = 'produced_data/treinamento.Rdata')
