rm(list = ls())

library(tidyverse)
library(jsonlite)
library(httr)
library(progress)
library(stringr)
library(data.table)
library(lubridate)

source('functions/consulta_api_senado.R')

# Recupera os ids das propostas enviadas ao Senado Federal
anos <- 2003:2017
barra <- progress_bar$new(
  format = "[:bar] :percent eta: :eta",
  total = length(anos)
)
ids_proposicoes_camara_no_sf <- map(anos, 
                                    ~{
                                      barra$tick()
                                      propostas_camara_no_sf(.x)
                                    })

ids_proposicoes_camara_no_sf <- bind_rows(ids_proposicoes_camara_no_sf)      

ids_proposicoes_camara_no_sf <- ids_proposicoes_camara_no_sf %>% 
  mutate(tipo = str_extract(tipo, "PLP|PL|PEC")) %>%
  na.omit()

# Recupera tramitações no Senado Federal
codigos_sf <- ids_proposicoes_camara_no_sf$codigo_sf
barra <- progress_bar$new(
  format = "[:bar] :percent eta: :elapsed",
  total = length(codigos_sf)
)

tram_df_sf <- map(codigos_sf,
                  ~{
                    barra$tick()
                    pega_tram_sf(.x)
                  })

tram_df_sf <- bind_rows(tram_df_sf)
head(tram_df_sf)

ids_proposicoes_camara_no_sf <- ids_proposicoes_camara_no_sf %>% 
  mutate(NOM_PROPOSICAO = paste0(tipo, ' ', numero, '/', ano))

tram_df_sf <- ids_proposicoes_camara_no_sf %>% 
  filter(ano >= 2003) %>% 
  dplyr::select(5, 4) %>% 
  left_join(., tram_df_sf, by = c("codigo_sf" = "CODIGO"))

write.csv(tram_df_sf, 'produced_data/tramitacoes_sf.csv', row.names = FALSE)


