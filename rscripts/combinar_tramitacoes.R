library(data.table)
library(tidyverse)
library(lubridate)

# Combinar com as tramitações da Câmara
tram_df_camara <- fread('produced_data/tramitacoes_camara.csv')
tram_df_camara <- tram_df_camara %>% 
  mutate(DATA_TRAM = dmy(DATA_TRAM))                                                                                             

tram_df_senado <- fread('produced_data/tramitacoes_sf.csv')
tram_df_senado <- tram_df_senado %>% 
  select(NOM_PROPOSICAO, DES_ORGAO, ORDEM_TRAM, DATA_TRAM, DES_TRAM) %>% 
  mutate(DATA_TRAM = ymd(DATA_TRAM))

tram_df <- bind_rows(tram_df_camara, tram_df_senado)

tram_df <- tram_df %>% 
  arrange(NOM_PROPOSICAO, DATA_TRAM, ORDEM_TRAM)

write.csv(tram_df, 'produced_data/tramitacoes.csv', row.names = FALSE)
