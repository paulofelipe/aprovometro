rm(list = ls())
library(tidyverse)
library(progress)
library(data.table)
source('functions/consulta_api_camara.R')

# dados <- read.csv2('http://www.camara.gov.br/internet/arquivosDadosAbertos/proposicoes.csv',
#                    stringsAsFactors = FALSE)
# 
# dados <- dados %>% 
#   select(NOM_PROPOSICAO, NOM_PARTIDO_POLITICO, DES_SITUACAO_PROPOSICAO, ANO_PROPOSICAO,
#          NUM_PROPOSICAO, SIG_TIPO_PROPOSICAO, SIG_NORMA_JURIDICA, 
#          DATAPRESENTACAOPROPOSICAO, DATTRANSFPROPOSICAOLEI, 
#          SIG_UF, TEX_REGIAO_GEOGRAFICA_AUTOR, AREAS_TEMATICAS_APRESENTACAO, COD_SEXO) %>% 
#   filter(SIG_TIPO_PROPOSICAO %in% c('PEC','PLP','PL'), ANO_PROPOSICAO >= 2003) %>% 
#   mutate(NOM_PROPOSICAO = paste0(SIG_TIPO_PROPOSICAO, ' ', NUM_PROPOSICAO, '/', ANO_PROPOSICAO))
dados <- fread('raw_data/propostas_detalhes.csv')
# Nomes das proposicoes
proposicoes <- dados$NOM_PROPOSICAO

# Cria barra de progresso
barra <- progress_bar$new(total = length(proposicoes),
                          format = "[:bar] :percent eta: :eta")

lista_proposicoes <- map(proposicoes, 
                         ~{
                           barra$tick()
                           pega_dados_pl(.x)
                           })

# Tramitacoes
dados_tram <- map(lista_proposicoes, ~ .x$tramitacoes) %>% 
  bind_rows()

# Ementas
dados_ementa <- map(lista_proposicoes, ~ .x$ementa) %>% 
  bind_rows()

# Situação
dados_situacao <- map(lista_proposicoes, ~ .x$situacao) %>% 
  bind_rows()


# Escrever Arquivos -------------------------------------------------------

# Propostas
write.csv(dados, 'produced_data/propostas.csv')

# Tramitações
write.csv(dados_tram, 'produced_data/tramitacoes_camara.csv', row.names = FALSE)

# Ementas
write.csv(dados_ementa, 'produced_data/ementas.csv', row.names = FALSE)

# Situção
write.csv(dados_situacao, 'produced_data/situacao.csv', row.names = FALSE)