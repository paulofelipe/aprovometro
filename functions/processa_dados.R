rm(list = ls())
options(na.action='na.pass')

library(data.table)
library(tidyverse)
library(text2vec)
library(lubridate)
library(progress)
library(Matrix)
library(stringr)
library(tm)


# Seleciona propostas que possuem tramitações -----------------------------
prop_df <- fread('produced_data/propostas.csv')
propostas1 <- unique(prop_df$NOM_PROPOSICAO)

tram_df <- fread('produced_data/tramitacoes.csv')
propostas2 <- unique(tram_df$NOM_PROPOSICAO)

propostas <- intersect(propostas1, propostas2)

tram_df <- tram_df %>% 
  filter(NOM_PROPOSICAO %in% propostas) %>% 
  as.data.table()

prop_df <- prop_df %>% 
  filter(NOM_PROPOSICAO %in% propostas) %>% 
  as.data.table()

rm(propostas, propostas1, propostas2); gc();


# Variáveis relacionadas às tramitações -----------------------------------

tram_df <- tram_df %>% 
  mutate(APROVADA = ifelse(grepl(pattern = '^Transformado n', DES_TRAM), 1, 0),
         ARQUIVADA = ifelse(grepl(pattern = '^Arquiva|^ARQUIVA|Declarado prejudicad',
                                  DES_TRAM), 1, 0),
         APENSADA = ifelse(grepl(pattern = 'apensado ao PL|Apense-se ao P|Apense-se à',
                                      DES_TRAM), 1, 0),
         DESARQUIVADA = ifelse(grepl('^Desarquivad', DES_TRAM), 1, 0),
         DESAPENSADA = ifelse(grepl(pattern = "Desapensação automática des",
                                         DES_TRAM), 1, 0))

# Stop words --------------------------------------------------------------

stop_words <- stopwords('pt')

# Cria o dicionário para tramitações --------------------------------------

dic_tram <-tram_df %>% 
  filter(DATA_TRAM <= "2015-12-31") %>% 
  group_by(NOM_PROPOSICAO) %>% 
  summarise(DES_TRAM = paste0(DES_TRAM, collapse = " ")) %>% 
  arrange(NOM_PROPOSICAO) %>% 
  pull(DES_TRAM) %>%
  str_replace_all(., pattern = "[0-9]", replacement = "") %>%
  str_replace_all(., pattern = "º|ª", replacement = "") %>%
  str_replace_all(., pattern = "\\b[a-zA-Z]{1,3}\\b", replacement = "") %>%
  itoken(.,
         preprocessor = tolower,
         tokenizer = word_tokenizer, 
         progressbar = TRUE)

tram_vocab <- create_vocabulary(dic_tram, stopwords = stop_words,
                                ngram = c(ngram_min = 1L,
                                          ngram_max = 1L))

pruned_vocab  <- prune_vocabulary(tram_vocab,
                                  vocab_term_max = 1000,
                                  doc_proportion_max = 0.5,
                                  doc_proportion_min = 0.01)

vectorizer <- vocab_vectorizer(pruned_vocab)

dtm_tram <- create_dtm(dic_tram, vectorizer)
tfidf = TfIdf$new()
dtm_tram <- fit_transform(dtm_tram, tfidf)

# Vetor de temas ---------------------------------------------------------
temas <- prop_df %>% 
  pull(AREAS_TEMATICAS_APRESENTACAO)
temas <- str_split(sort(unique(temas)),
                   pattern = "(,(?=\\S)|:)") %>%
  unlist() %>%
  unique()

# Vetor de tipos de proposicao ------------------------------------------
tipo_prop <- prop_df %>% 
  pull(SIG_TIPO_PROPOSICAO) %>% 
  unique

# Vetor de comissões ----------------------------------------------------
comissoes <- tram_df %>% 
  pull(DES_ORGAO) %>% 
  str_extract(pattern = "(?:^|(?:[.!?]\\s))(\\w+)") %>% 
  table %>% 
  sort(decreasing = TRUE) %>% 
  names %>% .[1:32]

comissoes <- tram_df %>% 
  filter(str_detect(DES_ORGAO, "^SF")) %>% 
  pull(DES_ORGAO) %>% 
  unique() %>% 
  c(., comissoes) %>% 
  unique()

# Vetor de partidos ------------------------------------------------------
partidos <- prop_df %>% 
  pull(NOM_PARTIDO_POLITICO) %>% 
  unique %>% 
  sort

# Vetor de UFs -----------------------------------------------------------
ufs <- prop_df %>% 
  pull(SIG_UF) %>% 
  unique %>% 
  sort

# Função para criar dummies ----------------------------------------------------
cria_dummies <- function(data, levels, prefix = ""){
  vetores <- map(levels, ~ as(str_detect(data, pattern = .x) * 1, "sparseMatrix"))
  matriz <- do.call(cBind, vetores)
  colnames(matriz) <- paste0(prefix, levels)
  matriz
}

# Função para processamento dos dados -------------------------------------

processa_dados <- function(data, meses = 6){
  data <- as.Date(data)
  data_meses <- data %m+% months(meses)
  
  # Cria base inicial
  dados_data_base <- tram_df %>% 
    filter(DATA_TRAM <= data) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(ARQUIVADA = ifelse(max(ARQUIVADA * ORDEM_TRAM) >
                                max(DESARQUIVADA * ORDEM_TRAM),
                              1, 0),
           APENSADA = ifelse(max(APENSADA * ORDEM_TRAM) >
                               max(DESAPENSADA * ORDEM_TRAM),
                             1, 0),
           STATUS_REAL = sum(APROVADA + ARQUIVADA + APENSADA, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(STATUS_REAL == 0) %>% 
    select(NOM_PROPOSICAO) %>% 
    distinct() %>% 
    mutate(data = data)
  
  dados_data_posterior <- tram_df %>% 
    filter(DATA_TRAM > data & DATA_TRAM <= data_meses) %>% 
    filter(APROVADA == 1) %>% 
    rename(APROVOU = APROVADA) %>% 
    select(NOM_PROPOSICAO, APROVOU)
  
  dados_treino <- left_join(dados_data_base, dados_data_posterior,
                            by = "NOM_PROPOSICAO") %>% 
    replace_na(list(APROVOU = 0)) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    mutate(APROVOU = max(APROVOU))
  
  # Número de tramitações nos últimos 30 dias
  ntram_30d <- tram_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    mutate(tram_30d = (DATA_TRAM >= data - 30  & DATA_TRAM <= data) * 1) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(ntram_30d = sum(tram_30d)) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    select(-NOM_PROPOSICAO) %>% 
    as.data.frame() %>% 
    data.matrix() %>% 
    as(., "sparseMatrix") 
  
  # Número de tramitações nos últimos 90 dias
  ntram_90d <- tram_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    mutate(tram_90d = (DATA_TRAM >= data - 90  & DATA_TRAM <= data) * 1) %>%
    group_by(NOM_PROPOSICAO) %>% 
    summarise(ntram_90d = sum(tram_90d)) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    select(-NOM_PROPOSICAO) %>% 
    as.data.frame() %>% 
    data.matrix() %>% 
    as(., "sparseMatrix") 
  
  # Tempo de tramitação em dias
  tram_dias <- prop_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    select(NOM_PROPOSICAO, DATAPRESENTACAOPROPOSICAO) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    mutate(dias_tramitando = as.numeric(as.Date(data) -
                                          dmy(DATAPRESENTACAOPROPOSICAO))) %>% 
    select(dias_tramitando) %>% 
    as.data.frame() %>% 
    data.matrix() %>% 
    as(., "sparseMatrix") 
  
  # Número de tramitações
  ntram <- tram_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    filter(DATA_TRAM <= data) %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(ntram = n()) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    select(ntram) %>% 
    as.data.frame() %>% 
    data.matrix() %>% 
    as(., "sparseMatrix") 
  
  # Dummies para temas
  dummies_temas <- prop_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(AREAS_TEMATICAS_APRESENTACAO) %>% 
    cria_dummies(data = ., levels = temas, prefix = "tema_")
  
  colnames(dummies_temas) <- str_replace_all(colnames(dummies_temas),
                                             " ", "_") %>% 
    tolower()
  
  # Dummies para tipo de proposicao
  dummies_tipo_prop <- prop_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(SIG_TIPO_PROPOSICAO) %>% 
    cria_dummies(data = ., levels = tipo_prop, prefix = "tipo_")
  
  colnames(dummies_tipo_prop) <- str_replace_all(colnames(dummies_tipo_prop),
                                             " ", "_") %>% 
    tolower()
  
  # Dummies para UF
  dummies_ufs <- prop_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(SIG_UF) %>% 
    cria_dummies(data = ., levels = ufs, prefix = "uf_")
  
  colnames(dummies_ufs) <- str_replace_all(colnames(dummies_ufs),
                                          " ", "_") %>% 
    tolower()
  
  # Dummies para partidos
  dummies_partidos <- prop_df %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(NOM_PARTIDO_POLITICO) %>% 
    cria_dummies(data = ., levels = partidos, prefix = "partido_")
  
  colnames(dummies_partidos) <- str_replace_all(colnames(dummies_partidos),
                                           " ", "_") %>% 
    tolower()
  
  # Contagem para comissões
  comissoes_tmp <- tram_df %>%
    filter(DATA_TRAM <= data) %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(DES_ORGAO = paste0(DES_ORGAO, collapse = " ")) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(DES_ORGAO)
  
  comissoes_df <- list()
  for(i in comissoes){
    comissoes_df[[i]] <- as(str_count(comissoes_tmp, i),
                            "sparseVector")
  }
  comissoes_df <- lapply(comissoes_df, as, "sparseMatrix")
  comissoes_df <- do.call(cBind, comissoes_df)
  colnames(comissoes_df) <- paste0("comissao_", comissoes) %>% 
    str_replace_all(., " ", "_") %>% 
    tolower()
  
  # Bag-of-Words/TFIdf para as tramitações
  dtm_tram_train <- tram_df %>% 
    filter(DATA_TRAM <= data) %>% 
    inner_join(dados_data_base, by = "NOM_PROPOSICAO") %>% 
    group_by(NOM_PROPOSICAO) %>% 
    summarise(DES_TRAM = paste0(DES_TRAM, collapse = " ")) %>% 
    arrange(NOM_PROPOSICAO) %>% 
    pull(DES_TRAM) %>% 
    tolower() %>% 
    word_tokenizer() %>% 
    itoken(progressbar = FALSE) %>% 
    create_dtm(., vectorizer) %>% 
    transform(tfidf)
  
  colnames(dtm_tram_train) <- paste0("tram_", colnames(dtm_tram_train))
  
  # Consolida as features
  x <- cBind(ntram, ntram_30d, ntram_90d, tram_dias, dummies_tipo_prop, dummies_temas,
             dummies_ufs, dummies_partidos, comissoes_df, dtm_tram_train)
  return(list(y = dados_treino, x = x))
}
  
gera_base <- function(data_inicio, data_fim){
  data_seq <- seq(ymd(data_inicio), ymd(data_fim), by = '1 month')
  
  barra <- progress_bar$new(total = length(data_seq))
  
  base <- map(data_seq, ~{
    barra$tick()
    processa_dados(data = .x)
    })
  
  y <- map(base, ~.x$y) %>% 
    bind_rows()
  
  x <- map(base, ~.x$x) %>% 
    do.call(rBind, .)
  
  return(list(y = y, x = x))
}

# teste <- processa_dados(data = "2016-04-30")
# dim(teste$x)

