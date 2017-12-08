rm(list = ls())
library(lightgbm)
library(tidyverse)
library(ModelMetrics)
library(rBayesianOptimization)
library(data.table)
library(Matrix)

load('produced_data/treinamento.Rdata')

x <- dados_treinamento$x
# features <- c("tram_senado", "regime_Urgência", "tram_senado_federal", "tram_legislativa", 
#               "dias_tramitando", "ntram_90d", "tram_avulso", "tram_mensagem", 
#               "tram_publicado_avulso", "tram_tramitação_prioridade", "tram_recebido", 
#               "comissao_ccjc", "tram_rodrigo", "ntram_30d", "regime_Prioridade", 
#               "tram_ordinárias", "tram_tribunal", "tram_sistema", "tram_emendas", 
#               "tram_proferido", "regime_Ordinária", "tram_apreciação", "tram_senhor_presidente", 
#               "tram_tramitação", "tram_relator", "tram_câmara", "tram_ccjc_deputado", 
#               "tram_maia", "tram_apense", "tram_ccjc", "tram_senhor", "tram_urgência", 
#               "tram_presente_projeto", "tram_comissão", "tram_sclsf", "tram_senador", 
#               "tram_sobre", "tram_final", "tram_envio", "tram_projeto_apresentadas", 
#               "tram_redação_final", "tram_parecer_contra", "tram_lopes", 
#               "tram_magalhães", "tram_tramitação_ordinária", "tram_deputados", 
#               "tram_designado_relator", "tram_família_constituição", "tram_emendas_parecer", 
#               "tram_matéria", "tram_pág", "tram_despacho", "tram_devolvido", 
#               "tram_letra_designado", "comissao_sf", "tram_senhora", "tram_aprovado", 
#               "tram_outros", "tram_prazo", "tram_apensados", "comissao_cec", 
#               "tram_aprovação_projeto", "tram_julho", "tram_prazo_emendas", 
#               "tram_líder", "tram_criação", "tram_deputado", "tram_aprovação", 
#               "tram_ordem", "tram_devolvida", "tram_união", "tram_justiça", 
#               "tram_emendas_apresentação", "tram_prioridade_encaminhada", 
#               "tram_sanção", "tram_presidência", "tram_ricd", "tram_tramitação_conjunta", 
#               "tram_apresentadas", "tram_rogério", "tram_atendimento", "comissao_cvt", 
#               "tram_texto", "tram_dezembro", "tram_mérito", "tram_setembro", 
#               "tram_apreciação_conclusiva", "uf_rs", "tram_sessões", "tram_câmara_deputados", 
#               "tram_pauta", "tram_presidente_comissão", "tram_relatar", "tram_distribuição", 
#               "tram_retirado_pauta", "tram_termos", "tram_letra_prazo", "tram_recebimento", 
#               "tram_inicial_recebimento", "comissao_cft", "tram_pauta_ofício", 
#               "tram_iiproposição", "comissao_sf_-_ccj", "tram_vista_deputado", 
#               "tram_parecer_vencedor", "tram_ricd_proposição", "tram_comissões", 
#               "tram_designado", "tram_desenvolvimento", "tram_requerimento", 
#               "tram_requerimento_desarquivamento", "tram_secretário", "tram_paulo", 
#               "ntram", "uf_se", "tram_carlos", "tram_ronaldo", "tram_costa", 
#               "comissao_cdeic", "partido_pr", "tram_pública", "tram_sessão", 
#               "tram_dispõe", "tram_providências", "tram_esporte", "tram_pedido", 
#               "tram_apreciação_plenário", "tram_marcos", "tram_público_constituição", 
#               "tram_dias", "tram_redação", "comissao_sf_-_sexpe", "tram_lima", 
#               "tram_pmdb", "tram_retirado", "tram_alterações", "tram_ordinária", 
#               "tram_secretaria", "tram_proposições", "tram_ordinária_publicação", 
#               "comissao_mesa", "tram_favorável", "tram_requer_realização", 
#               "tram_trabalho", "tram_constitucionalidade_juridicidade", "tram_letra", 
#               "tram_após", "tram_serviço", "comissao_sf_-_cae", "tram_legislativa_deste", 
#               "tram_deliberação", "tram_couto", "tram_parecer_relatora", 
#               "tram_psdb", "tram_pros", "tram_risf", "tram_interno_câmara", 
#               "tram_cinco", "tram_educação_cultura", "tram_constante", "tram_tributação_mérito", 
#               "tram_casa", "tram_apresentada", "tram_luiz_carlos", "tram_vista_encerrado", 
#               "tram_apresentação_emendas", "tram_relatório", "tram_constituição", 
#               "tram_ofício", "tram_encerrado_aprovado", "tram_contra", "tram_cultura_publicado", 
#               "tram_distribuído_senador", "tram_regimental", "tram_suplemento", 
#               "tram_pcdob", "tram_deputado_luiz", "tram_aprovação_substitutivo", 
#               "tram_parecer", "tram_relatora", "tram_ricardo", "comissao_cssf", 
#               "tram_deputada", "tram_parecer_parecer", "tram_parecer_relator", 
#               "tram_rejeição", "tram_audiência_pública", "tram_federal", 
#               "tram_nelson", "tram_conformidade", "tram_filho_pmdb", "tram_conjunta", 
#               "tram_oliveira", "tram_juridicidade_técnica", "tram_campos", 
#               "tram_prazo_vista", "tram_remessa_senado", "tram_tramitar", "tram_recurso_termos", 
#               "tram_cabendo")
# x <- x[,features]
#colnames(x) <- paste0("var", 1:ncol(x))
y_df <- dados_treinamento$y
y <- dados_treinamento$y$APROVOU
rm(dados_treinamento); gc();

datas <- unique(y_df$data)

# Treina o algoritmo gbm (lightgbm)
lgbm_fit <- function(dtrain, 
                     learning_rate = 0.1,
                     num_leaves = 12,
                     feature_fraction = 0.5,
                     bagging_fraction = 0.4,
                     nrounds = 200,
                     scale_pos_weight){
  
  params <- list(objective = "binary",
                 learning_rate = learning_rate,
                 num_threads = 4,
                 num_leaves = num_leaves,
                 device = "cpu",
                 max_depth = -1,
                 feature_fraction = feature_fraction,
                 bagging_freq = 1,
                 bagging_fraction = bagging_fraction,
                 metric = "binary_logloss",
                 #scale_pos_weight = scale_pos_weight,
                 is_unbalance = TRUE,
                 verbosity = -1,
                 verbose = -1
  )
  
 
  fit <- lgb.train(params,
                   dtrain,
                   nrounds)
  
  return(fit)
  
}

lgbm_fit_val <- function(learning_rate,
                         num_leaves,
                         feature_fraction,
                         bagging_fraction,
                         nrounds){
  f1_score <- vector(mode = "numeric", length = length(index))
  for(i in 1:length(index)){
    x_train <- x[index[[i]]$train,]
    y_train <- y[index[[i]]$train]
    
    pos_weight <- sum(y_train == 0)/sum(y_train == 1)
    
    dtrain <- lgb.Dataset(x_train,
                          label = y_train)
    
    rm(x_train, y_train); gc();
    
    fit <- lgbm_fit(dtrain, 
                    scale_pos_weight = pos_weight,
                    learning_rate = learning_rate,
                    num_leaves = num_leaves,
                    feature_fraction = feature_fraction,
                    bagging_fraction = bagging_fraction,
                    nrounds = nrounds)
    
    x_val <- x[index[[i]]$val, ]
    y_val <- y[index[[i]]$val]
    pred <- predict(fit, x_val)
    
    f1_score[i] <- f1Score(y_val, pred, cutoff = 0.5)
    print(f1_score)
    print(precision(y_val, pred, cutoff = 0.5))
    print(recall(y_val, pred, cutoff = 0.5))
  }
  
  list(Score = mean(f1_score), Pred = 0)
}

lgbm_fit_val2 <- function(learning_rate,
                         num_leaves,
                         feature_fraction,
                         bagging_fraction,
                         nrounds){
  f1_score <- vector(mode = "numeric", length = length(index))
  precision_ <- vector(mode = "numeric", length = length(index))
  recall_ <- vector(mode = "numeric", length = length(index))
  predictions <- list(length = length(index))
  for(i in 1:length(index)){
    x_train <- x[index[[i]]$train,]
    y_train <- y[index[[i]]$train]
    
    pos_weight <- sum(y_train == 0)/sum(y_train == 1)
    
    dtrain <- lgb.Dataset(x_train,
                          label = y_train)
    
    rm(x_train, y_train); gc();
    
    fit <- lgbm_fit(dtrain, 
                    scale_pos_weight = pos_weight,
                    learning_rate = learning_rate,
                    num_leaves = num_leaves,
                    feature_fraction = feature_fraction,
                    bagging_fraction = bagging_fraction,
                    nrounds = nrounds)
    
    x_val <- x[index[[i]]$val, ]
    y_val <- y[index[[i]]$val]
    pred <- predict(fit, x_val)
    
    predictions[[i]] <- pred
    f1_score[i] <- f1Score(y_val, pred, cutoff = 0.5)
    precision_[i] <- precision(y_val, pred, cutoff = 0.5)
    recall_[i] <- recall(y_val, pred, cutoff = 0.5)
    print(f1_score)
    print(precision(y_val, pred, cutoff = 0.5))
    print(recall(y_val, pred, cutoff = 0.5))
  }
  
  list(f1_score = f1_score, precision = precision_, recall = recall_, 
       pred = predictions)
}

# Validação ----------------------------------------------------------------

index <- list()
for(i in 1:6){
  data_fim <- datas[24 + i - 1]
  data_inicio <- datas[1 + i - 1]
  data_val <- datas[24 + i + 5]
  print(data_val)
  index[[i]] <- list()
  index[[i]]$train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
  index[[i]]$val <- which(y_df$data == data_val)
}

# Limites 
set.seed(49348)
bounds <- list(
  num_leaves = c(2L, 48L),
  learning_rate = c(0.05, 0.1),
  nrounds = c(100L, 400L),
  bagging_fraction = c(0.1, 1),
  feature_fraction = c(0.1, 1)
)

ba_search <- BayesianOptimization(lgbm_fit_val,
                                  bounds = bounds,
                                  init_points = 10,
                                  n_iter = 20,
                                  acq = "ucb",
                                  kappa = 1,
                                  eps = 0.0,
                                  verbose = TRUE)

# ba_search <- list(Best_Par = c("num_leaves" = 17,
#                                "learning_rate" = 0.1,
#                                "nrounds" = 200,
#                                "bagging_fraction" = 0.9991,
#                                "feature_fraction" = 0.5259))
scores <- do.call(lgbm_fit_val2, as.list(ba_search$Best_Par))
scores$precision %>% mean
scores$recall %>% mean
# lgbm_fit_val(learning_rate = 0.1,
#              num_leaves = 24,
#              feature_fraction = 0.1,
#              bagging_fraction = 1,
#              nrounds = 155)
#num_leaves    learning_rate          nrounds bagging_fraction feature_fraction 
#37.0000000        0.1000000      112.0000000        1.0000000        0.1004679 

# Teste -------------------------------------------------------------------
index <- list()
for(i in 1:8){
  data_fim <- datas[30 + i - 1]
  data_inicio <- datas[7 + i - 1]
  data_val <- datas[30 + i + 5]
  index[[i]] <- list()
  index[[i]]$train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
  index[[i]]$val <- which(y_df$data == data_val)
}

do.call(lgbm_fit_val, as.list(ba_search$Best_Par))

# Modelo final ------------------------------------------------------------
data_inicio <- datas[20]
data_fim <- datas[43]
data_pred <- datas[49]

idx_final_train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
idx_final_pred <- which(y_df$data == data_pred)

dtrain <- lgb.Dataset(x[idx_final_train, ],
                      label = y[idx_final_train])
pos_weight <- sum(y[idx_final_train] == 0)/sum(y[idx_final_train] == 1)

fit <- lgbm_fit(dtrain,
                learning_rate = ba_search$Best_Par["learning_rate"],
                num_leaves = ba_search$Best_Par["num_leaves"],
                feature_fraction = ba_search$Best_Par["feature_fraction"],
                bagging_fraction = ba_search$Best_Par["bagging_fraction"],
                scale_pos_weight = pos_weight)

pred <- predict(fit, x[idx_final_pred, ])

y_final <- y_df[idx_final_pred, ] %>% 
  select(-APROVOU)

y_final$chance <- pred
y_final$velocidade <- x[idx_final_pred, "ntram_30d"]
y_final$qtd_tramitacoes <- x[idx_final_pred, "ntram"]

fwrite(y_final, 'produced_data/dados_chance.csv')
  