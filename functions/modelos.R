rm(list = ls())
library(lightgbm)
library(tidyverse)
library(ModelMetrics)
library(rBayesianOptimization)
library(data.table)

load('produced_data/treinamento.Rdata')

x <- dados_treinamento$x
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
                 scale_pos_weight = scale_pos_weight,
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
    
    f1_score[i] <- f1Score(y_val, pred)
    print(f1_score)
  }
  
  list(Score = mean(f1_score), Pred = 0)
}


# Validação ----------------------------------------------------------------

index <- list()
for(i in 1:6){
  data_fim <- datas[24 + i - 1]
  data_inicio <- datas[1 + i - 1]
  data_val <- datas[24 + i + 5]
  index[[i]] <- list()
  index[[i]]$train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
  index[[i]]$val <- which(y_df$data == data_val)
}

# Limites 
set.seed(49348)
bounds <- list(
  num_leaves = c(2L, 48L),
  learning_rate = c(0.1, 0.1),
  nrounds = c(100L, 200L),
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

# do.call(lgbm_fit_val, as.list(ba_search$Best_Par))
# lgbm_fit_val(learning_rate = 0.1,
#              num_leaves = 24,
#              feature_fraction = 0.1,
#              bagging_fraction = 1,
#              nrounds = 155)
#Round = 17	num_leaves = 24.0000	learning_rate = 0.1000	nrounds = 155.0000	bagging_fraction = 1.0000	feature_fraction = 0.1000	Value = 0.3880 

# Teste -------------------------------------------------------------------
index <- list()
for(i in 1:6){
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
  