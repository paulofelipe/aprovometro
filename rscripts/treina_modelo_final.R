rm(list = ls())
library(lightgbm)
library(tidyverse)
library(ModelMetrics)
library(rBayesianOptimization)
library(data.table)
library(Matrix)

load('produced_data/treinamento.Rdata')

x <- dados_treinamento$x
y_df <- dados_treinamento$y
y <- dados_treinamento$y$APROVOU
rm(dados_treinamento); gc();

datas <- unique(y_df$data)

index <- list()
for(i in 1:8){
  data_fim <- datas[30 + i - 1]
  data_inicio <- datas[7 + i - 1]
  data_val <- datas[30 + i + 5]
  index[[i]] <- list()
  index[[i]]$train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
  index[[i]]$val <- which(y_df$data == data_val)
}

f1_score <- vector(mode = "numeric", length = 8L)
best_iter <- vector(mode = "numeric", length = 8L)
for(i in 1:length(f1_score)){
  
  x_train <- x[index[[i]]$train,]
  y_train <- y[index[[i]]$train]
  dtrain <- lgb.Dataset(x_train,
                        label = y_train)
  
  x_val <- x[index[[i]]$val, ]
  y_val <- y[index[[i]]$val]
  dval <- lgb.Dataset(x_val,
                      label = y_val)
  
  
  pos_weight <- sum(y_train == 0)/sum(y_train == 1)
  #pos_weight <- 191.5231
  th <- .85
  
  rm(x_train, y_train, x_val, y_val); gc();
  
  params <- list(objective = "binary",
                 learning_rate = 0.01,
                 num_threads = 4,
                 num_leaves = 2^3- 1,
                 device = "cpu",
                 max_depth = -1,
                 feature_fraction = 0.8,
                 bagging_fraction = 0.3,
                 metric = "auc",
                 lambda_l1 = 0.1,
                 lambda_l2 = 0.1,
                 #scale_pos_weight = 10,
                 is_unbalance = TRUE,
                 verbosity = -1,
                 verbose = -1
  )
  
  fit <- lgb.train(params,
                   dtrain,
                   valids = list(valid = dval),
                   nrounds = 10000,
                   early_stopping_rounds = 100,
                   eval_freq = 50)
  
  
  x_val <- x[index[[i]]$val, ]
  y_val <- y[index[[i]]$val]
  pred <- predict(fit, x_val, num_iteration = fit$best_iter)
  
  f1_score[i] <- ModelMetrics::f1Score(y_val, pred, cutoff = th)  
  best_iter[i] <- fit$best_iter
}

# Modelo final ------------------------------------------------------------
data_inicio <- datas[20]
data_fim <- datas[43]
data_pred <- datas[49]

idx_final_train <- which(y_df$data <= data_fim & y_df$data >= data_inicio)
idx_final_pred <- which(y_df$data == data_pred)

dtrain <- lgb.Dataset(x[idx_final_train, ],
                      label = y[idx_final_train])
pos_weight <- sum(y[idx_final_train] == 0)/sum(y[idx_final_train] == 1)

params <- list(objective = "binary",
               learning_rate = 0.01,
               num_threads = 4,
               num_leaves = 2^3- 1,
               device = "cpu",
               max_depth = -1,
               feature_fraction = 0.8,
               bagging_fraction = 0.3,
               metric = "auc",
               lambda_l1 = 0.1,
               lambda_l2 = 0.1,
               #scale_pos_weight = 10,
               is_unbalance = TRUE,
               verbosity = -1,
               verbose = -1
)

fit <- lgb.train(params,
                 dtrain,
                 nrounds = mean(best_iter))

pred <- predict(fit, x[idx_final_pred, ])

y_final <- y_df[idx_final_pred, ] %>% 
  select(-APROVOU)

y_final$chance <- pred
y_final$velocidade <- x[idx_final_pred, "ntram_30d"]
y_final$qtd_tramitacoes <- x[idx_final_pred, "ntram"]

fwrite(y_final, 'produced_data/dados_chance.csv')
