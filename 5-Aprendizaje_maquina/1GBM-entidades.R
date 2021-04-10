# Aprendizaje de máquina
# ****************************************************************************
# Objetivo: Determinar un modelo capaz de predecir la concentración de los
# actores de la contratación pública, a partir de datos transaccionales y 
# de fuentes secundarias.
#
# Variable target: IRCC (Indice de riesgo de concentración)
# Método de aprendizaje: Gradient Boosting Machine
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('readr', 'ggplot2', 'dplyr', 'recipes',
   'rsample', 'gbm',
   'vip', 'caret'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- paste0('1-1-Datasets/entidades/')

# Conjunto de entrenamiento
entidades <- read_csv(paste0(direccion, '2A_entidades.csv'))

# 3. Preprocesamiento ----
# Division del conjunto de datos
set.seed(73) # fijar semilla
entidades_split <- initial_split(entidades, prop = 4/5) # 80% training

# Extracción del dataset
entidades_train <- training(entidades_split) # train
entidades_test <- testing(entidades_split) # test

# Estandarización de variables
# Receta
entidades_rec <- recipe(IRCC ~ ., data = entidades) %>% # Formula
 step_normalize(all_numeric(), -all_outcomes()) %>% # Norm. numérica
 step_dummy(all_nominal(), -nit_entidad) %>% # Transformación dummy
 step_rm(nit_entidad) # Eliminación de variables no predictorias

# Revisión de la receta
summary(entidades_rec)

# Aplicación de la receta
# Train
entidades_train <- entidades_rec %>%
 prep(entidades_train) %>% # Aplicar el preprocesamiento al train
 juice()

# Test
entidades_test <- entidades_rec %>%
        prep(entidades_test) %>% # Aplicar el preprocesamiento al test
        juice()

# 4. Diseño del modelo ----
# 4.1. Gradient Boosting Machine

# Hiperparámetros
hyper_grid <- expand.grid(
  shrinkage = c(.001, .01, .1, .3),
  interaction.depth = c(1, 3, 5, 7),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# Encontrando la configuración óptima
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(478)
  
  # train model
  gbm.tune <- gbm(
    formula = IRCC ~ .,
    distribution = "gaussian",
    data = entidades_train,
    n.trees = 10000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$cv.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$cv.error))
  
  print(paste0('Tratamiento ', i, '/',nrow(hyper_grid)))
}

# Resultados del tunning
hyper_grid <- hyper_grid %>% arrange(min_RMSE)

hyper_grid # Resultado

# 5. Entrenamiento Final ----
# Selección del mejor resultado
param_final <- head(hyper_grid %>% 
    select(shrinkage, interaction.depth, optimal_trees), 1)
# Parámetros
param_final

# Modelo con la mejor configuración
gbm.fit <- gbm(
  formula = IRCC ~ .,
  distribution = "gaussian",
  data = entidades_train,
  n.trees = param_final$optimal_trees,
  interaction.depth = param_final$interaction.depth,
  shrinkage = param_final$shrinkage,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

# Modelo final
# train GBM model
gbm.fit.final <- gbm(
  formula = IRCC ~ .,
  distribution = "gaussian",
  data =  prep(entidades_rec, entidades) %>% juice(),
  n.trees = param_final$optimal_trees,
  interaction.depth = param_final$interaction.depth,
  shrinkage = param_final$shrinkage,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

# 6. Análisis de resultados ----
# Resultados del ajuste
# Prueba del modelo con los datos de entrenamiento 
entidades_test$pred <- predict(gbm.fit, 
                               n.trees = gbm.fit$n.trees, 
                               entidades_test)

# Calculo del RMSE y R2 para el test
v_rmse <- RMSE(entidades_test$pred, entidades_test$IRCC)
v_rsq <- R2(entidades_test$pred, entidades_test$IRCC)
print(paste0('|TEST| RMSE: ', round(v_rmse, 2), ', R2: ', round(v_rsq, 2)))

# Analisis grafico de valores predecidos vs reales
ggplot(entidades_test, aes(x = pred, y = IRCC)) + 
  geom_point(alpha = 0.3, size = 1, color = '#264653') +
  geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
  labs(title = 'Valores reales vs predicción para el IRCC entidades',
       subtitle = 'Modelo Gradient Boosting',
       x = 'Predicción', y = 'Real',
       caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
  theme_light()

# variables importantes en el modelo
vip(gbm.fit.final, fill = '#264653', num_features = 15) +
  labs(title = 'Importancia de variables para el IRCC entidades',
       subtitle = 'Modelo Gradient Boosting',
       x = '', y = 'Importancia',
       caption = 'Contratación pública hospitalaria en Colombia 2014-2019') + 
  theme_classic()
