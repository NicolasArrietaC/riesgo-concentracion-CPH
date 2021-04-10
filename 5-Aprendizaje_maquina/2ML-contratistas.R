# Aprendizaje de máquina
# ****************************************************************************
# Objetivo: Determinar un modelo capaz de predecir la concentración de los
# actores de la contratación pública, a partir de datos transaccionales y 
# de fuentes secundarias.
#
# Variable target: IRCC (Indice de riesgo de concentración)
# Método de aprendizaje: Modelo lineal
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('tidymodels', 'readr',
   'caret'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- paste0('1-1-Datasets/contratistas/')

# Conjunto de entrenamiento
contratistas <- read_csv(paste0(direccion, '2A_contratistas.csv'))

# 3. Preprocesamiento ----
# Division del conjunto de datos
set.seed(73) # fijar semilla
contratistas_split <- initial_split(contratistas, prop = 4/5) # 80% training

# Extracción del dataset
contratistas_train <- training(contratistas_split) # train
contratistas_test <- testing(contratistas_split) # test

# Estandarización de variables
contratistas_rec <- recipe(IRCC ~ ., data = contratistas) %>% # Formula
 step_normalize(all_numeric(), -all_outcomes()) %>% # Norm. numérica
 step_dummy(all_nominal(), -id_contratista_std) %>% # Transformación dummy
 step_rm(id_contratista_std) # Eliminación de variables no predictorias

# Revisión de la receta
summary(contratistas_rec)

# Aplicación de la receta
# Train
contratistas_train <- contratistas_rec %>%
 prep(contratistas_train) %>% # Aplicar el preprocesamiento al train
 juice()

# Test
contratistas_test <- contratistas_rec %>%
 prep(contratistas_test) %>% # Aplicar el preprocesamiento al test
 juice()

# 4. Diseño del modelo ----
# Modelo lineal
lm_model <- linear_reg() %>% 
        set_engine('lm') %>%
        set_mode('regression')

# 5. Entrenamiento Final ----
IRCC_fit <- lm_model %>% 
        fit(IRCC ~ ., data = contratistas_train)

# 6. Análisis de resultados ----
# Resultados del ajuste
summary(IRCC_fit$fit)

# Prueba del modelo con los datos de entrenamiento 
contratistas_test$pred <- predict(IRCC_fit, new_data = contratistas_test)[[1]]

# Calculo del RMSE y R2 para el test
v_rmse <- RMSE(contratistas_test$pred, contratistas_test$IRCC)
v_rsq <- R2(contratistas_test$pred, contratistas_test$IRCC)
print(paste0('|TEST| RMSE: ', round(v_rmse, 2), ', R2: ', round(v_rsq, 2)))

# Analisis grafico de valores predecidos vs reales
ggplot(contratistas_test, aes(x = pred, y = IRCC)) + 
 geom_point(alpha = 0.3, size = 1, color = '#264653') +
 geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
 labs(title = 'Valores reales vs predicción para el IRCC contratistas',
      subtitle = 'Modelo Lineal',
      x = 'Predicción', y = 'Real',
      caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
 theme_light()
