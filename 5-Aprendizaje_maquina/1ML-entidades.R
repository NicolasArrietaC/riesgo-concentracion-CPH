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
# Modelo lineal
lm_model <- linear_reg() %>% 
        set_engine('lm') %>%
        set_mode('regression')

# 5. Entrenamiento Final ----
IRCC_fit <- lm_model %>% 
        fit(IRCC ~ ., data = entidades_train)

# 6. Análisis de resultados ----
# Resultados del ajuste
summary(IRCC_fit$fit)

# Prueba del modelo con los datos de entrenamiento 
entidades_test$pred <- predict(IRCC_fit, new_data = entidades_test)[[1]]

# Calculo del RMSE y R2 para el test
v_rmse <- RMSE(entidades_test$pred, entidades_test$IRCC)
v_rsq <- R2(entidades_test$pred, entidades_test$IRCC)
print(paste0('|TEST| RMSE: ', round(v_rmse, 2), ', R2: ', round(v_rsq, 2)))

# Analisis grafico de valores predecidos vs reales
ggplot(entidades_test, aes(x = pred, y = IRCC)) + 
        geom_point(alpha = 0.3, size = 1, color = '#264653') +
        geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
        labs(title = 'Valores reales vs predicción para el IRCC contratistas',
         subtitle = 'Modelo Lineal',
         x = 'Predicción', y = 'Real',
         caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
        theme_light()
