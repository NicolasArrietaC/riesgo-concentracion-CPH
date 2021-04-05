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
 c('tidymodels', 'readr'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- paste0('1-1-Datasets/contratistas/')

# Conjunto de entrenamiento
contratistas <- read_csv(paste0(direccion, '2A_contratistas.csv'))

# 3. Preprocesamiento ----
# Division del conjunto de datos
set.seed(73) # fijar semilla
contratistas_split <- initial_split(contratistas, prop = 4/5)

# Extracción del dataset
contratistas_train <- training(contratistas_split) # train
contratistas_test <- testing(contratistas_split) # test

# Estandarización de variables
contratistas_rec <- recipe(IRCC ~ ., data = contratistas) %>% # Variables del modelo
 step_normalize(all_numeric(), -all_outcomes()) %>% # Normaliza los datos numéricos
 step_dummy(all_nominal(), -id_contratista_std) %>% # Crea variables dummy
 step_rm(id_contratista_std)

# Receta final
summary(contratistas_rec)

# Conjunto estandarizado
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
        set_engine('lm') %>% # adds lm implementation of linear regression
        set_mode('regression')

# 5. Entrenamiento ----
IRCC_fit <- lm_model %>% 
        fit(IRCC ~ ., data = contratistas_train)

# 6. Análisis de resultados ----
# Resultados del ajuste
summary(IRCC_fit$fit)

# Prueba del modelo con los datos de entrenamiento 
contratistas_test$pred <- predict(IRCC_fit, new_data = contratistas_test)[[1]]

# Analisis grafico de valores predecidos vs reales
ggplot(contratistas_test, aes(x = pred, y = IRCC)) + 
 geom_point(alpha = 0.3, size = 1) +
 geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
 labs(title = 'Valores reales vs predicción para el IRCC contratistas',
      x = 'Predicción', y = 'Real',
      caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
 theme_light()

# Calculo del RMSE
attach(contratistas_test)

rmse <- sqrt(sum((IRCC - pred) ^ 2) / nrow(contratistas_test))
print(paste('Valor RMSE: ', round(rmse, 2)))
