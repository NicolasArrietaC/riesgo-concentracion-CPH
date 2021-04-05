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
direccion <- paste0('1-1-Datasets/entidades/')

# Conjunto de entrenamiento
entidades <- read_csv(paste0(direccion, '2A_entidades.csv'))

# 3. Preprocesamiento ----
# Division del conjunto de datos
set.seed(73) # fijar semilla
entidades_split <- initial_split(entidades, prop = 4/5)

# Extracción del dataset
entidades_train <- training(entidades_split) # train
entidades_test <- testing(entidades_split) # test

# Estandarización de variables
# Receta
entidades_rec <- recipe(IRCC ~ ., data = entidades) %>% # Variables del modelo
 step_normalize(all_numeric(), -all_outcomes()) %>% # Normaliza los datos numéricos
 step_dummy(all_nominal(), -nit_entidad) %>% # Crea variables dummy
 step_rm(nit_entidad)

# Revisión de la salida
summary(entidades_rec)

# Conjunto estandarizado
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
        set_engine('lm') %>% # adds lm implementation of linear regression
        set_mode('regression')

# 5. Entrenamiento ----
IRCC_fit <- lm_model %>% 
        fit(IRCC ~ ., data = entidades_train)

# 6. Anpalisis de resultados ----
# Resultados del ajuste
summary(IRCC_fit$fit)

# Prueba del modelo con los datos de entrenamiento 
entidades_test$pred <- predict(IRCC_fit, new_data = entidades_test)[[1]]

# Analisis grafico de valores predecidos vs reales
ggplot(entidades_test, aes(x = pred, y = IRCC)) + 
        geom_point(alpha = 0.3, size = 1) +
        geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
        labs(title = 'Valores reales vs predicción para el IRCC contratistas',
             x = 'Predicción', y = 'Real',
             caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
        theme_light()

# Calculo del RMSE
attach(entidades_test)

rmse <- sqrt(sum((IRCC - pred) ^ 2) / nrow(entidades_test))
print(paste('Valor RMSE:', round(rmse, 2)))
