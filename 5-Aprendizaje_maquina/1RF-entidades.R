# Aprendizaje de máquina
# ****************************************************************************
# Objetivo: Determinar un modelo capaz de predecir la concentración de los
# actores de la contratación pública, a partir de datos transaccionales y 
# de fuentes secundarias.
#
# Variable target: IRCC (Indice de riesgo de concentración)
# Método de aprendizaje: Random Forest
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('tidymodels', 'readr', 'forcats'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- paste0('1-1-Datasets/entidades/')

# Dataset para el modelo
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
 update_role(nit_entidad, new_role = 'id') # Definir nuevos roles

# Revisión de la receta
summary(entidades_rec)

# 4. Diseño del modelo ----
# 4.1. Modelo Random Forest
rf_model <- rand_forest() %>%
        set_args(mtry = tune(),
                 trees = tune()) %>%
        set_engine("ranger", importance = "impurity") %>%
        set_mode("regression")

# 4.2. Workflow
workflow <- workflow() %>%
        add_recipe(entidades_rec) %>%
        add_model(rf_model)

# Hiperparámetros
rf_grid <- expand_grid(mtry = c(5, 6, 7),
                       trees = c(100, 200, 300))

# Parámetros validación cruzada
cv_folds <- vfold_cv(entidades_train, v = 10)

# Encontrando la configuración óptima
set.seed(487)# fijar semilla
rf_tune_results <- workflow %>%
        tune_grid(resamples = cv_folds, # CV
                  grid = rf_grid, # hiperparámetros
                  metrics = metric_set(rmse) # métrica usada
        )

# Resultados del tunning
rf_metrics <- rf_tune_results %>%
        collect_metrics() %>% arrange(mean)

# Imprimir resultado
rf_metrics

# 5. Entrenamiento Final ----
# Selección del mejor resultado
param_final <- select_best(rf_tune_results)
# Parámetros
param_final

# Actualización del workflow
workflow <- workflow %>%
        finalize_workflow(param_final)

# Ejecución de la mejor configuración
rf_fit <- workflow %>%
        last_fit(entidades_split)

# 6. Análisis de resultados ----
# 6.1. Rendimiento del modelo
# Resultado final test
rf_fit %>% collect_metrics()

# Estimación de métricas con el dataset completo
# Modelo final
final_model <- fit(workflow, entidades)
# Resultado
final_model

# 6.2. Predicción vs valores reales
# Recolectar predicciones
test_predictions <- rf_fit %>% collect_predictions()

# Analisis grafico de valores predecidos vs reales
ggplot(test_predictions, aes(x = .pred, y = IRCC)) + 
 geom_point(alpha = 0.3, color = '#264653') +
 geom_abline(intercept = 0, slope = 1, color = '#F4A261') +
 labs(title = 'Valores reales vs predicción para el IRCC entidades',
      subtitle = 'Modelo Random Forest',
      x = 'Predicción', y = 'Real',
      caption = 'Contratación pública hospitalaria en Colombia 2014-2019') +
 theme_light()

# Importancia de las variables
# Extracción de la importancia
ranger_obj <- pull_workflow_fit(final_model)$fit

rf_importancia <- tibble(
        variable = names(ranger_obj$variable.importance),
        importancia = ranger_obj$variable.importance)

rf_importancia %>% 
  mutate(variable = fct_reorder(variable, importancia)) %>% 
  ggplot(aes(importancia, variable)) +
  geom_bar(stat = "identity", fill = '#264653') +
  labs(title = 'Importancia de variables para el IRCC entidades',
       subtitle = 'Modelo Random Forest',
       x = 'Importancia', y = '',
       caption = 'Contratación pública hospitalaria en Colombia 2014-2019') + 
  theme_classic()
