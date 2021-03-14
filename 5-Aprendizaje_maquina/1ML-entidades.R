# Aprendizaje de máquina
# ****************************************************************************
# Objetivo: Determinar un modelo capaz de predecir la concentración de los
# actores de la contratación pública, a partir de datos transaccionales y 
# de fuentes secundarias.
#
# Variable target: val_HHI (Cuantitativa)
# Método de aprendizaje: Modelo lineal
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('tidymodels'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- paste0('C:/Users/nico2/Proyectos/contratacion_publica/',
                    'riesgo-concentracion-CPH/1-1-Datasets/entidades/')

# Conjunto de entrenamiento
entidades <- read_csv(paste0(direccion, '1A_entidades.csv'))

# 3. Preprocesamiento ----

# Division del conjunto de datos
set.seed(73) # fijar semilla
entidades_split <- initial_split(entidades, prop = 4/5)

# Extracción del dataset
entidades_train <- training(entidades_split) # train
entidades_test <- testing(entidades_split) # test

# Estandarización de variables
# Receta
entidades_rec <- recipe(val_HHI ~ ., 
                        data = entidades_train) %>% # Variables del modelo
 step_rm(nit_entidad) %>% # Remover la varaible de identificación
 step_normalize(all_numeric(), -val_HHI) %>% # Normaliza los datos numericos
 step_dummy(all_nominal(), one_hot = TRUE) # Crea variables dummy

# Conjunto estandarizado
entidades_train_prep <- entidades_rec %>%
 prep(entidades_train) %>% # Aplicar el preprocesamiento al train
 juice()

# 4. Diseño del modelo ----
# Modelo lineal
lm_reg <- lm(val_HHI ~ ., data = entidades_train_prep)
summary(lm_reg)
