# Indicadores de terridata
# ****************************************************************************
# Objetivo: Generación del indicadores territoriales de la fuente de
# terridata
# Dimensión: Educación
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('readr', 'dplyr', 'tidyr', 'skimr'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos eduación
ind_edu <- read_csv(paste0(direccion, "terr_indsel_educacion.csv"),
                    locale = locale(encoding = "UTF-8")) 

# 3. Generación de indicadores para el caso de Geraldine y Yilber ----
ind_edu <- ind_edu %>% pivot_wider(names_from = 'indicador', 
                                   values_from = 'valor')

# 4. Escritura de datos ----
write_csv(ind_edu, paste0(direccion, 'terr_indsel_educacion2.csv'))
