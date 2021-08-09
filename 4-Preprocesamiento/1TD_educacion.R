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
# Wider conjunto de datos
ind_edu <- ind_edu %>% pivot_wider(names_from = 'indicador', 
                                   values_from = 'valor')

# Corrección de datos numéricos
ind_edu <- ind_edu %>% mutate_at(names(ind_edu[, 3:9]), as.double)

# Resumen del conjunto de datos
skim(ind_edu)

# Agregación de indicadores por municipio
ind_edu <- ind_edu %>% 
 group_by(codigo_entidad) %>% 
 summarise(
  # Cobertura neta
  ine_coberturaNeta_m = mean(ine_coberturaNeta, na.rm = T),
  # Tasa de desrcion
  ine_tasaDesercion_m = mean(ine_tasaDesercion, na.rm = T),
  # Tasa repitencia
  ine_tasaRepitencia_m = mean(ine_tasaRepitencia, na.rm = T),
  # Tasa analfabetistmo
  ine_tasaAnalfabetismo_m = mean(ine_tasaAnalfabetismo, na.rm = T),
  # Porcentaje Asistencia
  ine_porcentajeAsistencia_m = mean(ine_porcentajeAsistencia, na.rm = T),
  # Prueba de matematicas
  ine_pruebaLecturaMatematicas_m = mean(ine_pruebaLecturaMatematicas, na.rm=T),
  # Prueba de lectura
  ine_pruebaLecturaCritica_m = mean(ine_pruebaLecturaCritica, na.rm = T)
 )

# 4. Escritura de datos ----
direccion <- '1-1-Datasets/terridata/'
write_csv(ind_edu, paste0(direccion, '1_terr_indsel_educacion.csv'))
