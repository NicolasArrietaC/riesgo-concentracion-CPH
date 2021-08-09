# Indicadores de terridata
# ****************************************************************************
# Objetivo: Generación del indicadores territoriales de la fuente de
# terridata
# Dimensión: Finanzas públicas
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('readr', 'dplyr', 'tidyr', 'skimr'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos finanzas publicas
ind_fp <- read_csv(paste0(direccion, 'terr_indsel_financiero.csv'),
                   locale = locale(encoding = 'UTF-8')) 

# 3. Generación de indicadores para el tratamiento 1 ----
# Agregación del indicador anualmente
ind_fp <- ind_fp %>% group_by(codigo_entidad, indicador, anno) %>% 
 summarise(valor = mean(valor, na.rm = T))

# Wider conjunto de datos
ind_fp <- ind_fp %>% pivot_wider(names_from = indicador, values_from = valor)

# Corrección de datos numéricos
ind_fp <- ind_fp %>% mutate_at(names(ind_fp[, 3:11]), as.double)

# Agregación de indicadores por municipio
ind_fp <- ind_fp %>% group_by(codigo_entidad) %>% 
 summarise(
  # Ingresis totales
  inf_ingresos_m = mean(`Ingresos totales`, na.rm = T),
  # Financiamiento
  inf_financiamiento_m = mean(Financiamiento, na.rm = T),
  # Desempeño fiscal
  inf_desempennofiscal_m = mean(`Indicador de desempeño fiscal`,  na.rm = T),
  #Índice de gestión de proyectos de regalías (IGPR)
  inf_igpr_m = mean(`Índice de gestión de proyectos de regalías (IGPR)`, na.rm=T),
  # Número total de proyectos
  inf_nproyectos_m = mean(`Número total de proyectos`,  na.rm = T),
  # ppa - Educación
  inf_peducacion_m = mean(`ppa - Educación`,  na.rm = T),
  # ppa - Salud
  inf_psalud_m = mean(`ppa - Salud`,  na.rm = T),
  # Regalías per cápita (Valor efectivamente girado al municipio)
  inf_regaliaspc_m = mean(`Regalías per cápita (Valor efectivamente girado al municipio)`, 
                          na.rm = T),
  # Valor del número total de proyectos
  inf_vproyectos_m = mean(`Valor del número total de proyectos`,  na.rm = T))

# 4. Escritura de datos ----
direccion <- '1-1-Datasets/terridata/'
write_csv(ind_fp, paste0(direccion, '1_terr_indsel_financiero.csv'))
