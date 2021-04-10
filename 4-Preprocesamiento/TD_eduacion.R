# Indicadores de terridata
# ****************************************************************************
# Objetivo: Seleccionar las variables más importantes de cada dimension
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
ind_edu <- read_csv(paste0(direccion, "terridata_educacion.csv.gz"),
                    locale = locale(encoding = "UTF-8")) 

# Renombrar variables
names(ind_edu) <- c("codigo_departamento", "departamento",
                    "codigo_entidad", "entidad", 
                    "dimension", "subcategoria", 
                    "indicador", "valor", 
                    "dato_cualitativo", "anno",
                    "mes", "fuente", "und_medida")

# 3. Selección de indicadores ----

# 3.1. Selección de columnas de interés
ind_edu <- select(ind_edu, all_of(c('codigo_entidad', 'indicador', 
                                    'valor', 'anno')))

# Indicadores de interés
indicadores <- c(
 'Cobertura neta en educación - Total', 
 'Porcentaje de asistencia de 5 a 24 años Urbana (Censo)',
 'Puntaje promedio Pruebas Saber 11 - Lectura crítica',
 'Puntaje promedio Pruebas Saber 11 - Matemáticas',
 'Tasa de deserción intra-anual del sector oficial en educación básica y media (Desde transición hasta once)',
 'Tasa de repitencia del sector oficial en educación básica y media (Desde transición hasta once)',
 'Tasa de Analfabetismo (Censo)'
)

# Filtro de indicadores y años de interés
ind_edu <- filter(ind_edu, indicador %in% indicadores, 
                  anno %in% 2014:2019)

# Nuevos nombres para indicadores
ren_ind <- c('ine_coberturaNeta', 'ine_porcentajeAsistencia',
             'ine_pruebaLecturaCritica', 'ine_pruebaLecturaMatematicas',
             'ine_tasaDesercion', 'ine_tasaRepitencia', 
             'ine_tasaAnalfabetismo')

# Cambio en los nuevos datos
ind_edu <- ind_edu %>% 
        left_join(tibble(indicador = indicadores,
                         indicadorn = ren_ind), by = 'indicador') %>% 
        select(-indicador) %>% rename(indicador = indicadorn)

# 4. Escritura de datos ----
write_csv(ind_edu, paste0(direccion, 'terr_indsel_educacion.csv'))
