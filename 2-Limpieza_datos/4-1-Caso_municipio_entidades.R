# Caso de estandarización de municipios de entidades
# ****************************************************************************
# Objetivo: Establecer el código del municipio de divipola a todos los 
# municipios registrados en el conjunto de SECOP.
# ****************************************************************************

# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'stringr', ## tidyverse
   'stringdist', 'fuzzyjoin'),
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_csv(paste0(direccion, 'secop_i_ips.csv.gz'),
                      locale = locale(encoding = 'UTF-8'))

# Conjunto de datos divipola
municipios <- read_csv(paste0(direccion, 'nombres_municipios.csv'),
                       locale = locale(encoding = 'UTF-8'))

municipios <- municipios %>% 
 rename(dpto = DEPARTAMENTO, cod_dpto = `CÓDIGO DANE DEL DEPARTAMENTO`,
        munp = MUNICIPIO, cod_munp = `CÓDIGO DANE DEL MUNICIPIO`,
        region = REGION)

# 3. Estandarización ----

# 3.1. Departamento de la entidad
# Comprobación de los valores de departamento
# temp <- contratos %>% group_by(departamento_entidad) %>% 
#  count() %>% arrange(desc(n))

#temp <- municipios %>% select(dpto) %>% distinct()

## No hay conflictos en los valores de departamentos de ambas tablas

# 3.2. Municipio de entidades
temp <- contratos %>% group_by(municipio_entidad) %>% 
  count() %>% arrange(desc(n))

temp2 <- municipios %>% select(munp) %>% distinct()

# Existen municipios que presentan más de una opción o una aclaración
# en el nombre del municipio
contratos <- contratos %>% rowwise() %>% 
 mutate(municipio_entidad = if_else(str_detect(municipio_entidad, '[/|(]'), 
          str_sub(municipio_entidad, 1, 
                  str_locate(municipio_entidad, '[/|(]')[[1]] - 1), 
          municipio_entidad))

# Estandarización
estandar_string <- function(string) {
 # eliminacion de signos de puntuacion
 string <- iconv(string, from = "UTF-8", to = 'ASCII//TRANSLIT')
 # eliminacion de caracteres inecesarios
 string <- str_remove_all(string, '[:punct:]|[:space:]')
 # Aplicación de mayúsculas
 string <- str_to_upper(string) 
 # Retorno
 return(string)
}

# Aplicación del estandar
# SECOP
lugares_secop <- contratos %>% 
 select(departamento_entidad, municipio_entidad) %>% distinct() %>% 
 mutate(dpto_st = estandar_string(departamento_entidad),
        munp_st = estandar_string(municipio_entidad))

# DIVIPOLA
lugares_divipola <- municipios %>% 
 select(dpto, munp, cod_munp) %>% distinct() %>% 
 mutate(dpto_st = estandar_string(dpto),
        munp_st = estandar_string(munp))

# Fuzzy join
tolerancia <- 0.2 # [0-0.1: Baja, 0.1-0.2: Moderada, >0.2: Alta]

# Union, teniendo en cuenta:
# - Solamente une las que cumplen el maximo de tolerancia
# - Elimina duplicados que pueden generarse
# - Utiliza el metodo de evaluacion jw
resultado_match <- lugares_secop %>% 
 stringdist_left_join(lugares_divipola, 
                       by = c('dpto_st', 'munp_st'),
                       method = 'jw', max_dist = tolerancia, 
                       distance_col = "result") %>%
 mutate(result = (dpto_st.result + munp_st.result) / 2) %>%
 group_by(departamento_entidad, municipio_entidad) %>% arrange(result) %>% 
 slice_head(1)

# Ajuste manual
# Casos que presentan problema
lugares <- c('SUCRESINCE', 'PUTUMAYOPUERTOLEGUIZAMO', 
            'ANTIOQUIASANPEDRODELOSMILAGROS', 'CESARMANAUREBALCONDELCESAR',
            'NORTEDESANTANDERSANJOSEDECUCUTA', 'TOLIMASANSEBASTIANDEMARIQUITA')

# Valores correctos de codigo
codigo_munp <- c(70742, 86573, 
                 5664, 20443,
                 54001, 73443)

# Cambio
for (i in lugares) {
 resultado_match[i == paste0(resultado_match$dpto_st.x, 
                             resultado_match$munp_st.x), 'cod_munp'] <- 
  codigo_munp[which(lugares == i)]
}

# Seleccion de variables de importancia
resultado_match <- resultado_match %>% 
 select(departamento_entidad, municipio_entidad, cod_munp) %>% 
 rename(cod_munp_entidad = cod_munp)

# Agregación del codigo de municipio al conjunto de contratos
contratos <- contratos %>% 
 left_join(resultado_match, 
           by = c('departamento_entidad', 'municipio_entidad'))

# 3.3. Validación de la información
sum(is.na(contratos$cod_munp_entidad)) # No hay NAs

temp <- contratos %>% group_by(cod_munp_entidad) %>% count()

# 4. Eliminación de variables ----
contratos$departamento_entidad <- NULL
contratos$municipio_entidad <- NULL

# 5. Escritura de datos ----
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))