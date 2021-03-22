# Caso de estandarización de municipios de contratistas
# ****************************************************************************
# Objetivo: Establecer el código del municipio de divipola a todos los 
# municipios registrados en el conjunto de SECOP.
# ****************************************************************************

# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'stringr', 'tidyr', ## tidyverse
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
# 3.1. Conjunto de datos de secop
# Existe un problema habitual con la variable de municipio de ejecución,
# ya que pueden existir varios casos
# División de los campos que presentan varios municipios registrados
temp <- str_split(contratos$municipio_ejecucion, ';', simplify = T)
temp <- as_tibble(temp, rownames = NULL) # Transformación en tibble
# Agregación del id del contratista, valor con adiciones
temp <- bind_cols(temp, contratos %>% 
                    select(id_contratista, valor_total_con_adiciones) %>% 
                    rename(cuantia = valor_total_con_adiciones))

# Agregacion de la llave
temp$id <- seq(along.with = temp[[1]])

# Transformar los municipios a filas
temp <- temp %>% pivot_longer(cols =  starts_with('V'), names_to = 'posicion',
                              values_to = 'municipio') %>% 
  filter(municipio != '')

# Existen municipios que presentan más de una opción o una aclaración
# en el nombre del municipio
temp <- temp %>% rowwise() %>% 
  mutate(municipio = if_else(str_detect(municipio, '[/|(]'), 
            str_sub(municipio, 1, str_locate(municipio, '[/|(]')[[1]] - 1), 
                             municipio))

# Division del departamento y municipio
temp <- temp %>% rowwise() %>% 
  mutate(departamento = str_sub(municipio, 1, 
                                str_locate(municipio, ' - ')[[1]] - 1),
         municipio = str_remove(municipio, paste0(departamento, ' - ')))

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
lugares_secop <- temp %>% 
  select(departamento, municipio) %>% distinct() %>% 
  mutate(dpto_st = estandar_string(departamento),
         munp_st = estandar_string(municipio))

# DIVIPOLA
lugares_divipola <- municipios %>% 
  select(dpto, munp, cod_munp) %>% distinct() %>% 
  mutate(dpto_st = estandar_string(dpto),
         munp_st = estandar_string(munp))

# Fuzzy join
tolerancia <- 0.20 # [0-0.1: Baja, 0.1-0.2: Moderada, >0.2: Alta]

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
  group_by(departamento, municipio) %>% arrange(result) %>% 
  slice_head(1)

# Ajuste manual
# Casos que presentan problema
lugares <- c('SUCRESINCE', 'PUTUMAYOPUERTOLEGUIZAMO', 
      'ANTIOQUIASANPEDRODELOSMILAGROS', 'CESARMANAUREBALCONDELCESAR',
      'CALDASMONTEBONITO', 'COLOMBIATODOELPAIS',
      'METAMEDELLINDEARIARI', 'METASA',
      'NORTEDESANTANDERSALAZARDELASPALMAS', 'NORTEDESANTANDERSANJOSEDECUCUTA', 
      'NORTEDESANTANDERSANTODOMIGODESILOS', 'OTROSPAISESOTRASCIUDADES',
      'SANANDRESPROVIDENCIAYSANTACATALINAPROVIDENCIA', 
      'SANANDRESPROVIDENCIAYSANTACATALINASANANDRES',
      'TOLIMASANSEBASTIANDEMARIQUITA', 'CALDASARAUCA', 'CALDASBOLIVIA',
      'CUNDINAMARCATOLEMAIDA',
      'LAGUAJIRAPUERTOBOLIVAR', 'MAGDALENAELDIFICIL', 'NARINOBERRUECOS',
      'NARINOBOCASDESATINGA', 'TOLIMAGUAYABAL', 'NAVALLEDELCAUCA')

# Valores correctos de codigo
codigo_munp <- c(70742, 86573, 
                 5664, 20443,
                 17446, 0,
                 50251, 50680,
                 54660, 54001, 
                 54743, 1,
                 88564,
                 88001,
                 73443, 17524, 17541,
                 25488,
                 44847, 47058, 52051,
                 52490, 73055, 76001)

# Cambio
for (i in lugares) {
  resultado_match[i == paste0(resultado_match$dpto_st.x, 
                              resultado_match$munp_st.x), 'cod_munp'] <- 
    codigo_munp[which(lugares == i)]
}

# Seleccion de variables de importancia
resultado_match <- resultado_match %>% 
  select(departamento, municipio, cod_munp) %>% 
  distinct() %>% rename(cod_munp_ejecucion = cod_munp)

# Agregación del codigo de municipio al conjunto de contratos
temp <- temp %>% 
  left_join(resultado_match, by = c('departamento', 'municipio'))

# Preparación del conjunto de datos
# Ajuste de la cuantia cuando hay más de un municipio por contrato 
# (reparticion igualitaria)
temp <- temp %>% 
  group_by(id) %>% mutate(cuantia = cuantia / n())

# Porcion por cada uno de los municipios donde esta presente el contratista
actividad_contratistas <- temp %>% 
  group_by(id_contratista, cod_munp_ejecucion) %>% 
  summarise(valor_total = sum(cuantia)) %>% 
  ungroup(cod_munp_ejecucion) %>% 
  mutate(p = valor_total / sum(valor_total)) %>% 
  select(id_contratista, cod_munp_ejecucion, p)

# Remover temporal
rm(temp)

# 4. Eliminación de variables ----
contratos$municipio_ejecucion <- NULL

# 5. Escritura de datos ----
# Conjunto contratos
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))
# Actividad de contratistas
write_csv(actividad_contratistas, 
          paste0(direccion, 'actividad_contratistas.csv'))