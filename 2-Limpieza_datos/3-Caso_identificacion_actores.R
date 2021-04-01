# Caso de errores en la identificacion de los actores de los procesos
# ****************************************************************************
# Objetivo: Estandarizar los nits de las entidades y contratistas,
# que presentan diferencias en sus nombres o valores extraños.
# ****************************************************************************

# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'stringr', 'ggplot2', ## tidyverse
   'stringdist', 'fuzzyjoin'),
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_csv(paste0(direccion, 'secop_i_ips.csv.gz'),
                      locale = locale(encoding = 'UTF-8'))

# 3. Limpieza en la identificacion de los actores ----
# 3.1. Entidades ----
# No se registran caracteres en los nits de la entidad
temp <- contratos %>% filter(str_detect(nit_entidad, '[:alpha:]')) %>% 
        select(nombre_entidad) %>% distinct() %>% 
        rename(nombre_sin_nit = nombre_entidad)

# Extraccion del complemento de nombres que tienen nit
temp_2 <- contratos %>% select(nombre_entidad, nit_entidad) %>% 
        distinct() %>% filter(!(nombre_entidad %in% temp$nombre_sin_nit))

# Ejecucion del algoritmo de fuzzy join
# Ningun caso presento un emparejamiento correcto.
resultado_match <- stringdist_inner_join(temp, temp_2, method = "jw",
                        by = c('nombre_sin_nit' = 'nombre_entidad'),
                        max_dist = 0.10, distance_col = "resl") %>% 
        group_by(nombre_sin_nit) %>% slice_min(resl)

# La solucion es darles un nit personalizado
temp$nit <- paste0(rep('ND', nrow(temp)), 1:nrow(temp))

# Union con el conjunto de datos completo
contratos <- contratos %>%
        left_join(temp, by = c('nombre_entidad' = 'nombre_sin_nit')) %>% 
        mutate(nit_entidad = if_else(!is.na(nit), nit, nit_entidad)) %>% 
        select(-nit)

# 3.2. Contratistas ----
# 3.2.1. Anomalias en las variables de identificación ----
# Extracción de contratistas del conjunto de datos
contratistas <- contratos %>% 
  group_by(id_contratista, razon_social_contratista, 
    id_representante_legal, nombre_representante_legal) %>% count()

# Desagregación de la información de las categorias
# Debido a que se presenta una mezcla de información en las variables de
# identificación de los contratistas, es necesario, desagregar la parte
# numérica de la parte no numérica y encontrar posibles arreglos al
# identificador del contratista.
# ID contratista
## Extracción parte numérica del identificador
contratistas <- contratistas %>% 
  mutate(id_dig = str_remove_all(id_contratista, '[^[:digit:]]'))
## Extracción parte no numérica del identificador
contratistas <- contratistas %>% 
  mutate(id_alpha = str_remove_all(id_contratista, '[^[:alpha:]]'))

# Nombre del contratista
## Extracción parte numérica del nombre
contratistas <- contratistas %>% 
  mutate(nom_dig = str_remove_all(razon_social_contratista, '[^[:digit:]]'))
## Extracción parte no numérica del nombre
contratistas <- contratistas %>% 
  mutate(nom_alpha = str_remove_all(razon_social_contratista, '[^[:alpha:]]'))

# ID representante legal
## Extracción parte numérica del ID
contratistas <- contratistas %>% 
  mutate(id_rl_dig = str_remove_all(id_representante_legal, '[^[:digit:]]'))
## Extracción parte no numérica del ID
contratistas <- contratistas %>% 
  mutate(id_rl_alpha = str_remove_all(id_representante_legal, '[^[:alpha:]]'))

# Nombre representante legal
## Extracción parte numérica del nombre
contratistas <- contratistas %>% 
  mutate(nom_rl_dig = str_remove_all(nombre_representante_legal, 
                                     '[^[:digit:]]'))
## Extracción parte no numérica del nombre
contratistas <- contratistas %>% 
  mutate(nom_rl_alpha = str_remove_all(nombre_representante_legal, 
                                       '[^[:alpha:]]'))

# Transformar las variables de dig a integer
contratistas <- contratistas %>% mutate(across(matches("dig"), as.numeric))

estandar_string <- function(string) {
  # eliminacion de signos de puntuacion
  string <- iconv(string, from = "UTF-8", to = 'ASCII//TRANSLIT')
  # Aplicación de mayúsculas
  string <- str_to_upper(string) 
  # Retorno
  return(string)
}

# Transformar las variables de dig a integer
contratistas <- contratistas %>% 
  mutate(across(matches("alpha"), estandar_string))

# Anomalias en la identificación de contratistas
# Contratistas sin ID registrado
temp <- contratistas %>% 
  filter(is.na(id_dig)) %>% 
  mutate(llave1 = id_alpha == nom_alpha,
         llave2 = id_alpha == id_rl_alpha,
         llave3 = id_alpha == nom_rl_alpha,
         llave4 = nom_alpha == id_rl_alpha,
         llave5 = nom_alpha == nom_rl_alpha,
         llave6 = id_rl_alpha == nom_rl_alpha,
         llave7 = nom_dig == id_rl_dig,
         llave8 = id_rl_dig == nom_rl_dig)

# Contratistas sin nombre registrado
temp <- contratistas %>% 
  filter(nom_alpha == '' | is.na(nom_alpha)) %>% 
  mutate(llave1 = id_alpha == id_rl_alpha,
         llave2 = id_alpha == nom_rl_alpha,
         llave3 = id_rl_alpha == nom_rl_alpha,
         llave4 = id_dig == id_rl_dig,
         llave5 = id_dig == nom_dig,
         llave6 = id_dig == nom_rl_dig,
         llave7 = nom_dig == id_rl_dig,
         llave8 = nom_dig == nom_rl_dig,
         llave9 = id_rl_dig == nom_rl_dig,
         id_alpha_difnull = id_alpha != '',
         id_rl_alpha_difnull = id_rl_alpha != '',
         nom_rl_alpha_difnull = nom_rl_alpha != '')

# Contratistas con patrones extraños en el nombre
# Reparación de los contratistas
f_casos <- c('CRA', 'KR', 'CALLE', 'CLL', 
             'CLCRA', 'AVENI', 'MANZANACASA')

f_casos_int <- 'CARRERA'

for (i in f_casos) f_casos_int <- paste(f_casos_int, i, sep = '|')

temp <- contratistas %>% 
  filter(str_detect(nom_alpha, f_casos_int)) %>% 
  mutate(llave1 = id_alpha == id_rl_alpha,
         llave2 = id_alpha == nom_rl_alpha,
         llave3 = id_rl_alpha == nom_rl_alpha,
         llave4 = id_dig == id_rl_dig,
         llave5 = id_dig == nom_dig,
         llave6 = id_dig == nom_rl_dig,
         llave7 = nom_dig == id_rl_dig,
         llave8 = nom_dig == nom_rl_dig,
         llave9 = id_rl_dig == nom_rl_dig,
         id_alpha_difnull = id_alpha != '',
         id_rl_alpha_difnull = id_rl_alpha != '',
         nom_rl_alpha_difnull = nom_rl_alpha != '',
         nom_digdiffNull = !is.na(nom_dig),
         nom_rl_digdiffNull = !is.na(nom_rl_dig))

# Reparación de los contratistas
f_casos <- c('CARRERA', 'CRA', 'KR', 'CALLE', 'CLL', 
             'CLCRA', 'AVENI', 'MANZANACASA')

# Ajuste del ID del contratista
# Hay 625 contratistas que no tienen ID (0.13% del total de contratistas)
contratistas <- contratistas %>% 
  mutate(id_contratista_nuevo = case_when(
    id_dig > 180000 & !(id_alpha %in% f_casos) ~ id_dig,
    id_alpha == nom_rl_alpha & nom_dig == id_rl_dig ~ id_rl_dig,
    id_rl_dig == nom_rl_dig & id_rl_dig > 1e3 ~ id_rl_dig,
    !is.na(id_rl_dig) & id_rl_dig > 100000 ~ id_rl_dig,
    TRUE ~ 1))

# Ajuste del nombre del contratista
contratistas <- contratistas %>% 
  mutate(grupo_sospechoso = str_detect(nom_alpha, f_casos_int),
         nom_cont = str_to_upper(str_remove_all(razon_social_contratista, 
                                     '[^[:alpha:]|[:digit:]]')),
    nom_contratista_nuevo = case_when(
    (nom_alpha == '' | is.na(nom_alpha)) & 
      id_alpha == nom_rl_alpha & id_alpha != '' ~ nom_rl_alpha,
    (nom_alpha == '' | is.na(nom_alpha)) & 
      id_dig == id_rl_dig & nom_rl_alpha != '' ~ nom_rl_alpha,
    grupo_sospechoso & is.na(nom_dig)  ~ nom_alpha,
    grupo_sospechoso & !is.na(nom_dig)  ~ nom_cont,
    !grupo_sospechoso & (nom_alpha != '' | !is.na(nom_alpha)) ~ nom_alpha,
  )) %>% select(-all_of(c('grupo_sospechoso', 'nom_cont')))

# Resultados
# Existen 195 contratistas con id vacio
temp <- contratistas %>% filter(id_contratista_nuevo == 1)
# Existen 176 contratistas con nombre vacio
temp <- contratistas %>% 
  filter(is.na(nom_contratista_nuevo) | nom_contratista_nuevo == '')
# Se puede reparar uno
contratistas <- contratistas %>% 
  mutate(nom_contratista_nuevo = if_else(
    id_alpha == 'MARIAELSOCORRODURANROJAS', 'MARIAELSOCORRODURANROJAS', 
      nom_contratista_nuevo))

# El lado positivo es que la mayoria que no poseen un numéro, si tienen
# nombre y viceversa, por lo que es posible encontrar la pareja.

# 3.2.2. Divergencias entre los valores
# ¿Por qué existen diferentes nombres para un único id de contratista?

contratistas_unicos <- contratistas %>% 
  group_by(id_contratista_nuevo, nom_contratista_nuevo) %>% 
  summarise(n = sum(n, na.rm = T))

# Cálculo para determinar los contratistas con diferentes nombres para 
# un unico id
temp <- contratistas_unicos %>% 
  group_by(id_contratista_nuevo) %>% 
  mutate(N = sum(n), p = n / N * 100, nombres = n()) %>% 
  slice_max(p, with_ties = F) %>% filter(nombres > 1) %>% 
  arrange(desc(nombres))

# Se observa que existe una gran proporción de casos que tienen un nombre de
# preferencia
ggplot(temp %>% mutate(ajuste = (nombres > 2 & p > 8) | p > 50), 
       aes(x = p, fill = ajuste)) + geom_histogram(bins = 100) + 
  theme_light()

# Analisis de intervalor
sum(temp$p < 50) # 7.51%
sum(temp$p == 50) # 22.10%
sum(temp$p > 50) # 70.39%

# Selección de los casos donde el nombre dominante es mayor al 50%
temp <- temp %>% filter((nombres > 2 & p > 8) | p > 50) %>% 
  select(id_contratista_nuevo, nom_contratista_nuevo) %>% 
  rename(nom_contratista_nuevo2 = nom_contratista_nuevo)

# Agregación del ajuste al conjunto de datos de contratistas únicos
contratistas_unicos <- contratistas_unicos %>%
  left_join(temp, by = 'id_contratista_nuevo') %>% 
  mutate(nom_contratista_nuevo2 = if_else((!is.na(nom_contratista_nuevo2)),
              nom_contratista_nuevo2, nom_contratista_nuevo))

# Cálculo para determinar los contratistas con diferentes ids para 
# un unico nombre
temp <- contratistas_unicos %>% 
  group_by(nom_contratista_nuevo2) %>% 
  mutate(N = sum(n), p = n / N * 100, 
         ids = n_distinct(id_contratista_nuevo)) %>% 
  slice_max(p, with_ties = F) %>% filter(ids > 1) %>% arrange(desc(ids))

ggplot(temp %>% mutate(ajuste = (ids > 2 & p > 4) | p > 50), 
       aes(x = p, fill = ajuste)) + geom_histogram(bins = 100) + 
  theme_light()

# Analisis de intervalor
sum(temp$p < 50) # 11.21%
sum(temp$p == 50) # 21.17%
sum(temp$p > 50) # 67.63%

# Selección de los casos donde el id dominante es mayor al 50%
temp <- temp %>% filter((ids > 2 & p > 4) | p > 50) %>% 
  select(id_contratista_nuevo, nom_contratista_nuevo2) %>% 
  rename(id_contratista_nuevo2 = id_contratista_nuevo)

# Agregación del ajuste al conjunto de datos de contratistas únicos
contratistas_unicos <- contratistas_unicos %>%
  left_join(temp, by = 'nom_contratista_nuevo2') %>% 
  mutate(id_contratista_nuevo2 = if_else((!is.na(id_contratista_nuevo2)),
                    id_contratista_nuevo2, id_contratista_nuevo))
# Remover el dataset temporal
rm(temp)

# Agregación al conjunto de datos general
contratistas_unicos <- contratistas_unicos %>% ungroup() %>% select(-n)

# Agregación al conjunto de contratistas
contratistas <- contratistas %>% 
  left_join(contratistas_unicos, 
            by = c('nom_contratista_nuevo', 'id_contratista_nuevo'))

rm(contratistas_unicos)
# Selección de variables de interes
contratistas <- contratistas %>% 
  select(id_contratista, razon_social_contratista, id_representante_legal,
         nombre_representante_legal, id_contratista_nuevo2, 
         nom_contratista_nuevo2) %>% 
  rename(id_contratista_std = id_contratista_nuevo2,
         nom_contratista_std = nom_contratista_nuevo2)

# Agregación al conjunto de datos de contratos
contratos <- contratos %>% 
  left_join(contratistas, 
            by = c('id_contratista', 'razon_social_contratista', 
                   'id_representante_legal', 'nombre_representante_legal'))

# 6. Eliminación de variables ----
# 6.1. REmover la variable del representante legal
contratos$id_contratista <- NULL
contratos$razon_social_contratista <- NULL
contratos$tipo_doc_representate_legal <- NULL
contratos$id_representante_legal <- NULL

# 5. Escritura de datos ----
# Contratos
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))