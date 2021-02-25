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
# Existen 9 contratos con id vacio, de los cuales 3 se pueden solucionar
temp <- contratos %>% filter(is.na(id_contratista)) %>% 
        select(razon_social_contratista, id_representante_legal, 
               nombre_representante_legal)

# Solucion en la tabla completa
contratos <- contratos %>% 
        mutate(id_contratista = if_else(
                is.na(id_contratista) & !is.na(id_representante_legal), 
                id_representante_legal, id_contratista))

# Seleccion de los que aún presenta problemas
temp <- contratos %>% filter(is.na(id_contratista)) %>% 
        select(razon_social_contratista)
temp <- temp[[1]]

# Busqueda entre los otros contratos
temp_2 <- contratos[contratos$razon_social_contratista %in% temp, 
          c('razon_social_contratista', 'id_contratista')]

temp <- temp_2 %>% filter(is.na(id_contratista)) %>% 
        distinct() %>% select(razon_social_contratista)

# Obtencion de los que se pueden obtener mediante los otros registros
temp_2 <- temp_2 %>% filter(!is.na(id_contratista)) %>% distinct()

# Los que aún faltan por resolver
temp <- temp[!(temp$razon_social_contratista %in% 
                       temp_2$razon_social_contratista),][[1]]

# Extraccion de todos los contratistas
temp_3 <- contratos %>% 
        select(razon_social_contratista, id_contratista,
               id_representante_legal, nombre_representante_legal) %>% 
        distinct() %>% filter(!(razon_social_contratista %in% temp))

# No fue posible encontrar un emparejamiento correcto
# **Consulta sombreada porque consume mucho recurso**
# resultado_match <- stringdist_inner_join(tibble(contratista = temp), 
#                                         temp_3, method = "jw",
#         by = c('contratista' = 'razon_social_contratista'),
#         max_dist = 0.10, distance_col = "resl") %>% 
#        group_by(contratista) %>% slice_min(resl)

rm(temp_3)

# Asignar un valor predeterminado al id que no se pudo encontrar
temp <- rbind(temp_2, 
  tibble(razon_social_contratista = temp, id_contratista = c(1111, 1112))) %>%
        rename(id = id_contratista)

# Asignar nuevos valores en la tabla completa
contratos <- contratos %>%
        left_join(temp, by = 'razon_social_contratista') %>% 
        mutate(id_contratista = if_else(is.na(id_contratista), 
                                        id, id_contratista)) %>% select(-id)

# La mayoria de casos que tienen los caracteres en los contratistas
# se debe a la informacion anexa al documento como el lugar de expedicion
temp <- contratos %>% 
        filter(str_detect(id_contratista, '[:alpha:]|[:punct:]')) %>% 
        select(razon_social_contratista, id_contratista) %>% distinct() %>% 
        rename(nombre_sin_id = razon_social_contratista)

#¿Hay algún nit de contratista sin nungun número?
# R/. No lo hay
temp_2 <- temp %>% 
  mutate(id_contratista = str_remove_all(id_contratista, '[:alpha:]')) %>% 
  filter(is.na(id_contratista))

# Limpieza de todo dato que no sea numero en el nit de los contratistas
contratos <- contratos %>% 
  mutate(id_contratista = str_remove_all(id_contratista, '[^[:digit:]]'))

# ¿Por qué existen diferentes id para un único nombre de contratista?
# Los que tienen un valor alto de ids son porque posiblemente no registren
# un id de una persona juridica sino de varias personas que pertenecen 
# a dicha entidad.
temp <- contratos %>%
 group_by(razon_social_contratista, id_contratista) %>% count() %>% 
 ungroup(id_contratista) %>% 
 mutate(N = sum(n), p = n / N * 100, ids = n()) %>% 
 slice_max(p, with_ties = F) %>% filter(ids > 1) %>% arrange(desc(ids))

# A partir de la distribución de las proporciones se puede establecer que
# existe una gran porción donde se emplea en mayor instancia un solo id.
ggplot(temp, aes(x = p)) + geom_histogram(bins = 100)

# Analisis de intervalor
sum(temp$p < 50) # 1.67%
sum(temp$p == 50) # 22.3%
sum(temp$p > 50) # 76%

# Con respecto a los que están en el 50% se tomara la siguiente regla:
# se cambiará el id solo si es un tipo de identificacion nit de persona juri.
temp <- temp %>% filter(p >= 50) %>% 
        select(razon_social_contratista, id_contratista, p) %>% 
        rename(id = id_contratista)

# Ajuste en la tabla principal
contratos <- contratos %>%
 left_join(temp, by = 'razon_social_contratista') %>% 
 mutate(id_contratista = if_else((!is.na(id) & p > 50) | 
  (!is.na(id) & (tipo_id_contratista == 'Nit de Persona Jurídica' & p == 50)),
         id_contratista, id_contratista)) %>% select(-c('id', 'p'))

# 4. Escritura de datos ----
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))
