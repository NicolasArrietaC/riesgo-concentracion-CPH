# ANÁLISIS GENERAL DE CALIDAD DE DATOS 
# ****************************************************************************
# Objetivo: Descripción de los datos relevantes a la calidad de datos
# sobre el conjunto de datos  contratación pública hospitalaria, además de
# la solución de casos de calidad no tan complejos.
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'stringr', 'tibble', 'lubridate'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_delim(paste0(direccion, 'secop_i_ips_base.csv.gz'),
                        delim = ';', locale = locale(encoding = 'UTF-8'), 
                        col_types = cols_only(
                         nit_entidad = col_character(),
                         nombre_entidad = col_character(),
                         tipo_id_contratista = col_character(),
                         id_contratista = col_character(),
                         razon_social_contratista = col_character(),
                         tipo_doc_representate_legal = col_character(),
                         id_representante_legal = col_character(),
                         nombre_representante_legal = col_character(),
                         cuantia_proceso = col_double(),
                         valor_adiciones = col_double(),
                         cuantia_contrato = col_double(),
                         valor_total_con_adiciones = col_double(),
                         moneda = col_character(),
                         ruta_web = col_character(),
                         tipo_proceso = col_character(),
                         regimen_contratacion = col_character(),
                         tipo_contrato = col_character(),
                         municipio_ejecucion = col_character(),
                         nombre_grupo = col_character(),
                         nombre_familia = col_character(),
                         dpto_mcpo_contratista = col_character(),
                         fecha_firma_contrato = col_character(),
                         plazo_ejec_contrato = col_number(),
                         rango_ejec_contrato = col_character(),
                         tiempo_adiciones_dias = col_number(),
                         tiempo_adiciones_meses = col_number(),
                         municipio_entidad = col_character(),
                         departamento_entidad = col_character()))

# 3. Problemas de Calidad de datos ----
# Indicadores generales
# Existen 1391442 contratos en el conjunto de datos inicialmente
n_cont <- nrow(contratos)
# Todos los datos suman 51.47 billones de pesos
v_cont <- round(sum(contratos$valor_total_con_adiciones) / 1e12, 2)
# El promedio por contrato está en 36.99 M
m_cont <- mean(contratos$valor_total_con_adiciones)
# La desviación estándar está en 7.77 MM
sd_cont <- sd(contratos$valor_total_con_adiciones)
# El coeficiente de variación está en 21.014 %
cv_cont <- sd_cont/m_cont * 100

ind_calidad <- tibble(
 indicador = c('Cantidad de contratos', 'Valor total de contratos', 
               'Media valor total', 'Desviación E. valor total',
               'Coeficiente variación valor total'),
 clase = rep('General', 5),
 valor_inicial = c(n_cont, v_cont, m_cont, sd_cont, cv_cont)
 )

# 3.1. Duplicados ---- 
# Se encontraron 652 registros duplicados
n_cont - nrow(contratos %>% distinct())

# 3.2. Valor de los contratos ----
# 3.2.1. Consultas detección de anomalias ----
# Generacion de 3 consultas
## 1. racha de 9 ceros a al derecha del valor
## 2. tasa de cuantía de proceso sobre valor total
## 3. tasa de adiciones sobre valor total
temp <- contratos %>% 
 mutate(racha_9ceros = (valor_total_con_adiciones %% 1e9 == 0 & 
                         valor_total_con_adiciones > 0),
        t_cuantia_proceso = cuantia_proceso / valor_total_con_adiciones * 100,
        t_valor_adiciones = valor_adiciones / valor_total_con_adiciones * 100,
        decil = cut(valor_total_con_adiciones, 
                    breaks = quantile(valor_total_con_adiciones, 
                                      probs = seq(0, 1, 0.1)),
                    labels = letters[seq(1, 10)], include.lowest = T)) %>%
 arrange(desc(valor_total_con_adiciones)) %>% 
 select(cuantia_proceso, cuantia_contrato, 
        valor_adiciones, valor_total_con_adiciones,
        t_cuantia_proceso, t_valor_adiciones, 
        racha_9ceros, decil)

# Resultados
# Rachas
# ¿Cuántos presentan la racha de 9 ceros a la derecha? R/ 139
n_9ceros <- sum(temp$racha_9ceros)
# ¿Todos se encuentran en el último decíl? R/ Si
temp %>% group_by(decil) %>% 
 summarise(n = sum(racha_9ceros),
           N = n(),
           p = n / N * 100) %>% select(-N)
# ¿Cuánto dinero representa los contratos implicados en esta racha?
# R/ 12.32 Bill (23.9%)
v_9ceros <- round(sum(temp[temp$racha_9ceros, 
                           'valor_total_con_adiciones'][[1]]) / 1e12, 2)

# Tasa proceso/valor total
pbajo <- 25; palto <- 250
# ¿Cuántos presentan una tasa exagerada por encima o por debajo? R/ 31051
n_tproc <- sum(temp$t_cuantia_proceso <= pbajo | 
                temp$t_cuantia_proceso >= palto, na.rm = T)
# ¿Cómo es su comportamiento por deciles?
# Hay mayor probabilidad de que este error suceda en valores altos del 
# contrato, debido a que en el ultimo decil alcanza hasta un 5.44%
temp %>% 
 mutate(posible_error = (t_cuantia_proceso <= pbajo | 
           t_cuantia_proceso >= palto)) %>% 
 group_by(decil) %>% 
 summarise(n = sum(posible_error, na.rm = T),
           N = n(),
           p = n / N * 100) %>% select(-N)
# ¿Cuánto dinero representa los contratos implicados en este posible error?
# R/. 19.13 bill (37.16%)
v_tproc <- round(sum(temp[temp$t_cuantia_proceso <= pbajo | 
            temp$t_cuantia_proceso >= palto, 'valor_total_con_adiciones'][[1]], 
                     na.rm = T)/1e12, 2)

# Tasa adicion/valor total
pbajo2 <- 3; palto2 <- 150
# ¿Cuantos presentan una tasa exagerada por encima o por debajo? R/ 2158 **
n_tadc <- sum(temp$t_valor_adiciones != 0 & 
        (temp$t_valor_adiciones <= pbajo2 | temp$t_valor_adiciones >= palto2), 
       na.rm = T)
# ¿Cómo es su comportamiento por deciles?
# Hay mayor probabilidad de que este error suceda en valores altos del 
# contrato, debido a que en el ultimo decil alcanza hasta un 0.6%
temp %>% 
 mutate(posible_error = (t_valor_adiciones != 0 & 
  (t_valor_adiciones <= pbajo2 | t_valor_adiciones >= palto2))) %>% 
 group_by(decil) %>%
 summarise(n = sum(posible_error, na.rm = T),
           N = n(),
           p = n / N * 100) %>% select(-N)
# ¿Cuánto dinero representa los contratos implicados en este posible error?
# R/. 1.04 Bill (2.02%)
v_tadc <- round(sum(temp[temp$t_valor_adiciones != 0 & 
        (temp$t_valor_adiciones <= pbajo2 | temp$t_valor_adiciones >= palto2), 
       'valor_total_con_adiciones'][[1]], na.rm = T) / 1e12, 2)

# 3.2.2. Valores extremos ----
# Valores atipicos
temp <- scale(contratos$valor_total_con_adiciones)
atipicos <- sum(temp > 3 | temp < -3)

# 3.2.* Generacion de indicadores ----
temp <- tibble(
 indicador = c('Cantidad contratos en racha de 9 ceros', 
               'Valor de contratos en racha de 9 ceros', 
               'Cantidad contratos en tasa alta/baja en valor proceso/cuantia', 
               'Valor de contratos en tasa alta/baja en valor proceso/cuantia',
               'Cantidad contratos en tasa alta/baja en valor adicion/cuantia', 
               'Valor de contratos en tasa alta/baja en valor adicion/cuantia',
               'Valores atípicos'),
 clase = rep('Posible error en valor', 7),
 valor_inicial = c(n_9ceros, v_9ceros, n_tproc, 
                   v_tproc, n_tadc, v_tadc, atipicos)
)
# Almacenar en la tabla matriz
ind_calidad <- rbind(ind_calidad, temp)

# 3.3. Identificación de actores del proceso ----
# 3.3.1. Entidades ----
# ¿Cuantas entidades hay?
n_entidades <- contratos %>% 
  select(nit_entidad) %>% 
  distinct() %>% count()

n_entidades <- n_entidades[[1]]

# ¿Cuantas entidades presentan diferentes nits?
# R/. Ninguna
contratos %>% group_by(nombre_entidad) %>% 
  summarise(nits = n_distinct(nit_entidad)) %>% 
  filter(nits > 1) %>% count()
# ¿Cuantas entidades presentan diferentes nombres para un unico nit?
# R/. Solo 1 y es cuando no registra
nit_noreg <- contratos %>% group_by(nit_entidad) %>% 
  summarise(nombres = n_distinct(nombre_entidad)) %>% 
  filter(nombres > 1) %>% select(nombres)

n_nit_na <- nit_noreg[[1]]

# ¿Cuánto dinero esta involucrado en los contratos con nit de entidad no reg?
v_nit_na <- round(sum(contratos[contratos$nit_entidad == "No registra", 
                      'valor_total_con_adiciones'][[1]]) / 1e12, 2)

# 3.3.2. Contratistas ----
# ¿Cuántos contratistas hay juntando nombre y nit? R/. 437130
contratos %>% 
  select(id_contratista, razon_social_contratista) %>% 
  distinct() %>% count()
# ¿Cuántos contratistas hay teniendo en cuenta el nombre? R/. 375563
contratos %>% 
  select(razon_social_contratista) %>% 
  distinct() %>% count()
# ¿Cuántos contratistas hay teniendo en cuenta el nit? R/. 291784
n_contratistas <- contratos %>% 
  select(id_contratista) %>% 
  distinct() %>% count()

n_contratistas <- n_contratistas[[1]]
# Lo mejor es seleccionar el nit para determinar la poblacion 
# de los contratistas

# ¿Cuantos contratistas presentan diferentes nits?
# R/. 46313 (15.87% de los nits de contratistas)
temp <- contratos %>% group_by(razon_social_contratista) %>% 
  summarise(nits = n_distinct(id_contratista)) %>% 
  filter(nits > 1)

n_varios_id <- nrow(temp)
# ¿Cuánto dinero representa ambos casos?
# R/. 18.52 Bill (35.98% del valor total de contratos)
v_varios_id <- round(sum(contratos[contratos$razon_social_contratista %in% 
      temp$razon_social_contratista, 'valor_total_con_adiciones'])/1e12 , 2)

# 3.3.* Generación de indicadores ----
temp <- tibble(
  indicador = c('Número de entidades',
                'Cantidad contratos con nit de entidad no registrado',
                'Valor de contratos con nit de entidad no registrado',
                'Número de contratistas', 
                'Número de contratistas con varios nits',
            'Valor de contratos involucrados en contratistas con varios nits'),
  clase = rep('Errores en identificacion de actores', 6),
  valor_inicial = c(n_entidades, n_nit_na, v_nit_na, 
                    n_contratistas, n_varios_id, v_varios_id)
)

# Almacenar en la tabla matriz
ind_calidad <- rbind(ind_calidad, temp)

# 4. Modificación de variables ----
# 4.1. Reducción de caracteres en ruta web
# Esta es la ruta estandar para acceder a cualquier contrato
 estandar_link <- paste0('https://www.contratos.gov.co/consultas/',
                         'detalleProceso.do\\?numConstancia=')
# Eliminacion de la cadena de caracteres estandar 
contratos$ruta_web <- str_remove_all(contratos$ruta_web, estandar_link)

# 4.2. Remover departamento del nombre de la entidad
contratos$nombre_entidad <- str_split(contratos$nombre_entidad, 
                                      ' - ', simplify = T)[, 2]

# 4.3. Estandarizacion del tipo de proceso
# Codificacion
# 1) Licitacion pública
# 2) Selección abreviada
# 3) Concurso de méritos
# 4) Contratación directa
# 5) Mínima cuantía
# 6) Asociación público privada
# 7) Régimen especial
contratos <- contratos %>% 
  mutate(tipo_proceso = str_to_lower(tipo_proceso),
         tipo_proceso_cod = case_when(
           str_detect(tipo_proceso, 
                      pattern = "licitación públ|obra públ|subasta") ~ 1,
           str_detect(tipo_proceso, "selección abreviada") ~ 2,
           str_detect(tipo_proceso, "concurso de méri") ~ 3,
           str_detect(tipo_proceso, "contratación dir|dos pa") ~ 4,
           str_detect(tipo_proceso, "mínima cuantía") ~ 5,
           str_detect(tipo_proceso, "público privada") ~ 6,
           str_detect(tipo_proceso, "régimen especial") ~ 7))

# Remover categoria original
contratos$tipo_proceso <- NULL

# 4.4. Estandarización del regimen de contratación
# Estatuto general de contratación (1)
# Regimen especial (2)
contratos$es_regimen_especial <- if_else(contratos$regimen_contratacion == 
                                           'Régimen Especial', 1, 0)

# Remover la columna de regimen de contratacion
contratos$regimen_contratacion <- NULL

# 4.5. Estandarizacion del tipo de contrato
# Codificación
# 1) Agregación de demanda
# 2) Prestación de Servicios
# 3) Compraventa
# 4) Concesión
# 5) Suministro
# 6) Obra
# 7) Consultoría
# 8) Otro tipo de contrato
contratos <- contratos %>% 
  mutate(tipo_contrato = str_to_lower(tipo_contrato),
    tipo_contrato_cod = case_when(
      str_detect(tipo_contrato, "marco|agregación de deman") ~ 1, 
      str_detect(tipo_contrato, "prestación de servicios") ~ 2,
      str_detect(tipo_contrato, "compraventa") ~ 3,
      str_detect(tipo_contrato, "concesión") ~ 4,
      str_detect(tipo_contrato, "suministro") ~ 5,
      str_detect(tipo_contrato, "obra") ~ 6,
      str_detect(tipo_contrato, "interventoría|consultoría") ~ 7,
      str_detect(tipo_contrato, "otro|no definido") ~ 8))

# Remover la columna de regimen de contratacion
contratos$tipo_contrato <- NULL

# 4.6. Tipo de moneda
# Transformación a dummy
contratos$son_cop <- if_else(contratos$moneda != 'Dólares (USD)', 1, 0)

# Remover la categoria de memoria
contratos$moneda <- NULL

# 4.7. Estandatizacion de plazo de ejecucion
contratos <- contratos %>% 
  mutate(plazo_ejec_contrato = if_else(rango_ejec_contrato == 'M' & 
                  rango_ejec_contrato < 120, 30, 1) * plazo_ejec_contrato)

# Remover la variable rango_ejec
contratos$rango_ejec_contrato <- NULL

# 4.8. Estandariación de las adiciones de tiempo
contratos$adiciones_dias <- contratos$tiempo_adiciones_dias + 
  contratos$tiempo_adiciones_meses * 30

# Remover las variables que mencionaban las adiciones
contratos$tiempo_adiciones_dias <- NULL
contratos$tiempo_adiciones_meses <- NULL

# 4.9. Clasificación del nombre de grupo
# 1) [A] Material Vivo Animal y Vegetal
# 2) [B] Materias Primas 
# 3) [C] Maquinaria, Herramientas, Equipo Industrial y Vehículos
# 4) [D] Componentes y Suministros
# 5) [E] Productos de Uso Final
# 6) [F] Servicios
# 7) [G] Terrenos, Edificios, Estructuras y vías
contratos <- contratos %>% 
  mutate(nombre_grupo = case_when(
           str_detect(nombre_grupo, "[A]") ~ '1', 
           str_detect(nombre_grupo, "[B]") ~ '2',
           str_detect(nombre_grupo, "[C]") ~ '3',
           str_detect(nombre_grupo, "[D]") ~ '4',
           str_detect(nombre_grupo, "[E]") ~ '5',
           str_detect(nombre_grupo, "[F]") ~ '6',
           str_detect(nombre_grupo, "[G]") ~ '7'))

# 4.10. Transformación de la variable fecha de firma
contratos$fecha_firma_contrato <- ymd(contratos$fecha_firma_contrato)

# 5. Eliminación de registros ----
# 5.1. Remover duplicados
# Se encontraron 652 registros duplicados
contratos <- contratos %>% distinct()

# 5.2. Remover contratos del año 2020
contratos <- contratos %>% filter(year(fecha_firma_contrato) <= 2019)

# 6. Escritura de datos ----
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))
write_csv(ind_calidad, paste0(direccion, 'ind_calidad.csv'))
