# Generación de indicadores de riesgo de corrupción para contratistas
# ****************************************************************************
# Objetivo: Construccion de indicadores de riesgo de corrupción para
# cada entidad hospitalaria, a partir de los datos transaccionales.
# Desagregados por tiempo
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'tidyr', 'stringr', 'lubridate'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_csv(paste0(direccion, 'secop_i_ips.csv.gz'),
                      locale = locale(encoding = 'UTF-8'))

# 3. Conjunto de datos base ----
# parámetros
# Número de contratos mínimos
cont_min <- 2 # Por lo menos un contrato por año en promedio
rel_min <- 3 # relación minima con entidades

# Consulta
grupo_cont <- contratos |> 
 group_by(id_contratista_std) |> 
 summarize(num_contratos = n(),
           num_contratistas_dif = n_distinct(nit_entidad)) |> 
 filter(num_contratos >= cont_min,
        num_contratistas_dif >= rel_min)

# Remover entidades que no entraron en el filtro
contratos <- contratos %>% 
  filter(id_contratista_std %in% grupo_cont$id_contratista_std)

# Establecer rangos de tiempo
rango <- 2 # Años
# Número de años registrados
num_annos <- year(contratos$fecha_firma_contrato) |> unique() |> length()
# Número de intervalos
num_intv <- num_annos/rango 

start_time <- Sys.time()

contratos <- contratos |> 
 mutate(anno_firma = year(fecha_firma_contrato),
        intervalo_tiempo = cut(anno_firma, num_intv, labels = 1:num_intv))

und_anl <- c("id_contratista_std", "intervalo_tiempo")

ind_cont <- contratos |> 
 group_by(across(all_of(und_anl))) |> 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones),
           val_contratos_media = mean(valor_total_con_adiciones),
           val_contratos_median = median(valor_total_con_adiciones),
           val_contratos_desv = sd(valor_total_con_adiciones),
           num_familias_dif = n_distinct(nombre_familia),
           num_grupos_dif = n_distinct(nombre_grupo),
           num_entidades_dif = n_distinct(nit_entidad))

end_time <- Sys.time()

print(paste("Process time for ind_cont: ", round(end_time - start_time, 2)))

start_time <- Sys.time()

cols <- names(ind_cont)[3:(names(ind_cont) |> length())]

ind_cont_abs <- ind_cont |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_cont_abs <-  ind_cont_abs %>% replace(., is.na(.), 0)

end_time <- Sys.time()

print(paste("Process time for ind_cont_abs: ", round(end_time - start_time, 2)))

# Medida cambio
start_time <- Sys.time()

ind_cont_r <- ind_cont |> mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
                               intervalo_tiempo = as.factor(intervalo_tiempo))

names(ind_cont_r) <- c(names(ind_cont)[1:2], 
                      paste0(names(ind_cont)[3:(names(ind_cont) |> length())], "1"))

ind_cont_r <- ind_cont |> 
 left_join(ind_cont_r, by = und_anl) |> 
 filter(!is.na(num_contratos1))

for(col in cols){
 rate <- round(ind_cont_r[, col] / ind_cont_r[, paste0(col, "1")] - 1, 4) * 100
 ind_cont_r[, paste0(col, "_r")] <- rate
}

ind_cont_r <- ind_cont_r |> 
 select(all_of(und_anl), contains("_r"))

cols <- names(ind_cont_r)[3:(names(ind_cont_r) |> length())]

ind_cont_r <- ind_cont_r |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

end_time <- Sys.time()

ind_cont_r <-  ind_cont_r %>% replace(., is.na(.), 0)
ind_cont_r <-  ind_cont_r %>% replace(., . == as.numeric("Inf"), 0)

print(paste("Process time for ind_cont_r: ", round(end_time - start_time, 2)))

# 4. Creación de indicadores de riesgo ----
# 4.1. Indicadores por falta de competencia ----
# 4.1.1. Indice Herfindahl–Hirschman ----
# Salida: num_HHI: indice HHI de contratistas por número de contratos 
#         val_HHI: indice HHI de contratistas por valor de contratos
temp <- contratos %>% group_by(id_contratista_std, nit_entidad) %>% 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones)) %>% 
 ungroup(nit_entidad) %>% 
 mutate(si2_num = (num_contratos / sum(num_contratos) * 100) ^ 2,
        si2_val = (val_contratos / sum(val_contratos) * 100) ^ 2) %>%
 summarise(num_HHI = sum(si2_num) / 100,
           val_HHI = sum(si2_val) / 100)

# Agregación al conjunto de datos
ind_cont_abs <- ind_cont_abs %>% left_join(temp, by = 'id_contratista_std')
ind_cont_r <- ind_cont_r %>% left_join(temp, by = 'id_contratista_std')

# 4.1.2. Porcentaje de contratación directa o regimen especial ----
# Salida: pnum_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
#         pval_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
temp <- contratos %>% 
 mutate(es_cc = tipo_proceso_cod %in% c(4), # códigos de cd y re
        valor_total = valor_total_con_adiciones * es_cc) %>% 
 group_by(across(all_of(und_anl))) |> 
 summarise(pnum_cont_cerrada = sum(es_cc) / n() * 100,
   pval_cont_cerrada = sum(valor_total) / sum(valor_total_con_adiciones) * 100)

# Indicador absoluto
# La distribución del indicador parece comportarse binomial, en el sentido que 
# los extremos tienen la mayoría de los datos, por lo tanto se decidio dividir
# la variable en dos domensiones 1: > 0, 0: no aplica

temp1 <- temp |> 
  mutate(b_cont_cerrada = if_else(pnum_cont_cerrada == 0, 0, 1)) |> 
  select(all_of(und_anl), b_cont_cerrada)|> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = b_cont_cerrada,
              values_fill = 0,
              names_prefix = "b_cont_cerrada_")

# Agregación al conjunto de datos absoluto
ind_cont_abs <- ind_cont_abs %>% left_join(temp1, by = 'id_contratista_std')

# Indicador relativo
temp2 <- temp |> 
  mutate(b_cont_cerrada = if_else(pnum_cont_cerrada == 0, 0, 1)) |> 
  select(all_of(und_anl), b_cont_cerrada)

temp3 <- temp2 |> 
  mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
         intervalo_tiempo = as.factor(intervalo_tiempo))

names(temp3) <- c(names(temp3)[1:2], 
                  paste0(names(temp3)[3:(names(temp3) |> length())], "1"))

temp2 <- temp2 |> left_join(temp3, und_anl) |> 
  filter(!is.na(b_cont_cerrada1)) |> 
  mutate(r_cont_cerrada = if_else(b_cont_cerrada==1 & b_cont_cerrada1==1, 1, 0)) |>
  select(all_of(und_anl), r_cont_cerrada) |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = r_cont_cerrada,
              values_fill = 0,
              names_prefix = "r_cont_cerrada_")

# Agregación al conjunto de datos relativo
ind_cont_r <- ind_cont_r %>% left_join(temp2, by = 'id_contratista_std')

# 4.1.3. Porcentaje de entidades por cada 100 contratos ----
# Salida: pentidades: porcentaje de entidades por cada 100 contratos*
#         *(se toma inverso para mantener el mismo sentido que los otros ind)

# Absoluto
temp <- ind_cont |> 
 mutate(pentidades = (num_entidades_dif / num_contratos) * 100) |> 
  select(all_of(und_anl), pentidades)

temp2 <- temp |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = pentidades,
              values_fill = 0,
              names_prefix = "pentidades_")

  
# Agregación al conjunto de datos absoluto
ind_cont_abs <- ind_cont_abs %>% left_join(temp2, by = 'id_contratista_std')

# Indicador relativo

temp1 <- temp |> 
  mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
         intervalo_tiempo = as.factor(intervalo_tiempo))


names(temp1) <- c(names(temp1)[1:2], 
                  paste0(names(temp1)[3:(names(temp1) |> length())], "1"))

temp <- temp |> left_join(temp1, und_anl) |> 
  filter(!is.na(pentidades1)) |> 
  mutate(pentidades_r = (pentidades / pentidades1 - 1) * 100) |>
  select(all_of(und_anl), pentidades_r) |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = pentidades_r,
              values_fill = 0,
              names_prefix = "pentidades_r_")

# Agregación al conjunto de datos relativo
ind_cont_r <- ind_cont_r %>% left_join(temp, by = 'id_contratista_std')

# 4.1.4. Indice IC1K ----
# Salida: pnum_ic1k: porcentaje de la cantidad de contratos de la primera
#         entidad más representativoa en el contratista
#         pval_ic1k: porcentaje del valor de contratos de la primera
#         entidad más representativa en el contratista.
# Calculo de pnum_ic1k
temp <- contratos %>% 
 group_by(id_contratista_std, nit_entidad) %>% count() %>% 
 ungroup(nit_entidad) %>% 
 mutate(Pi = n / sum(n)) %>%
 arrange(desc(Pi)) %>% slice_head(n = 1) %>%
 summarise(pnum_ic1k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_cont_abs <- ind_cont_abs %>% left_join(temp, by = 'id_contratista_std')
ind_cont_r <- ind_cont_r %>% left_join(temp, by = 'id_contratista_std')

# Calculo de pval_ic4k
temp <- contratos %>% 
 group_by(id_contratista_std, nit_entidad) %>% 
 summarise(v = sum(valor_total_con_adiciones)) %>% 
 ungroup(nit_entidad) %>% 
 mutate(Pi = v / sum(v)) %>% 
 arrange(desc(Pi)) %>% slice_head(n = 1) %>%
 summarise(pval_ic1k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_cont_abs <- ind_cont_abs %>% left_join(temp, by = 'id_contratista_std')
ind_cont_r <- ind_cont_r %>% left_join(temp, by = 'id_contratista_std')

# 4.2. Indicadores de Violaciones o anomalías en los procesos de compra ----
# 4.2.1. Porcentaje de modificaciones a los contratos en tiempo ----
# Salida: pnum_adicion_tiempo: porcentaje de contratos con adición en tiempo
#         pval_adicion_tiempo: porcentaje de dias adicionales on relación 
#         a todos los plazos de los contratos
# Absoluto
temp <- contratos %>% 
 mutate(hay_adicion = adiciones_dias > 0,
        plazo_total = adiciones_dias + plazo_ejec_contrato) %>% 
 group_by(across(all_of(und_anl))) %>%
 summarise(pnum_adicion_tiempo = sum(hay_adicion) / n() * 100,
           pval_adicion_tiempo = sum(adiciones_dias) / sum(plazo_total) * 100)

# Se considera por ocurrencia
temp1 <- temp |> 
  mutate(b_adicion_tiempo = if_else(pnum_adicion_tiempo > 0, 1, 0)) |> 
  select(all_of(und_anl), b_adicion_tiempo)

temp2 <- temp1 |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = b_adicion_tiempo,
              values_fill = 0,
              names_prefix = "b_adicion_tiempo_")

# Agregación al conjunto de datos
ind_cont_abs <- ind_cont_abs %>% left_join(temp2, by = 'id_contratista_std')

# Indice Relativo
temp2 <- temp1 |> 
  mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
         intervalo_tiempo = as.factor(intervalo_tiempo))

names(temp2) <- c(names(temp2)[1:2], 
                  paste0(names(temp2)[3:(names(temp2) |> length())], "1"))

temp1 <- temp1 |> left_join(temp2, und_anl) |> 
  filter(!is.na(b_adicion_tiempo1)) |> 
  mutate(r_adicion_tiempo = if_else(b_adicion_tiempo==1 & b_adicion_tiempo1==1, 1, 0)) |>
  select(all_of(und_anl), r_adicion_tiempo) |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = r_adicion_tiempo,
              values_fill = 0,
              names_prefix = "r_adicion_tiempo_")

# Agregación al conjunto de datos
ind_cont_r <- ind_cont_r %>% left_join(temp1, by = 'id_contratista_std')

# 4.2.2. Porcentaje de modificaciones a los contratos en valor ----
# Salida: pnum_adicion_cuantia: porcentaje de contratos con adición en cuantía
#         pval_adicion_cuantia: porcentaje de dinero en adición con 
#         relación a toda la cuentía de los contratos.
# Absoluto
temp <- contratos %>% 
 mutate(hay_adicion = valor_adiciones > 0) %>% 
 group_by(across(all_of(und_anl))) %>%
 summarise(pnum_adicion_cuantia = sum(hay_adicion) / n() * 100,
           pval_adicion_cuantia = sum(valor_adiciones)/
                          sum(valor_total_con_adiciones) * 100)

# Se considera por ocurrencia
temp1 <- temp |> 
  mutate(b_adicion_cuantia = if_else(pnum_adicion_cuantia > 0, 1, 0)) |> 
  select(all_of(und_anl), b_adicion_cuantia)

temp2 <- temp1 |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = b_adicion_cuantia,
              values_fill = 0,
              names_prefix = "b_adicion_cuantia_")

# Agregación al conjunto de datos
ind_cont_abs <- ind_cont_abs %>% left_join(temp2, by = 'id_contratista_std')

# Indice Relativo
temp2 <- temp1 |> 
  mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
         intervalo_tiempo = as.factor(intervalo_tiempo))

names(temp2) <- c(names(temp2)[1:2], 
                  paste0(names(temp2)[3:(names(temp2) |> length())], "1"))

temp1 <- temp1 |> left_join(temp2, und_anl) |> 
  filter(!is.na(b_adicion_cuantia1)) |> 
  mutate(r_adicion_cuantia = if_else(b_adicion_cuantia==1 & b_adicion_cuantia1==1, 1, 0)) |>
  select(all_of(und_anl), r_adicion_cuantia) |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = r_adicion_cuantia,
              values_fill = 0,
              names_prefix = "r_adicion_cuantia_")

# Agregación al conjunto de datos
ind_cont_r <- ind_cont_r %>% left_join(temp1, by = 'id_contratista_std')

# 5. Escritura del conjunto de datos ----
direccion <- '1-1-Datasets/contratistas/'
write_csv(ind_cont_abs, paste0(direccion, '2A_contratistas.csv'))
write_csv(ind_cont_r, paste0(direccion, '3A_contratistas.csv'))