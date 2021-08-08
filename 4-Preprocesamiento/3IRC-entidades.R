# Generación de indicadores de riesgo de corrupción para entidades
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

contratos[contratos$nit_entidad == "ND36", "orden_entidad"] <- 5

# 3. Conjunto de datos base ----
# parámetros
# Número de contratos mínimos
cont_min <- 6 # Por lo menos un contrato por año en promedio
rel_min <- 5 # relación minima con contratistas

# Consulta
grupo_ent <- contratos |> 
 group_by(nit_entidad) |> 
 summarize(num_contratos = n(),
           num_contratistas_dif = n_distinct(id_contratista_std)) |> 
 filter(num_contratos >= cont_min,
        num_contratistas_dif >= rel_min)

# Remover entidades que no entraron en el filtro
contratos <- contratos %>% filter(nit_entidad %in% grupo_ent$nit_entidad)

# Establecer rangos de tiempo
rango <- 2 # Años
# Número de años registrados
num_annos <- year(contratos$fecha_firma_contrato) |> unique() |> length()
# Número de intervalos
num_intv <- num_annos/rango 

start_time <- Sys.time()

# Agregación de variables temporales
contratos <- contratos |> 
  mutate(anno_firma = year(fecha_firma_contrato),
         intervalo_tiempo = cut(anno_firma, num_intv, labels = 1:num_intv))

und_anl <- c("nit_entidad", "intervalo_tiempo")

ind_ent <- contratos |>
 group_by(across(all_of(und_anl)), orden_entidad) |> 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones),
           val_contratos_media = mean(valor_total_con_adiciones),
           val_contratos_median = median(valor_total_con_adiciones),
           val_contratos_desv = sd(valor_total_con_adiciones),
           num_familias_dif = n_distinct(nombre_familia),
           num_grupos_dif = n_distinct(nombre_grupo),
           num_contratistas_dif = n_distinct(id_contratista_std))

end_time <- Sys.time()

print(paste("Process time for ind_ent: ", round(end_time - start_time, 2)))

start_time <- Sys.time()

cols <- names(ind_ent)[4:(names(ind_ent) |> length())]

ind_ent_abs <- ind_ent |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_ent_abs <-  ind_ent_abs %>% replace(., is.na(.), 0)

end_time <- Sys.time()

print(paste("Process time for ind_ent_abs: ", round(end_time - start_time, 2)))

# Medida cambio
start_time <- Sys.time()

ind_ent_r <- ind_ent |> mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
                               intervalo_tiempo = as.factor(intervalo_tiempo)) |> 
  select(-orden_entidad)

names(ind_ent_r) <- c(names(ind_ent_r)[1:2], 
                      paste0(names(ind_ent_r)[3:(names(ind_ent_r) |> length())], "1"))

ind_ent_r <- ind_ent |> 
 left_join(ind_ent_r, by = und_anl) |> 
 filter(!is.na(num_contratos1))

for(col in cols){
 rate <- round(ind_ent_r[, col] / ind_ent_r[, paste0(col, "1")] - 1, 4) * 100
 ind_ent_r[, paste0(col, "_r")] <- rate
}

ind_ent_r <- ind_ent_r |> 
 select(all_of(und_anl), orden_entidad, contains("_r"))

cols <- names(ind_ent_r)[4:(names(ind_ent_r) |> length())]

ind_ent_r <- ind_ent_r |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

end_time <- Sys.time()

ind_ent_r <-  ind_ent_r %>% replace(., is.na(.), 0)
ind_ent_r <-  ind_ent_r %>% replace(., . == as.numeric("Inf"), 0)

print(paste("Process time for ind_ent_r: ", round(end_time - start_time, 2)))

# 4. Creación de indicadores de riesgo ----
# 4.1. Indicadores por falta de competencia ----
# 4.1.1. Indice Herfindahl–Hirschman ----
# Salida: num_HHI: indice HHI de contratistas por número de contratos 
#         val_HHI: indice HHI de contratistas por valor de contratos
temp <- contratos %>% group_by(nit_entidad, id_contratista_std) %>% 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones)) %>% 
 ungroup(id_contratista_std) %>% 
 mutate(si2_num = (num_contratos / sum(num_contratos) * 100) ^ 2,
        si2_val = (val_contratos / sum(val_contratos) * 100) ^ 2) %>%
 summarise(num_HHI = sum(si2_num) / 100,
           val_HHI = sum(si2_val) / 100)

# Agregación al conjunto de datos
ind_ent_abs <- ind_ent_abs %>% left_join(temp, by = 'nit_entidad')
ind_ent_r <- ind_ent_r %>% left_join(temp, by = 'nit_entidad')

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
ind_ent_abs <- ind_ent_abs %>% left_join(temp1, by = 'nit_entidad')

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
ind_ent_r <- ind_ent_r %>% left_join(temp2, by = 'nit_entidad')

# 4.1.3. Porcentaje de empresas ganadoras por cada 100 contratos ----
# Salida: pnum_ganadoras: porcentaje de empresas ganadoras por
#         cada 100 contratos*
#         *(se toma inverso para mantener el mismo sentido que los otros ind)

# Absoluto
temp <- ind_ent |> 
 mutate(pnum_ganadoras = (num_contratistas_dif / num_contratos) * 100) |> 
  select(all_of(und_anl), pnum_ganadoras)

temp2 <- temp |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = pnum_ganadoras,
              values_fill = 0,
              names_prefix = "pnum_ganadoras_")

  
# Agregación al conjunto de datos absoluto
ind_ent_abs <- ind_ent_abs %>% left_join(temp2, by = 'nit_entidad')

# Indicador relativo

temp1 <- temp |> 
  mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
         intervalo_tiempo = as.factor(intervalo_tiempo))


names(temp1) <- c(names(temp1)[1:2], 
                  paste0(names(temp1)[3:(names(temp1) |> length())], "1"))

temp <- temp |> left_join(temp1, und_anl) |> 
  filter(!is.na(pnum_ganadoras1)) |> 
  mutate(pnum_ganadoras_r = (pnum_ganadoras / pnum_ganadoras1 - 1) * 100) |>
  select(all_of(und_anl), pnum_ganadoras_r) |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = pnum_ganadoras_r,
              values_fill = 0,
              names_prefix = "pnum_ganadoras_r_")

# Agregación al conjunto de datos relativo
ind_ent_r <- ind_ent_r %>% left_join(temp, by = 'nit_entidad')

# 4.1.4. Indice IC4K ----
# Salida: pnum_ic4k: porcentaje de la cantidad de contratos de los cuatro
#         contratistas más representativos en la entidad.
#         pval_ic4k: porcentaje del valor de contratos de los cuatro
#         contratistas más representativos en la entidad.
# Calculo de pnum_ic4k
temp <- contratos %>% 
 group_by(nit_entidad, id_contratista_std) %>% count() %>% 
 ungroup(id_contratista_std) %>% 
 mutate(Pi = n / sum(n)) %>%
 arrange(desc(Pi)) %>% slice_head(n = 4) %>%
 summarise(pnum_ic4k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_ent_abs <- ind_ent_abs %>% left_join(temp, by = 'nit_entidad')
ind_ent_r <- ind_ent_r %>% left_join(temp, by = 'nit_entidad')

# Calculo de pval_ic4k
temp <- contratos %>% 
 group_by(nit_entidad, id_contratista_std) %>% 
 summarise(v = sum(valor_total_con_adiciones)) %>% 
 ungroup(id_contratista_std) %>% 
 mutate(Pi = v / sum(v)) %>% 
 arrange(desc(Pi)) %>% slice_head(n = 4) %>%
 summarise(pval_ic4k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_ent_abs <- ind_ent_abs %>% left_join(temp, by = 'nit_entidad')
ind_ent_r <- ind_ent_r %>% left_join(temp, by = 'nit_entidad')

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
ind_ent_abs <- ind_ent_abs %>% left_join(temp2, by = 'nit_entidad')

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
ind_ent_r <- ind_ent_r %>% left_join(temp1, by = 'nit_entidad')

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
ind_ent_abs <- ind_ent_abs %>% left_join(temp2, by = 'nit_entidad')

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
ind_ent_r <- ind_ent_r %>% left_join(temp1, by = 'nit_entidad')

# 5. Escritura del conjunto de datos ----
# Conjunto de indicadores de entidades absoluto
direccion <- '1-1-Datasets/entidades/'
write_csv(ind_ent_abs, paste0(direccion, '2A_entidades.csv'))

# Conjunto de indicadores de entidades relativo
write_csv(ind_ent_r, paste0(direccion, '3A_entidades.csv'))
