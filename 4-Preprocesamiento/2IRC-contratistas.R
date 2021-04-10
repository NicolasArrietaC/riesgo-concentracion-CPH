# Generación de indicadores de riesgo de corrupción para contratistas
# ****************************************************************************
# Objetivo: Construccion de indicadores de riesgo de corrupción para
# cada contratistas presente en los contratos hospitalarios públicos
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'lubridate'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_csv(paste0(direccion, 'secop_i_ips.csv.gz'),
                      locale = locale(encoding = 'UTF-8'))
# Actividad de los contratistas
act_contratistas <- read_csv(paste0(direccion, 'actividad_contratistas.csv'),
                             locale = locale(encoding = 'UTF-8'))

# 3. Conjunto de datos base ----
# parámetros
# Número de contratos mínimos
cont_min <- 2 # Por lo menos dos contratos entre los 6 años
rel_min <- 2 # relación minima con entidades

# Consulta
ind_cont <- contratos %>% group_by(id_contratista_std) %>% 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones),
           val_contratos_media = mean(valor_total_con_adiciones),
           val_contratos_median = median(valor_total_con_adiciones),
           val_contratos_desv = sd(valor_total_con_adiciones),
           num_familias_dif = n_distinct(nombre_familia),
           num_grupos_dif = n_distinct(nombre_grupo),
           num_entidades_dif = n_distinct(nit_entidad),
           num_annos_activo = n_distinct(year(fecha_firma_contrato))) %>% 
 filter(num_contratos >= cont_min, num_entidades_dif >= rel_min)

# Remover contratistas que no entraron en el filtro
# SECOP
contratos <- contratos %>% 
 filter(id_contratista_std %in% ind_cont$id_contratista_std)
# Act. contratistas
act_contratistas <- act_contratistas %>% 
  filter(id_contratista_std %in% ind_cont$id_contratista_std)

# Agregación del indicador del número de municipios presentes
act_contratistas <- act_contratistas %>% 
  group_by(id_contratista_std) %>% 
  summarise(num_municipios_dif = n())

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(act_contratistas, by = 'id_contratista_std')

# 4. Creación de indicadores de riesgo ----
# 4.1. Indicadores por falta de competencia ----
# 4.1.1. Indice Herfindahl–Hirschman ----
# Salida: num_HHI: indice HHI de entidades por número de contratos 
#         val_HHI: indice HHI de entidades por valor de contratos
temp <- contratos %>% group_by(id_contratista_std, nit_entidad) %>% 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones)) %>% 
 ungroup(nit_entidad) %>% 
 mutate(si2_num = (num_contratos / sum(num_contratos) * 100) ^ 2,
        si2_val = (val_contratos / sum(val_contratos) * 100) ^ 2) %>%
 summarise(num_HHI = sum(si2_num) / 100,
           val_HHI = sum(si2_val) / 100)

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# 4.1.2. Porcentaje de contratación directa o regimen especial ----
# Salida: pnum_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
#         pval_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
temp <- contratos %>% 
 mutate(es_cc = tipo_proceso_cod %in% c(4), # códigos de cd y re
        valor_total = valor_total_con_adiciones * es_cc) %>% 
 group_by(id_contratista_std) %>% 
 summarise(pnum_cont_cerrada = sum(es_cc) / n() * 100,
  pval_cont_cerrada = sum(valor_total) / sum(valor_total_con_adiciones) * 100)

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# 4.1.3. Porcentaje de entidades por cada 100 contratos ----
# Salida: pentidades: porcentaje de entidades por cada 100 contratos*
#         *(se toma inverso para mantener el mismo sentido que los otros ind)
ind_cont <- ind_cont %>% 
 mutate(pnum_entidades = (num_entidades_dif / num_contratos) * 100)

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
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# Calculo de pval_ic4k
temp <- contratos %>% 
 group_by(id_contratista_std, nit_entidad) %>% 
 summarise(v = sum(valor_total_con_adiciones)) %>% 
 ungroup(nit_entidad) %>% 
 mutate(Pi = v / sum(v)) %>% 
 arrange(desc(Pi)) %>% slice_head(n = 1) %>%
 summarise(pval_ic1k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# 4.2. Indicadores de Violaciones o anomalías en los procesos de compra ----
# 4.2.1. Porcentaje de modificaciones a los contratos en tiempo ----
# Salida: pnum_adicion_tiempo: porcentaje de contratos con adición en tiempo
#         pval_adicion_tiempo: porcentaje de dias adicionales on relación 
#         a todos los plazos de los contratos
temp <- contratos %>% 
 mutate(hay_adicion = adiciones_dias > 0,
        plazo_total = adiciones_dias + plazo_ejec_contrato) %>% 
 group_by(id_contratista_std) %>%
 summarise(pnum_adicion_tiempo = sum(hay_adicion) / n() * 100,
           pval_adicion_tiempo = sum(adiciones_dias) / sum(plazo_total) * 100)

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# 4.2.2. Porcentaje de modificaciones a los contratos en valor ----
# Salida: pnum_adicion_cuantia: porcentaje de contratos con adición en cuantía
#         pval_adicion_cuantia: porcentaje de dinero en adición con 
#         relación a toda la cuentía de los contratos.
temp <- contratos %>% 
 mutate(hay_adicion = valor_adiciones > 0) %>% 
 group_by(id_contratista_std) %>%
 summarise(pnum_adicion_cuantia = sum(hay_adicion) / n() * 100,
           pval_adicion_cuantia = sum(valor_adiciones)/
            sum(valor_total_con_adiciones) * 100)

# Agregación al conjunto de datos
ind_cont <- ind_cont %>% left_join(temp, by = 'id_contratista_std')

# 5. Escritura del conjunto de datos ----
direccion <- '1-1-Datasets/contratistas/'
write_csv(ind_cont, paste0(direccion, '1A_contratistas.csv'))
