# Generación de indicadores de riesgo de corrupción para entidades
# ****************************************************************************
# Objetivo: Construccion de indicadores de riesgo de corrupción para
# cada entidad hospitalaria, a partir de los datos transaccionales.
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('dplyr', 'readr'), 
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
cont_min <- 6 # Por lo menos un contrato por año en promedio
rel_min <- 5 # relación minima con contratistas

# Consulta
ind_ent <- contratos %>% group_by(nit_entidad) %>% 
 summarise(num_contratos = n(),
           val_contratos = sum(valor_total_con_adiciones),
           val_contratos_media = mean(valor_total_con_adiciones),
           val_contratos_median = median(valor_total_con_adiciones),
           val_contratos_desv = sd(valor_total_con_adiciones),
           num_familias_dif = n_distinct(nombre_familia),
           num_grupos_dif = n_distinct(nombre_grupo),
           num_contratistas_dif = n_distinct(id_contratista_std)) %>% 
 filter(num_contratos >= cont_min,
        num_contratistas_dif >= rel_min)

# Remover entidades que no entraron en el filtro
contratos <- contratos %>% filter(nit_entidad %in% ind_ent$nit_entidad)

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
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# 4.1.2. Porcentaje de contratación directa o regimen especial ----
# Salida: pnum_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
#         pval_cont_cerrada: porcentaje de contratos donde su tipo de proceso
#         está marcado como contratacion directa o regimen especial.
temp <- contratos %>% 
 mutate(es_cd_o_re = tipo_proceso_cod %in% c(4,7), # códigos de cd y re
        valor_total = valor_total_con_adiciones * es_cd_o_re) %>% 
 group_by(nit_entidad) %>% 
 summarise(pnum_cont_cerrada = sum(es_cd_o_re) / n() * 100,
   pval_cont_cerrada = sum(valor_total) / sum(valor_total_con_adiciones) * 100)

# Agregación al conjunto de datos
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# 4.1.3. Porcentaje de empresas ganadoras por cada 100 contratos ----
# Salida: pnum_ganadoras: porcentaje de empresas ganadoras por
#         cada 100 contratos*
#         *(se toma inverso para mantener el mismo sentido que los otros ind)
ind_ent <- ind_ent %>% 
 mutate(pnum_ganadoras = (num_contratistas_dif / num_contratos) * 100)

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
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# Calculo de pval_ic4k
temp <- contratos %>% 
 group_by(nit_entidad, id_contratista_std) %>% 
 summarise(v = sum(valor_total_con_adiciones)) %>% 
 ungroup(id_contratista_std) %>% 
 mutate(Pi = v / sum(v)) %>% 
 arrange(desc(Pi)) %>% slice_head(n = 4) %>%
 summarise(pval_ic4k = sum(Pi) * 100)

# Agregación al conjunto de datos
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# 4.2. Indicadores de Violaciones o anomalías en los procesos de compra ----
# 4.2.1. Porcentaje de modificaciones a los contratos en tiempo ----
# Salida: pnum_adicion_tiempo: porcentaje de contratos con adición en tiempo
#         pval_adicion_tiempo: porcentaje de dias adicionales on relación 
#         a todos los plazos de los contratos
temp <- contratos %>% 
 mutate(hay_adicion = adiciones_dias > 0,
        plazo_total = adiciones_dias + plazo_ejec_contrato) %>% 
 group_by(nit_entidad) %>%
 summarise(pnum_adicion_tiempo = sum(hay_adicion) / n() * 100,
           pval_adicion_tiempo = sum(adiciones_dias) / sum(plazo_total) * 100)

# Agregación al conjunto de datos
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# 4.2.2. Porcentaje de modificaciones a los contratos en valor ----
# Salida: pnum_adicion_cuantia: porcentaje de contratos con adición en cuantía
#         pval_adicion_cuantia: porcentaje de dinero en adición con 
#         relación a toda la cuentía de los contratos.
temp <- contratos %>% 
 mutate(hay_adicion = valor_adiciones > 0) %>% 
 group_by(nit_entidad) %>%
 summarise(pnum_adicion_cuantia = sum(hay_adicion) / n() * 100,
  pval_adicion_cuantia = sum(valor_adiciones)/
                          sum(valor_total_con_adiciones) * 100)

# Agregación al conjunto de datos
ind_ent <- ind_ent %>% left_join(temp, by = 'nit_entidad')

# 5. Escritura del conjunto de datos ----
direccion <- '1-1-Datasets/entidades/'
write_csv(ind_ent, paste0(direccion, '1A_entidades.csv'))
