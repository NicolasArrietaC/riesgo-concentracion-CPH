# Caso de posibles errores en el valor de los contratos
# ****************************************************************************
# Objetivo: corregir, eliminar o detectar casos donde se presente un error
# en las variables relacionadas con el valor de los contratos.
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('dplyr', 'readr', 'ggplot2', 'stringr'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos SECOP
contratos <- read_csv(paste0(direccion, 'secop_i_ips.csv.gz'),
                      locale = locale(encoding = 'UTF-8'))
# contratos validados en la consultoria
cont_validados <- read_csv(paste0(direccion, 
                       'correccion_contratos_valor_secop_i.csv'))

# 3. Analisis de problematicas ----
# 3.1. Valores bajos ----
## Valores cero
# Existen 902 contratos con valor total igual a cero
sum(contratos$valor_total_con_adiciones == 0)
# Existen 589 contratos con cuantia de proceso igual a cero
sum(contratos$cuantia_proceso == 0)
# Existen 902 contratos con valor total igual a cero y cuantia del contrato
sum(contratos$valor_total_con_adiciones == 0 & 
     contratos$cuantia_contrato == 0)
# Existen 494 contratos con valor total igual a cero y cuantia de proceso
sum(contratos$valor_total_con_adiciones == 0 & 
     contratos$cuantia_proceso == 0)

# Conclusión parcial: por ahora se pueden remover los contratos con valor
# igual a cero, porque no hay evidencias clarar de la causa del problema,
# puede que el valor ya este contando en otro contrato que lleve el mismo
# valor del proceso.

# Valores cercano al cero (hasta 1 millon)
# Se observa una distribucion con doble curvatura, con valores al rededor
# de los 100 mil pesos y valores cercanos al millon de pesos.
# El pico minimo, sin contar el cero, esta en 500 mil, por lo tanto, se
# establece como el punto de corte.
temp <- contratos %>% filter(valor_total_con_adiciones <= 1e6) %>% 
 select(cuantia_proceso, cuantia_contrato, 
        valor_adiciones, valor_total_con_adiciones)

ggplot(temp, aes(x = valor_total_con_adiciones)) + geom_density()

# 4. Limpieza de contratos ----
# 4.1. Correccion del valor de contratos ----
# 4.1.1. Cruce de contratos con el conjunto de validacion
# Actividades previas
# Contraer el link del conjunto de datos validados
estandar_link <- paste0('https://www.contratos.gov.co/consultas/',
                        'detalleProceso.do\\?numConstancia=')
cont_validados$ruta_web <- str_remove(cont_validados$ruta_web, 
                                      estandar_link)

# Obtención de los procesos que comparten enlace
url_conjunto <- contratos %>% group_by(ruta_web) %>% count() %>% 
 filter(n > 1) %>% arrange(desc(n)) %>% select(ruta_web)
# Numero de casos: 4627
url_conjunto <- url_conjunto[[1]]

# Extraer los registros que se pueden modificar
temp <- contratos %>% 
 select(cuantia_proceso, cuantia_contrato, 
        valor_adiciones, valor_total_con_adiciones, ruta_web) %>% 
 inner_join(x = ., y = cont_validados) %>% 
 mutate(hay_error = error_cuantia_proceso + 
         error_cuantia_contrato + error_adicion,
        cuantia_proceso_real = if_else(cuantia_proceso_real == 1 | 
                                   is.na(cuantia_proceso_real), 
                                       cuantia_proceso, cuantia_proceso_real),
        cuantia_contrato_real = if_else(is.na(cuantia_contrato_real), 
                                   cuantia_contrato, cuantia_contrato_real),
        valor_adicion_real = valor_adiciones) %>%
 filter(!(ruta_web %in% url_conjunto), hay_error > 0) %>% 
 select(ruta_web, contains('real')) %>% distinct()

# Asignación al conjunto completo
for (i in temp$ruta_web) {
 # Cambio por atributo
 contratos[contratos$ruta_web == i, 'cuantia_proceso'] <- 
  temp[which(temp$ruta_web == i), 'cuantia_proceso_real'][[1]]
 
 inicial <- temp[which(temp$ruta_web == i), 
                 'cuantia_contrato_real'][[1]]
 contratos[contratos$ruta_web == i, 'cuantia_contrato'] <- inicial
 
 adicion <- temp[which(temp$ruta_web == i), 
                 'valor_adicion_real'][[1]]
 contratos[contratos$ruta_web == i, 'valor_adiciones'] <- adicion
 
 
 contratos[contratos$ruta_web == i, 'valor_total_con_adiciones'] <- 
  inicial + adicion
}
# 5. Eliminación de registros ----
# 5.1. Filtro de contratos con valor bajo (analisis 3.2.2)
# Con este filtro se eliminan 100717 registros.
contratos <- contratos %>% 
 filter(valor_total_con_adiciones > 5e5 | moneda != 'Pesos (COP)')

# 6. Busqueda de contratos con posibles errores ----
# 6.1. Mediante el uso de las consultas de deteccion de anomalias
temp <- contratos %>% 
 filter(!(ruta_web %in% temp$ruta_web | ruta_web %in% url_conjunto),
        valor_total_con_adiciones >= 
         quantile(contratos$valor_total_con_adiciones, 
                  probs = 0.9, names = F)) %>% 
 mutate(racha_9ceros = (valor_total_con_adiciones %% 1e9 == 0 &
                         valor_total_con_adiciones > 0),
     t_cuantia_proceso = cuantia_proceso / valor_total_con_adiciones * 100,
     t_valor_adiciones = valor_adiciones / valor_total_con_adiciones * 100,
     ruta_web = paste0(estandar_link, ruta_web)) %>%
 select(cuantia_proceso, cuantia_contrato, valor_adiciones,
        valor_total_con_adiciones, t_cuantia_proceso, t_valor_adiciones, 
        racha_9ceros, nombre_entidad, razon_social_contratista,
        ruta_web) %>% arrange(desc(valor_total_con_adiciones)) 
 

# 6. Escritura de datos ----
write_csv(contratos, paste0(direccion, 'secop_i_ips.csv.gz'))
# Extraccion de datos para revision
write_csv(temp, paste0(direccion, 'rv_valores_altos.csv'))
