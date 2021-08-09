# Indicadores de terridata
# ****************************************************************************
# Objetivo: Generación del indicadores territoriales de la fuente de
# terridata
# Dimensión: Educación
# Realizado por: Nicolas Arrieta y Lizeth Jerez
# ****************************************************************************
# 1. Librerias ----
sapply(
 c('readr', 'dplyr', 'tidyr', "stringr", 'skimr'), 
 require, character.only = T
)

# 2. Lectura de datos ----
direccion <- '1-2-Datasets_complementarios/'
# Conjunto de datos finanzas publicas
ind_fp <- read_csv(paste0(direccion, 'terr_indsel_financiero.csv'),
                   locale = locale(encoding = 'UTF-8')) 

# 3. Generación de indicadores para el caso de Geraldine y Yilber ----
# Wider conjunto de datos
ind_fp <- ind_fp %>% pivot_wider(names_from = 'indicador', 
                                   values_from = 'valor')

# Establecer rangos de tiempo
rango <- 2 # Años
# Número de años registrados
num_annos <- ind_fp$anno |> unique() |> length()
# Número de intervalos
num_intv <- num_annos/rango 

ind_fp <- ind_fp |> 
 mutate(intervalo_tiempo = cut(anno, num_intv, labels = 1:num_intv))

und_anl <- c("codigo_entidad", "intervalo_tiempo")

cols <- names(ind_fp)[4:((names(ind_fp) |> length()) - 1)]

ind_fp <- ind_fp |>
 group_by(across(all_of(und_anl))) |> 
 summarise_at(cols, mean, na.rm = T) |> 
  rename(inf_ingresos_m = "Ingresos totales",
         inf_financiamiento_m = "Financiamiento",
         inf_desempennofiscal_m = "Indicador de desempeño fiscal",
         inf_igpr_m = "Índice de gestión de proyectos de regalías (IGPR)",
         inf_nproyectos_m = "Número total de proyectos",
         inf_peducacion_m = "ppa - Educación",
         inf_psalud_m = "ppa - Salud",
         inf_regaliaspc_m = "Regalías per cápita (Valor efectivamente girado al municipio)",
         inf_vproyectos_m = "Valor del número total de proyectos")

# Medida absoluta

cols <- names(ind_fp)[3:(names(ind_fp) |> length())]

ind_fp_abs <- ind_fp |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_fp_abs <-  ind_fp_abs %>% replace(., is.na(.), 0)

# Remover columnas que redundantes
ind_fp_abs <- ind_fp_abs |> 
 select(-inf_vproyectos_m_3, -inf_igpr_m_1)

# Medida relativa

ind_fp_r <- ind_fp |> mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
                               intervalo_tiempo = as.factor(intervalo_tiempo))

names(ind_fp_r) <- c(names(ind_fp_r)[1:2], 
                      paste0(names(ind_fp_r)[3:(names(ind_fp_r) |> length())], "1"))

ind_fp_r <- ind_fp |> 
 left_join(ind_fp_r, by = und_anl) |> 
 filter(!is.na(inf_ingresos_m1))

for(col in cols){
 rate <- round(ind_fp_r[, col] / ind_fp_r[, paste0(col, "1")] - 1, 4) * 100
 ind_fp_r[, paste0(col, "_r")] <- rate
}

ind_fp_r <- ind_fp_r |> 
 select(all_of(und_anl), contains("_r"))

cols <- names(ind_fp_r)[4:(names(ind_fp_r) |> length())]

ind_fp_r <- ind_fp_r |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_fp_r <-  ind_fp_r %>% replace(., is.na(.), 0)
ind_fp_r <-  ind_fp_r %>% replace(., . == as.numeric("Inf"), 0)

# Remover columnas redundantes
ind_fp_r <- ind_fp_r |> 
 select(-inf_vproyectos_m_r_3, -inf_igpr_m_r_2)

# 4. Escritura de datos ----
direccion <- '1-1-Datasets/terridata/'
write_csv(ind_fp_abs, paste0(direccion, '2_terr_indsel_financiero.csv'))
write_csv(ind_fp_r, paste0(direccion, '3_terr_indsel_financiero.csv'))
