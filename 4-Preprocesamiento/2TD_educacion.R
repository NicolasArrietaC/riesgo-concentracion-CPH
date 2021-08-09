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
# Conjunto de datos eduación
ind_edu <- read_csv(paste0(direccion, "terr_indsel_educacion.csv"),
                    locale = locale(encoding = "UTF-8"))

# 3. Generación de indicadores para el caso de Geraldine y Yilber ----
# Wider conjunto de datos
ind_edu <- ind_edu %>% pivot_wider(names_from = 'indicador', 
                                   values_from = 'valor')

# Establecer rangos de tiempo
rango <- 2 # Años
# Número de años registrados
num_annos <- ind_edu$anno |> unique() |> length()
# Número de intervalos
num_intv <- num_annos/rango 

ind_edu <- ind_edu |> 
 mutate(intervalo_tiempo = cut(anno, num_intv, labels = 1:num_intv))

und_anl <- c("codigo_entidad", "intervalo_tiempo")

cols <- names(ind_edu)[str_detect(names(ind_edu), "ine_*")]

ind_edu <- ind_edu |>
 group_by(across(all_of(und_anl))) |> 
 summarise_at(cols, mean, na.rm = T)

# Medida absoluta

cols <- names(ind_edu)[3:(names(ind_edu) |> length())]

ind_edu_abs <- ind_edu |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_edu_abs <-  ind_edu_abs %>% replace(., is.na(.), 0)

# Remover columnas que redundantes
ind_edu_abs <- ind_edu_abs |> 
 select(-ine_tasaAnalfabetismo_1, -ine_tasaAnalfabetismo_2,
        -ine_porcentajeAsistencia_1, -ine_porcentajeAsistencia_2)

# Medida relativa

ind_edu_r <- ind_edu |> mutate(intervalo_tiempo = as.numeric(intervalo_tiempo) + 1,
                               intervalo_tiempo = as.factor(intervalo_tiempo))

names(ind_edu_r) <- c(names(ind_edu_r)[1:2], 
                      paste0(names(ind_edu_r)[3:(names(ind_edu_r) |> length())], "1"))

ind_edu_r <- ind_edu |> 
 left_join(ind_edu_r, by = und_anl) |> 
 filter(!is.na(ine_coberturaNeta1))

for(col in cols){
 rate <- round(ind_edu_r[, col] / ind_edu_r[, paste0(col, "1")] - 1, 4) * 100
 ind_edu_r[, paste0(col, "_r")] <- rate
}

ind_edu_r <- ind_edu_r |> 
 select(all_of(und_anl), contains("_r"))

cols <- names(ind_edu_r)[4:(names(ind_edu_r) |> length())]

ind_edu_r <- ind_edu_r |> 
  pivot_wider(names_from = intervalo_tiempo, 
              values_from = all_of(cols),
              values_fill = 0)

ind_edu_r <-  ind_edu_r %>% replace(., is.na(.), 0)
ind_edu_r <-  ind_edu_r %>% replace(., . == as.numeric("Inf"), 0)

# Remover columnas redundantes
ind_edu_r <- ind_edu_r |> 
 select(-ine_tasaAnalfabetismo_r_2, -ine_tasaAnalfabetismo_r_3,
        -ine_porcentajeAsistencia_r_2, -ine_porcentajeAsistencia_r_3)

# 4. Escritura de datos ----
direccion <- '1-1-Datasets/terridata/'
write_csv(ind_edu_abs, paste0(direccion, '2_terr_indsel_educacion.csv'))
write_csv(ind_edu_r, paste0(direccion, '3_terr_indsel_educacion.csv'))