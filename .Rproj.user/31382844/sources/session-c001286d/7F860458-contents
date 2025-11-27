# Actividad 02
# Daniel Arteta Salazar
# Fuente: https://rpubs.com/profe_ferro/1338139

# Paquetes
library(readr); library(readxl); library(dplyr)
library(lubridate); library(forcats)

# 1) Lectura de datos (readr/readxl)
# Rutas
ruta_csv <- '/cloud/project/bases/polizas.csv'
ruta_xlsx <- '/cloud/project/bases/pagos.xlsx'

# CSV con especificación de tipos
polizas_raw <- read_csv(
  ruta_csv,
  col_types = cols(
    id = col_character(),
    fecha_inicio = col_date(format = '%Y-%m-%d'),
    edad = col_integer(),
    riesgo = col_character(),
    prima = col_double()
  )
)

# Excel
pagos_raw <- read_excel(ruta_xlsx, sheet = 'pagos',
                        col_types = c('text', 'date', 'numeric'))
# Mirar bases
glimpse(polizas_raw)
glimpse(pagos_raw)

# Escribir CSV de respaldo
write_csv(polizas_raw, '/cloud/project/bases/polizas_backup.csv')

# Mini-reto 1.1: ¿Qué pasa si 'fecha_inicio' no tiene formato ISO?
# Si no se especifica el formato de fecha correcto, la conversión del texto 
# al dato no será correcta o no se podrá hacer. En este caso, el archivo CSV
# tiene el año en primer lugar, por lo que al usar el formato '%d-%m-%Y' no se
# logra procesar.

# 2) Limpieza de factores y strings
library(stringr)
polizas <- polizas_raw %>%
  mutate(
    riesgo = str_trim(riesgo), # quitar espacios
    riesgo = str_to_title(riesgo), # 'alto' -> 'Alto'
    riesgo = factor(riesgo, levels = c('Bajo', 'Medio', 'Alto', ordered = TRUE))
  )

count(polizas, riesgo)

# Mini-reto 2.1: Colapsa 'Medio' y 'Bajo' en un nivel 'No-Alto' usando
# forcats::fct_collapse

forcats::fct_collapse(polizas$riesgo,
                      'No-Alto' = c('Medio', 'Bajo'),
                      other_level = 'Alto')

# 3) Fechas con 'lubridate'
polizas <- polizas %>%
  mutate(
    anio = year(fecha_inicio),
    mes = month(fecha_inicio, label = TRUE, abbr = TRUE, locale = 'es'),
    antig = as.integer(Sys.Date() - fecha_inicio),
    f_mes = floor_date(fecha_inicio, unit = 'month')
  )

# Mini-reto 3.1: Crea una columna 'trimestre' y cuenta pólizas por trimestre
polizas <- polizas %>%
  mutate(
    trimestre = quarter(fecha_inicio, with_year = TRUE)
  )
count(polizas, trimestre)
# En el primer trimestre hay 5 pólizas. En el segundo semestre hay 7 pólizas

# 4) Joins modernos con 'dplyr'
# Resumen de pagos por id
pagos_sum <- pagos_raw %>%
  mutate(mes_pago = floor_date(fecha_pago, 'month')) %>%
  group_by(id) %>%
  summarise(
    total_pagos = sum(valor, na.rm = TRUE),
    n_pagos = n(),
    .groups = 'drop'
  )

# LEFT JOIN: traer totales a la tabla de pólizas
polizas_join <- polizas %>%
  left_join(pagos_sum, by = 'id') %>%
  mutate(
    total_pagos = replace_na(total_pagos, 0),
    n_pagos = replace_na(n_pagos, 0L)
  )

# Diagnósticos útiles
sin_pagos <- polizas %>% anti_join(pagos_sum, by = 'id')
solo_en_pagos <- pagos_sum %>% anti_join(polizas, by = 'id')

nrow(sin_pagos); nrow(solo_en_pagos)

# INNER JOIN: 'explota' a nivel de pago
polizas_detalle <- polizas %>% inner_join(pagos_raw, by = 'id')

# Mini-reto 4.1: Calcula la prima promedio y el total de pagos por riesgo ¿Quién paga más?
# Prima promedio: 
mean(polizas_detalle$prima, na.rm = TRUE) # 358.789 
# Pagos por riesgo: 
aggregate(prima ~ riesgo, data = polizas_detalle, FUN = 'sum')
# Bajo 4118.48 -> Pagan más
# Medio 593.29
# Alto 2822.80

# 5) Exportar resultados
reporte <- polizas_join %>%
  select(id, fecha_inicio, riesgo, prima, total_pagos, n_pagos)

write_csv(reporte, '/cloud/project/bases/reporte_poliza_pagos.csv')

# Mini-reto final
df <- pagos_raw %>%
  mutate(mes_pago = floor_date(fecha_pago, 'month')) %>%
  left_join(polizas %>% select(id, riesgo), by = 'id') %>%
  group_by(riesgo, mes_pago) %>%
  summarise(total_mes = sum(valor, na.rm = TRUE), .groups = 'drop') %>%
  group_by(riesgo) %>%
  mutate(prom_riesgo = mean(total_mes)) %>%
  ungroup() %>%
  mutate(gap = total_mes - prom_riesgo)

write_csv(df, '/cloud/project/bases/reporte_riesgo_mes.csv')
df
