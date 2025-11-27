# Actividad 01
# Daniel Arteta Salazar
# Fuente: https://posit.cloud/content/10741321

# 1) Crear un data frame desde vectores
set.seed(123)
id <- paste0("P", 1:12)
edad <- sample(18:70, 12, replace = TRUE)
riesgo <- factor(sample(c("Bajo","Medio","Alto"), 12, replace = TRUE),
                 levels = c("Bajo","Medio","Alto"), ordered = TRUE)
siniestros <- sample(c(0,0,0,1,2, NA), 12, replace = TRUE)
prima <- round(runif(12, 200, 600), 2)

polizas <- data.frame(id, edad, riesgo, siniestros, prima)
polizas
str(polizas)
nrow(polizas); ncol(polizas); names(polizas)

# Mini-reto 1.1: ¿Qué clase tiene riesgo y siniestros?
class(riesgo); typeof(riesgo) # 'riesgo' es de clase 'ordered' y tipo 'factor'
class(siniestros); typeof(siniestros) # 'siniestros' es de clase 'numeric' y tipo 'double'

# 2) Selección e indexacción
polizas$edad # seleccionar una columna
polizas[,'prima'] # por nombre
polizas[1:3, c('id', 'prima')] # filas 1:3 y columnas elegidas

# Filtrado lógico
polizas[polizas$riesgo == 'Alto', ]
polizas[polizas$edad >= 40 & polizas$prima > 400,]

# Ordenar
polizas_orden <- polizas[order(polizas$riesgo, -polizas$edad),]
head(polizas_orden, 12)

# Mini-reto 2.1: Crea polizas_top_edad con las 5 edades más altas
polizas_top_edad <- head(polizas[order(polizas$edad, decreasing = TRUE),], 5)
polizas_top_edad

# Transformación básica
# Nueva columna: prima mensual
polizas$prima_mensual <- round(polizas$prima/12, 2)

# Tasa de siniestro (con NA -> 0)
polizas$siniestros_clean <- ifelse(is.na(polizas$siniestros), 0, polizas$siniestros)
polizas$tiene_siniestro <- polizas$siniestros_clean > 0

# Renombrar y reordenar
names(polizas)[names(polizas) == 'tiene_siniestro'] <- 'tiene_sin'
polizas <- polizas[,c('id', 'edad', 'riesgo', 'siniestros', 'siniestros_clean', 'tiene_sin', 'prima', 'prima_mensual')]
head(polizas)

# Mini-reto 3.1: Elimina la columna 'siniestros' dejando solo 'siniestros_clean'
polizas$siniestros <- NULL

# 4) Uniones: cbind/rbind y merge
# cbind/rbind (cuanod dimensiones coinciden)
nueva_col <- sample(LETTERS[1:3], nrow(polizas), replace = TRUE)
polizas2 <- cbind(polizas, grupo = nueva_col)
nrow(polizas2); ncol(polizas2)

# Creamos pagos por id (varias filas por id)
set.seed(456)
pagos <- data.frame(
  id = sample(polizas$id, 20, replace = TRUE),
  fecha = as.Date('2025-01-01') + sample(0:200, 20, replace = TRUE),
  valor = round(runif(20, 50, 500), 2)
)
head(pagos, 5)

# Resumen de pagos por id
pagos_sum <- aggregate(valor ~ id, data = pagos, FUN = sum)
head(pagos_sum)

# merge: traemos el total de pagos a polizas2
polizas3 <- merge(polizas2, pagos_sum, by = 'id', all.x = TRUE)
polizas3$valor[is.na(polizas3$valor)] <- 0
head(polizas3, 8)

# Mini-reto 4.1: ¿Cuántas pólizas no tienen pagos?
sum(polizas3$valor == 0) # solo 1 póliza no tiene pagos

# 5) Resumen por grupo
# Tasa de siniestro por nivel de riesgo
tasa_por_riego <- aggregate(tiene_sin ~ riesgo, data = polizas3, FUN = mean)
tasa_por_riego

# Prima promedio por riesgo
prima_prom_riesgo <- aggregate(prima ~ riesgo, data = polizas3, FUN = mean)
prima_prom_riesgo

# Opcionarl con la librería 'dplyr'
library(dplyr)
polizas3 %>%
  group_by(riesgo) %>%
  summarise(tasa = mean(tiene_sin), prima_prom = mean(prima, .groups = 'drop'))

# 6) Listas: objetos heterogéneos
# Construimos una lista 'carteta' con todo junto
cartera <- list(
  meta = list(fecha = Sys.Date(), n_polizas = nrow(polizas3)),
  polizas = polizas3,
  pagos = pagos,
  util = list(
    calcular_loading = function(prima, loading = 0.15) {round(prima * (1 + loading), 2)}
  )
)

# Acceso
cartera$meta$n_polizas
head(cartera$polizas[, c('id', 'prima', 'valor')], 3)

# Añadir un elemento nuevo
cartera$resumen_riesgo <- aggregate(tiene_sin ~ riesgo, data = cartera$polizas, mean)
cartera$resumen_riesgo

# Mini-reto 6.1: Extrae desde la lista 'cartera' solo los 'id' con 'valor' > 300
# en pagos totales
cartera$polizas$id[cartera$polizas$valor > 300]

# 7) lapply/sapply con listas y data frames
# Medios de columnas numéricas del data frame
num_cols <- sapply(cartera$polizas, is.numeric)
lapply(cartera$polizas[, num_cols], mean)

# Sobre una lista de vectores
mis_vectores <- list(a = 1:5, b = c(10, 20, 30), c = c(2, 2, 8, 4))
sapply(mis_vectores, mean)

# Mini-reto final
riesgos_por_grupo <- aggregate(cbind(tiene_sin, prima) ~ riesgo + grupo, data = cartera$polizas, FUN = mean)
cartera$riesgos_por_grupo <- riesgos_por_grupo

cartera$util$categorizar_tasa <- function(tasa) {
  ifelse(tasa > 0.4, 'Alta', ifelse(tasa >= 0.2, 'Media', 'Baja'))
}

cartera$riesgos_por_grupo$categoria <- cartera$util$categorizar_tasa(cartera$riesgos_por_grupo$tiene_sin)
names(cartera$riesgos_por_grupo)[names(riesgos_por_grupo) == 'tiene_sin'] <- 'tasa'
cartera$riesgos_por_grupo
