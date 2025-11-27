# Actividad Base Simulada
# Daniel Arteta Salazar
# Fuente: https://rpubs.com/profe_ferro/1346450

# Paquetes
library(dplyr)
library(ggplot2)

# 0) Simulación de base de datos 
# Simulación de datos
set.seed(123)  # establecer semilla

n <- 500
siniestros <- tibble(
  id_poliza   = sample(1000:2000, n, replace = TRUE),   # puede tener duplicados
  edad        = sample(18:90, n, replace = TRUE),
  genero      = sample(c("M", "F", NA), n, replace = TRUE, prob = c(0.45, 0.45, 0.10)),
  valor_aseg  = round(rnorm(n, mean = 100000, sd = 20000), 0),
  siniestros  = rpois(n, lambda = 0.3),
  prima       = round(rnorm(n, mean = 3000, sd = 800), 0)
)

# Añadir datos errádos
siniestros$edad[sample(1:n, 5)] <- c(-5, 150, 200, -10, 500)  # edades fuera de rango
siniestros$valor_aseg[sample(1:n, 5)] <- NA  

# 1) Transformación y diagnóstico de calidad
# Limpiar nombres de las variables
siniestros <- siniestros %>% janitor::clean_names()

# Detectar duplicados
duplicados <- duplicated(siniestros$id_poliza) # devuelve una lista de booleanos indicando si el valor de la variable "id_poliza" se repite
sum(duplicados) # suma de valores duplicados
siniestros[duplicados,] # matriz de registros con id duplicado

# Identificar valores faltantes y su porcentaje
colSums(is.na(siniestros)) # número de valores nulos por variable
round(colSums(is.na(siniestros)) / nrow(siniestros) * 100, 2) # porcentaje de valores nulos por variable
siniestros$valor_aseg <- ifelse(
  is.na(siniestros$valor_aseg),
  median(siniestros$valor_aseg,na.rm = TRUE),
  siniestros$valor_aseg) # reemplazamos los datos nulos de "valor_aseg" por la mediana
siniestros$genero <- ifelse(
  is.na(siniestros$genero),
  "No Genero",
  siniestros$genero) # reemplazamos los datos nulos de "genero" por la etiqueta "No Genero"

# Encontrar edades fuera de rango (<18 o >100)
condicion <- siniestros$edad < 18 | siniestros$edad > 100
sum(condicion) # número de registros que están fuera del rango
siniestros <- siniestros[!condicion,] # eliminar registros fuera del rango

## 2) Etapa descriptiva
# Calcular la edad promedio y mediana de lo asegurados
siniestros %>%
  summarise(
    edad_promedio = mean(edad, na.rm = TRUE),
    edad_mediana = median(edad, na.rm = TRUE)
  )

# Calcular el valor asegurado medio y la prima promedio
siniestros %>%
  summarise(
    valor_aseg_promedio = mean(valor_aseg, na.rm = TRUE),
    prima_promedio = mean(prima, na.rm = TRUE)
  )

# Mostrar la distribución de siniestros
table(siniestros$siniestros)

# 3) Etapa prescriptiva
# Definir una regla de riesgo actuarial según edad
siniestros$riesgo <-  ifelse(siniestros$edad < 30, "Bajo", ifelse(siniestros$edad < 60, "Medio", "Alto"))

# Contar cuántas pólizas hay en cada categoría de riesgo
table(siniestros$riesgo) # conteo de siniestros por nivel de riesgo

siniestros %>%
  group_by(riesgo) %>%
  summarise(
    prima_promedio = mean(prima),
    prima_mediana = median(prima)
  )

# Recomendar si se debe ajustar la prima a los de riesgo alto:
# RTA: Por los valores de las medidas centrales, no parece necesario un ajuste
# en la prima ya que se adecua al crecimiento del nivel de riesgo.

# 4) Etapa predictiva
# Ajustar un modelo sencillo para predecir la prima en función de edad y siniestros
mod01 <- lm(prima ~ edad + siniestros, data = siniestros)
summary(mod01) # Tras ejecutar el modelo se observa que las variables de edad y siniestros no son buenas para explicar la el valor de la prima.

# 5) Etapa prospectiva
# Suponer que la aseguradora planea aumentar en 20% el valor asegurado promedio
# Usar el modelo predictivo para estimar cómo cambiaría la prima promedio en ese escenario

mod02 <- lm(prima ~ valor_aseg, data = siniestros)

promedio <- mean(siniestros$valor_aseg, na.rm = TRUE) # valor promedio del valor asegurado
promedio_20 <- mean(siniestros$valor_aseg, na.rm = TRUE) * (1 + 0.2) # aumento del 20% al valor promedio del valor asegurado

esc01 <- predict(mod02, data.frame(valor_aseg = promedio)) # predicción sin aumento 
esc02 <- predict(mod02, data.frame(valor_aseg = promedio_20)) # predicción con aumento

data.frame(
  escenario        = c("Actual", "Valor asegurado + 20%"),
  valor_promedio  = c(promedio, promedio_20),
  prima_esperada    = c(esc01,  esc02)
)

porc <- round(((esc02 - esc01) / esc01) * 100, 2) # cambio porcentual

# paste("Un aumento del 20% en el valor promedio del valor asegurado representa un aumento de ", porc, "% en el valor de la prima.")
