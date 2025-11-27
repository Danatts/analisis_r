# APLICACIÓN DE METODOLOGÍA DE CRISP-DM y KDD
# Daniel Alfonso Arteta Salazar
# Fuente: https://rpubs.com/profe_ferro/1346684

# Objetivo: aplicar CRISP, CRISP-DM y KDD a una base actuarial simulada.

library(dplyr)

# Generación de la base simulada
set.seed(101)
n <- 1000
polizas <- tibble::tibble(
  id         = 1:n,
  edad       = sample(18:85, n, replace = TRUE),
  genero     = sample(c("M","F"), n, replace = TRUE),
  siniestros = rpois(n, lambda = 0.25),
  valor_aseg = round(rnorm(n, mean = 120000, sd = 25000), 0),
  prima      = 600 + 15*edad + 250*siniestros + rnorm(n, 0, 400)
)

# Introducimos "ruido" realista
polizas$edad[sample(1:n, 8)]          <- c(-3, 0, 130, 200, 500, 5, 150, -10)  # rangos imposibles
polizas$valor_aseg[sample(1:n, 10)]   <- NA                                       # faltantes
polizas <- polizas %>% janitor::clean_names()

# RUTA B - CRISP-DM
# FASE 1: Enteder el problema:
# - Se desea explicar el valor de la prima con el sexo y la edad del asegurado
# - Se espera que no exista un sesgo por el sexo a la hora de adjudicar el valor
#   de una prima

# FASE 2: Entender los datos

dim(polizas) # Revisar tamaño de la matriz
str(polizas) # Revisar los tipos de datos de las variables
colSums(is.na(polizas)) # No hay datos nulos en las columnas
sum(duplicated(polizas)) # No hay filas repetidas en la matriz
summary(polizas) # Se observa que hay valores negativos en 'prima' y 'edad', además de valores muy altos en edad
head(polizas) # Conocer los valores de las primeras filas
summary(polizas) # Conocer los datos estadísticos más relevantes de cada variable.

# FASE 3: Preparación de los datos

polizas <- polizas %>%
  janitor::clean_names() %>%
  select(edad, genero, prima)

# Limpieza de datos
polizas <- polizas %>% filter(prima > 0, edad > 0, edad < 100) # Se eliminan los valores negativos y se establece 100 como edad máxima

# FASE 4: Modelaje
mod_cd <- lm(prima ~ genero + edad, data = polizas)

# FASE 5: Evaluar
summary(mod_cd)

# FASE 6: Predecir
predecir_prima <- function(genero, edad) predict(mod_cd, data.frame(genero = genero, edad = edad))
predecir_prima("F", 79)
predecir_prima("F", 45)
predecir_prima("F", 22)
predecir_prima("M", 79)
predecir_prima("M", 45)
predecir_prima("M", 22)
