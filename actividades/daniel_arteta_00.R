# Actividad 00
# Daniel Arteta Salazar
# Fuente: https://rpubs.com/profe_ferro/1338137

# 1) Objetos y asignación
# Crear 4 objetos:
edad <- 27
nombre <- 'Daniel'
es_actuario <- TRUE
tasa <- 0.035

# Mirar objetos
edad; nombre; es_actuario; tasa

# Mini-reto 1.1: ¿Qué está mal aquí?
# El nombre de las variables es incorrecto pues no pueden empezar con números
# ni contener espacios.

#2tasa <- 3
#mi nombre <- 'Luis'
tasa2 <- 3
mi_nombre <- 'Luis'

# 2) Tipos y clases
class(edad); typeof(edad)
class(nombre); typeof(nombre)
class(es_actuario); typeof(es_actuario)
class(tasa); typeof(tasa)

# Mini-reto 2.1: Convierte y verifica
x <- '42'
x_num <- as.numeric(x)
class(x_num); typeof(x_num)

y <- 10
y_char <- as.character(y)
class(y_char); typeof(y_char)

# 3) Vectores atómicos
# Construcción
v1 <- c(10, 20, 30)
v2 <- 1:5
v3 <- seq(from = 0, to = 1, by=0.25)
v4 <- rep(TRUE, 4)

v1; v2; v3; v4
length(v1); length(v2)

# Operaciones vectorizadas y funciones básicas
ingresos <- c(100, 120, 90, 110, 130)
ingresos_aj <- ingresos * 1.05
sum(ingresos); mean(ingresos); median(ingresos)

# Indexación
ingresos[1] # primer elemento
ingresos[2:4] # rango
ingresos[-1 ] # todos menos el primero
ingresos[ingresos < 110] # filtrado lógico

# Datos nulos y manejo básico
siniestros <- c(1, 0, 2, NA, 1, 3,0, 0)
is.na(siniestros) # revisa si los valores son nulos
mean(siniestros) # toma en cuenta valores nulos
mean(siniestros, na.rm = True) # ignora valores nulos
siniestros_sinNA <- ifelse(is.na(siniestros), 0, siniestros)
siniestros_sinNA

# 4) Coerción y reciclaje
c(1, 2, '3') # coerciona a caracter
c(TRUE, 2) + 1 # TRUE -> 1
c(10, 20, 30, 40) + c(1,2) # reciclaje: se repite el vector corto

# Mini-reto 4.1: ¿Qué resultado esperas antes de correr lo anterior?
# Los elementos del segundo vector se sumen en los primeros dos del primer vector
# y los demás queden igual.

# 5) Factores
riesgo <- c('Bajo', 'Medio', 'Alto', 'Bajo', 'Alto')
riesgo_f <- factor(riesgo, levels = c('Bajo', 'Medio', 'Alto'), ordered = TRUE)
riesgo_f
class(riesgo)
table(riesgo_f)

# 6) Mini-reto final
siniestros_clean <- ifelse(is.na(siniestros), 0, siniestros)
tasa_prom <- mean(siniestros_clean)
tipo_siniestro <- ifelse(siniestros_clean == 0, "sin", "con")
names(siniestros_clean) <- paste0("P", seq_along(siniestros_clean))
siniestros_con <- siniestros_clean[tipo_siniestro == "con"]

tasa_prom
tipo_siniestro
siniestros_con
