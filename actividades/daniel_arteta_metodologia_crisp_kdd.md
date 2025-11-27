---
output:
  pdf_document: default
  html_document: default
---
# RUTA B — CRISP-DM

**Nombre**: Daniel Alfonso Arteta Salazar

**Documento**: CC 1071170585

## FASE 1: Entender el negocio

El objetivo del presente trabajo busca explicar si el género y la edad de los
asegurados define la prima del seguro.

## FASE 2: Entender los datos

Para la exploración general de los datos se uso:

- **dim(polizas)**: Para conocer las dimensiones de la matriz.
- **str(polizas)**: Para conocer el tipo de dato de cada una de las variables.
- **head(polizas)**: Para dar un vistazo de los primeros valores de la matriz.
- **sum(duplicated(polizas))**: Para conocer si hay filas duplicadas.
- **colSums(is.na(polizas))**: Para conocer si hay columnas con datos nulos.
- **summary(polizas**: Para conocer los valores estadísticos más importantes de
cada una de las variables.

Los resultados nos indican que hay valores atípicos o erroneos en las columnas
de prima y edad, por lo que se procede a limpiar estos casos.

## FASE 3: Preparación de los datos

Se determina por eliminar los registros cuya varible de edad no esté
entre 0 y 100, ya que por fuera de este rango se puede considerar datos atípicos
o erróneos. De estos casos solo hay 6 registros por lo que no se considera
relevante quitarlos.

Adicionalmente,se eliminan los registros cuya variable "prima" sea negativa. De
este caso solo hay dos, por lo que se considera insignificativo retirarlos.

Para tal, se ejecuta:

```r
polizas <- polizas %>% filter(prima > 0, edad > 0, edad < 100)
```
Lo cual reasigna a la variable "polizas" la matriz filtrada.

## FASE 4: Modelaje

Se ajusta un modelo de regresión lineal simple con la variable "prima" variable
endógena, y las variables edad y género como exógenas.

```r
mod_cd <- lm(prima ~ genero + edad, data = polizas)
```

## FASE 5: Evaluar

Se ejecuta `summary(mod_cd)` para conocer el resultado de la regresión lineal:
conocer el valor del intercepto, el valor de los parámetros y su significancia.

```
Call:
lm(formula = prima ~ genero + edad, data = polizas)

Residuals:
     Min       1Q   Median       3Q      Max 
-1296.77  -286.39    -0.41   286.19  1246.36 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  654.484     38.110  17.174   <2e-16
generoM        5.425     26.108   0.208    0.835
edad          15.173      0.652  23.271   <2e-16
               
(Intercept) ***
generoM        
edad        ***
---
Signif. codes:  
0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 410.8 on 989 degrees of freedom
Multiple R-squared:  0.3542,	Adjusted R-squared:  0.3529 
F-statistic: 271.2 on 2 and 989 DF,  p-value: < 2.2e-16
```
Lo resultados indican que el género no es estadísticamente significativo. Solo
la edad resulta ser una variable significativa.

## FASE 6: Predecir

Para usar el modelo para la predicción, creamos una nueva función que captura el
género y la edad para predecir el valor que tendría la prima.

```r
predecir_prima <- function(genero, edad) predict(mod_cd, data.frame(genero = genero, edad = edad))
```

Por ejemplo, en el caso de una mujer de 40 años, el modelo predice una prima de 1266.819