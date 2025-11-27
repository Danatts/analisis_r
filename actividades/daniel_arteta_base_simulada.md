---
output:
  pdf_document: default
  html_document: default
---
# BASE SIMULADA

**Nombre**: Daniel Alfonso Arteta Salazar

**Documento**: CC 1071170585

## 1) Transformación y diagnóstico de calidad

**Limpieza inicial:**

- Se aplicó `janitor::clean_names()` para estandarizar los nombres de las columnas.
- Se detectaron 110 duplicados en `id_poliza` usando `duplicated()`, por lo que
se decidió conservarlos debido a que son un número significativo de registros para
el ejercicio.

**Manejo de valores faltantes:**

- Se calcula el porcentaje de valores faltantes a traves de la siguiente 
formula `round(colSums(is.na(siniestros)) / nrow(siniestros) * 100, 2`.
- Los valores nulos de "valor_aseg" se reemplazan por la mediana de la variable.
- Los valores nulos de "genero" se reemplazan por la etiqueta "No Genero".

**Depuración de edades fuera de rango:**

- Se crea la condición de `edad < 18 | edad > 100` para determinar el número de
registros que están fuera del rango de edad.
- Se excluyeron 5 observaciones para mantener coherencia demográfica.

## 2) Etapa descriptiva

**Estadísticas descriptivas:**

- Edad promedio: `mean(edad, na.rm = TRUE)`
- Edad mediana: `median(edad, na.rm = TRUE)`
- Valor asegurado promedio: `mean(valor_aseg, na.rm = TRUE)`
- Valor prima promedio: `median(prima, na.rm = TRUE)`
- Distribución de siniestros mediante `table(siniestros$siniestros)`.

## 3) Etapa prescriptiva

**Clasificación de riesgo por edad:**

- Se creó la variable `riesgo` con tres categorías:
  - Bajo: menores de 30 años.
  - Medio: entre 30 y 60 años.
  - Alto: mayores de 60 años.
- Se contabilizaron las pólizas por categoría de riesgo usando `table(siniestro$riesgo)`.

**Análisis de primas por riesgo:**

Por los valores de las medidas centrales, no parece necesario un ajusteen la 
prima ya que se adecua al crecimiento del nivel de riesgo.

## 4) Etapa predictiva

**Modelo predictivo 1:**

- Se ajustó un modelo de regresión lineal con `prima` como variable endógena y
`edad` y `siniestros` como variables exógenas.
- El resumen del modelo (`summary(mod01)`) mostró que ninguna de las dos
variables es significativa, lo que es coherente con la naturaleza aleatoria
de la simulación.

## 5) Etapa prospectiva

**Modelo predictivo 2:**

- Se ajustó un segundo modelo: `prima ~ valor_aseg`.
- Se simuló un escenario donde el valor asegurado promedio aumenta un 20%.
- Se compararon las primas esperadas en ambos escenarios usando `predict()`.

**Resultado del escenario:**

- Se concluyó que el incremento es marginal, lo que sugiere que el modelo no
captura relaciones fuertes en los datos simulados.
- Se calculó el cambio porcentual en la prima esperada ante el aumento del valor
asegurado.
- Según el modelo, un aumento del 20% en el valor promedio del valor asegurado
representa un aumento de  1.38 % en el valor de la prima.