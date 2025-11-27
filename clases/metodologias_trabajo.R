# METODOLOGÍAS DE TRABAJO
# Daniel Alfonso Arteta Salazar
# Fuente: https://rpubs.com/profe_ferro/1346684

# 1. CRISP (Cross-Industry Standard Process)
# Paso 1: Entender el problema
# - Objetivo: estimar mass con height.
# - Criterio: signo esperado del coeficiente y ajuste razonable (demo).
#
# Paso 2: Entender los datos
sw <- dplyr::starwars %>%
  janitor::clean_names() %>%
  select(name, species, height, mass)

summary(sw)

# Pase 3: Preparar los datos
sw_prep <- sw %>%
  mutate(
    height = as.numeric(height),
    mass   = as.numeric(mass)
  ) %>%
  filter(!is.na(height), !is.na(mass)) %>%    # remover NA para demo
  filter(height > 50, height < 300,           # reglas de rango simples
         mass > 10,   mass   < 5000)

# Paso: 4 Modelar (sencillo)
mod_crisp <- lm(mass ~ height, data = sw_prep)

# Paso 5: Evaluar (rápido)
summary(mod_crisp)                          # coeficientes y R^2

# 2. CRISP-DM (Estándar operativo por fases)
# Fase 1: Business Understanding
# - Objetivo: estimar mass con height.
# - Criterio: signo esperado del coeficiente y ajuste razonable (demo).

# Fase 2: Data Understanding
sw <- dplyr::starwars %>%
  janitor::clean_names() %>%
  select(name, species, height, mass)
summary(sw)                    # NA, rangos, consistencia

# Fase 3: Data Preparation
sw_final <- sw %>%
  mutate(height = as.numeric(height), mass = as.numeric(mass)) %>%
  filter(!is.na(height), !is.na(mass)) %>%
  filter(height > 50, height < 300, mass > 10, mass < 5000) %>%
  distinct(name, .keep_all = TRUE)

# Fase 4: Modeling
mod_cd <- lm(mass ~ height, data = sw_final)

# Fase 5: Evaluation
summary(mod_cd)

# Fase 6: Deployment (mini): una función de predicción
predecir_mass <- function(h) predict(mod_cd, data.frame(height = h))
predecir_mass(c(160, 170, 200))

# 3. KDD (Knowledge Discovery in Database)
# Paso 1: Selección
kdd_sel <- dplyr::starwars %>%
  janitor::clean_names() %>%
  select(name, species, height, mass)

# Paso 2: Preprocesamiento
kdd_pre <- kdd_sel %>%
  mutate(height = as.numeric(height), mass = as.numeric(mass)) %>%
  filter(!is.na(height), !is.na(mass))

# Paso 3: Transformación (feature simple)
kdd_tra <- kdd_pre %>%
  mutate(bmi_aprox = mass / ((height/100)^2))

# Paso 4: Minería de datos (modelo básico)
mod_kdd <- lm(mass ~ height, data = kdd_tra)

# Paso 5: Interpretación/Evaluación
summary(mod_kdd)