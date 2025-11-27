# La carrera de la vida
library(tidyverse)
library(knitr)
library(readr)

qfun_linear <- function(x, m) pmin(x/m, 1)

qfun_gompertz <- function(x, m, A = 0.002, B = 0.35) {
  pmin(A * exp(B * x), 1)
}

qfun_weibull <- function(x, m, alpha = 0.002, beta = 2) {
  pmin(alpha * (x ^ beta), 1)
}

get_qfun <- function(model = c("linear","gompertz","weibull")) {
  model <- match.arg(model)
  switch(model,
         linear   = function(x, m) qfun_linear(x, m),
         gompertz = function(x, m) qfun_gompertz(x, m),
         weibull  = function(x, m) qfun_weibull(x, m))
}

l0 <- 1000L
edad_max <- 100L
q_model <- 'linear'

simulate_cohort <- function(l0, edad_max, q_model = "linear", censura = 0) {
  stopifnot(l0 > 0, edad_max >= 1, censura >= 0, censura <= 1)
  qfun <- get_qfun(q_model)
  
  lx  <- integer(edad_max + 1)
  dx  <- integer(edad_max + 1)
  cx  <- integer(edad_max + 1)  # censuras por edad (opcional)
  
  qxv <- numeric(edad_max + 1)
  pxv <- numeric(edad_max + 1)
  Sv  <- numeric(edad_max + 1)
  
  lx[1]  <- l0
  qxv[1] <- 0
  pxv[1] <- NA
  Sv[1]  <- 1
  
  for (x in 1:edad_max) {
    qx <- qfun(x, edad_max)
    # Muertes por Bernoulli(lx[x], qx)
    muertes <- sum(runif(lx[x]) < qx)
    # Censura aleatoria independiente
    cens    <- if (censura > 0) sum(runif(lx[x] - muertes) < censura) else 0
    
    dx[x+1] <- muertes
    cx[x+1] <- cens
    lx[x+1] <- lx[x] - muertes - cens
    
    qxv[x+1] <- qx
    pxv[x+1] <- 1 - qx
    Sv[x+1]  <- lx[x+1] / l0
    
    if (lx[x+1] <= 0) {
      # completar filas restantes con ceros si se extingue la cohorte
      lx[(x+2):(edad_max+1)] <- 0
      dx[(x+2):(edad_max+1)] <- 0
      cx[(x+2):(edad_max+1)] <- 0
      qxv[(x+2):(edad_max+1)] <- NA
      pxv[(x+2):(edad_max+1)] <- NA
      Sv[(x+2):(edad_max+1)]  <- 0
      break
    }
  }
  
  tabla <- tibble(
    Edad = 0:edad_max,
    lx   = lx,
    dx   = c(NA, dx[-1]),
    cx   = c(NA, cx[-1]),
    qx   = round(qxv, 4),
    px   = round(pxv, 4),
    Sx   = round(Sv, 4)
  )
  
  # Esperanza de vida discreta aproximada (área bajo S)
  e0 <- sum(Sv, na.rm = TRUE)
  
  list(tabla = tabla, e0 = e0)
}

res <- simulate_cohort(
  l0       = l0,
  edad_max = edad_max,
  q_model  = q_model,
)



tabla_vida <- res$tabla

kable(head(tabla_vida, 36), caption = "Primeras 10 filas de la tabla de vida simulada")
