## Método SEMMA

### ======== SAMPLE ========
data("mtcars")

set.seed(123)
n <- nrow(mtcars)
indice <- sample(1:n, size = 0.6*n)
train <- mtcars[indice, ]
test  <- mtcars[-indice, ]

### ======== EXPLORE ========
plot(train$wt, train$mpg,
     main = "Relación entre peso y rendimiento",
     xlab = "Peso del vehículo (wt)",
     ylab = "Millas por galón (mpg)",
     pch = 19, col = "darkblue")
abline(lm(mpg ~ wt, data = train), col = "red", lwd = 2)

### ======== MODIFY ========
train$wt2 <- train$wt^2
test$wt2  <- test$wt^2

### ======== MODEL ========
modelo <- lm(mpg ~ wt + wt2, data = train)
summary(modelo)

### ======== ASSESS ========
predicciones <- predict(modelo, newdata = test)

rmse <- sqrt(mean((test$mpg - predicciones)^2))
r2   <- 1 - sum((test$mpg - predicciones)^2) / sum((test$mpg - mean(test$mpg))^2)

cat("RMSE =", round(rmse, 2), "   R² =", round(r2, 3))

plot(test$mpg, predicciones,
     main = "Valores observados vs. predichos",
     xlab = "Rendimiento real (mpg)",
     ylab = "Predicción del modelo",
     pch = 19, col = "darkgreen")
abline(0, 1, col = "red", lwd = 2, lty = 2)