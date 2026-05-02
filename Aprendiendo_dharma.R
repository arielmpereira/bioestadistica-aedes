library(DHARMa)

set.seed(123)

# Partimos del modelo
x <- rnorm(100)
y <- 2 + 3*x + rnorm(100, sd = 1)

datos <- data.frame(x, y)

x11()
plot(x=datos$x, y=datos$y)

modelo <- lm(y ~ x, data = datos)


# Metodo tradicional
# Residuo = observado - predicho

# Calculamos los predichos
datos$y_pred <-fitted(modelo)
datos$y_pred
sigma_modelo <- sigma(modelo) # dev std de los errores
sigma_modelo

# Residuos estandarizados
# rs = (y - y^) / sd(error)

datos$residuo_std <- rstandard(modelo)

x11()
plot(datos$y_pred, datos$residuo_std,
     xlab = "Valores predichos",
     ylab = "Residuo estandar")
abline(h = 0, lty = 2)

# Mini dharma manual
# Simulamos datos del modelo

n_sim <- 1000
n <- nrow(datos)

simulaciones <- matrix(NA, nrow = n, ncol = n_sim)
# Fila: observacion
# Columna: simulacion

for(j in 1:n_sim){
  simulaciones[, j] <- rnorm(n, mean = datos$y_pred, sd = sigma_modelo)
}


# Calculamos el residuo tipo Dharma

datos$residuo_dharma_manual <- NA

for(i in 1:n){
  datos$residuo_dharma_manual[i] <- mean(simulaciones[i, ] <= datos$y[i])
}

# Graficos de la simulacion

x11()
hist(datos$residuo_dharma_manual,
     breaks = 10,
     main = "Histograma de residuos DHARMa manual",
     xlab = "Residuo DHARMa manual",
     ylab = "Frecuencia")

# Si el modelo está bien:
# el histograma debería verse más o menos plano

x11()
res_ordenados <- sort(datos$residuo_dharma_manual)
cuantiles_uniformes <- ppoints(length(res_ordenados))
plot(cuantiles_uniformes, res_ordenados,
     xlab = "Cuantiles teóricos Uniforme(0,1)",
     ylab = "Residuos DHARMa ordenados",
     main = "QQ plot uniforme manual")
abline(0, 1, lty = 2)

# Si el modelo está bien:
# los puntos deberían seguir la diagonal

x11()
plot(datos$y_pred, datos$residuo_dharma_manual,
     xlab = "Valores predichos",
     ylab = "Residuo DHARMa manual",
     main = "Residuos DHARMa vs valores predichos",
     ylim = c(0, 1))
abline(h = 0.5, lty = 2)
abline(h = c(0.025, 0.975), lty = 3)

# Si el modelo está bien:
# la nube debe estar entre 0 y 1,
# sin patrón claro,
# centrada aproximadamente en 0.5

x11()
plot(datos$y_pred, datos$residuo_dharma_manual,
     xlab = "Valores predichos",
     ylab = "Residuo DHARMa manual",
     main = "Residuos DHARMa vs valores predichos",
     ylim = c(0, 1))

abline(h = 0.5, lty = 2)
abline(h = c(0.025, 0.975), lty = 3)

lines(lowess(datos$y_pred, datos$residuo_dharma_manual), lwd = 2)

# La línea suave debería quedar cerca de 0.5.
# Si sube, baja o forma curva, puede haber patrón no capturado.


# ========================
# Resolucion con dharma
# ========================

res <- simulateResiduals(modelo)

x11()
plot(res)

# Interpretacion

# QQ plot (izq):
# Los puntos siguen muy bien la diagonal roja
# KS test p = 0.975 (>>0.05)
# Dispersion p = 0.864 (>> 0.05)
# Outlier p = 0.55 (>> 0.05)

# Los residuos se comportan como deberian: Los datos reales parecen una 
# simulacion valida del modelo

# Residual vs Predicted (der):
# No hay patron
# No hay tendencia
# Nube bastante homogenea
# Las curvas casi planas
# Ligera curvatura dentro de la banda gris
# Y dice: "No significant problems detected"


# ===============================
# Modelo mal hecho para verificar como funciona dharma
# ===============================

set.seed(123)

# Datos con relación no lineal
x <- rnorm(100)
y <- 2 + 3*x + 2*x^2 + rnorm(100, sd = 1)

datos_mal <- data.frame(x, y)

plot(datos_mal$x, datos_mal$y,
     main = "Datos con relación no lineal",
     xlab = "x",
     ylab = "y")

modelo_mal <- lm(y ~ x, data = datos_mal)

abline(modelo_mal, col = "red")


datos_mal$y_pred <- fitted(modelo_mal)
datos_mal$residuo_std <- rstandard(modelo_mal)

plot(datos_mal$y_pred, datos_mal$residuo_std,
     xlab = "Valores predichos",
     ylab = "Residuo estandarizado",
     main = "Residuos clásicos: modelo mal especificado")

abline(h = 0, lty = 2)
lines(lowess(datos_mal$y_pred, datos_mal$residuo_std), lwd = 2)

res_mal <- simulateResiduals(modelo_mal)

x11()
plot(res_mal)

