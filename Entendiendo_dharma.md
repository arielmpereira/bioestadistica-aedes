Tutorial: Entendiendo DHARMa desde cero
0. Idea general

Este tutorial compara dos enfoques para analizar residuos:

Método clásico

residuo = observado - predicho

Método DHARMa

residuo = qué tan probable es el valor observado según el modelo
1. Generación de datos
set.seed(123)

x <- rnorm(100)
y <- 2 + 3*x + rnorm(100, sd = 1)

datos <- data.frame(x, y)

plot(datos$x, datos$y,
     main = "Datos simulados",
     xlab = "x",
     ylab = "y")
2. Ajuste del modelo
modelo <- lm(y ~ x, data = datos)
summary(modelo)

El modelo estima la relación entre x e y.

3. Método tradicional
3.1 Valores predichos
datos$y_pred <- fitted(modelo)

Son los valores esperados por el modelo.

3.2 Residuo clásico
datos$residuo <- datos$y - datos$y_pred

Representa el error directo.

3.3 Residuo estandarizado
datos$residuo_std <- rstandard(modelo)

Indica cuántas desviaciones estándar se aleja cada punto.

3.4 Gráfico de residuos
plot(datos$y_pred, datos$residuo_std,
     xlab = "Valores predichos",
     ylab = "Residuo estandarizado",
     main = "Método clásico")

abline(h = 0, lty = 2)

Interpretación:

Los residuos deben estar centrados en 0
No debe haber patrones visibles
4. Construcción manual de DHARMa
4.1 Idea central

Para cada observación se evalúa si el valor observado es compatible con lo que el modelo podría generar.

4.2 Estimación de la variabilidad
sigma_modelo <- sigma(modelo)

Es la desviación estándar estimada de los errores.

4.3 Simulación desde el modelo
n_sim <- 1000
n <- nrow(datos)

simulaciones <- matrix(NA, nrow = n, ncol = n_sim)

for(j in 1:n_sim){
  simulaciones[, j] <- rnorm(
    n,
    mean = datos$y_pred,
    sd = sigma_modelo
  )
}

Interpretación:

Cada columna es un dataset simulado
Cada fila contiene valores posibles para una observación
4.4 Cálculo del residuo tipo DHARMa
datos$residuo_dharma_manual <- NA

for(i in 1:n){
  datos$residuo_dharma_manual[i] <- mean(simulaciones[i, ] <= datos$y[i])
}

Interpretación:

Para cada observación se calcula el percentil del valor observado dentro de la distribución simulada.

5. Gráficos tipo DHARMa
5.1 Histograma
hist(datos$residuo_dharma_manual,
     breaks = 10,
     main = "Residuos DHARMa (manual)",
     xlab = "Valor entre 0 y 1")

Si el modelo es correcto, la distribución debería ser aproximadamente uniforme.

5.2 QQ plot uniforme
res_ordenados <- sort(datos$residuo_dharma_manual)
cuantiles_uniformes <- ppoints(length(res_ordenados))

plot(cuantiles_uniformes, res_ordenados,
     xlab = "Cuantiles Uniforme(0,1)",
     ylab = "Residuos",
     main = "QQ plot DHARMa")

abline(0, 1, lty = 2)

Si el modelo es correcto, los puntos siguen la diagonal.

5.3 Residuos vs valores predichos
plot(datos$y_pred, datos$residuo_dharma_manual,
     xlab = "Valores predichos",
     ylab = "Residuo DHARMa",
     main = "DHARMa vs predicho",
     ylim = c(0,1))

abline(h = 0.5, lty = 2)
abline(h = c(0.025, 0.975), lty = 3)

lines(lowess(datos$y_pred, datos$residuo_dharma_manual), lwd = 2)

Interpretación:

Los puntos deben distribuirse sin patrón
La tendencia debe ser aproximadamente horizontal en 0.5
6. Comparación con DHARMa real
library(DHARMa)

res <- simulateResiduals(modelo)

plot(res)
7. Interpretación final

Método clásico:

Analiza el error directo
Depende de supuestos como normalidad

DHARMa:

Analiza si los datos son compatibles con el modelo
No depende de normalidad
Funciona para cualquier tipo de modelo
8. Conclusión

El enfoque clásico evalúa cuánto se equivoca el modelo.
DHARMa evalúa si los datos observados podrían haber sido generados por el modelo.
