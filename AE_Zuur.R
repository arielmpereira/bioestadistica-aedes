# ==========================================
# TP2 Bioestadística - AE paper Aedes
# ==========================================
library(readxl)
library(car)

#  Cargar base
datos <- read_excel("aedes_data.xlsx")

dim(datos)

# ==================
# Paso 0: filtrado e inspeccion inicial
# ==================
# Nos quedamos solamente con los criaderos activos (log volume >0)
# y los datos longitudinales (Grid_type == Index)

seleccion = datos$`log volume` > 0 & datos$Grid_type == "Index"
datos_clean <- datos[seleccion, ]

# ----------------------------
# Selección de variables para AE
# ----------------------------
# Variables respuesta:
# Prevalence
# Total mosquito emerged
# Aedes aegypti
# Aedes albopictus
#
# Variables explicativas:
# Season
# Microhabitat
# Macrohabitat
# Temperature
# pH
# log volume

ae <- datos_clean[, c(
  "Prevalence",
  "Total mosquito emerged",
  "Aedes aegypti",
  "Aedes albopictus",
  "Season",
  "Macrohabitat",
  "Microhabitat",
  "Temperature",
  "pH",
  "log volume",
  "Grid_no",
  "Latititude",
  "Longitude"
)]

dim(ae)
str(ae)
colSums(is.na(ae))


# Convertimos las variables categorias en factor
ae$Season <- as.factor(ae$Season)
ae$Macrohabitat <- as.factor(ae$Macrohabitat)
ae$Microhabitat <- as.factor(ae$Microhabitat)
ae$Prevalence <- factor(ae$Prevalence, levels = c(0,1))

str(ae)
summary(ae)


# ----------------------------
# Paso 1: outliers
# ----------------------------
# Analizamos outliers gráficamente con los boxplots
# e histogramas

x11()
par(mfrow = c(2,3))
boxplot(ae$Temperature, main = "Temperature")
boxplot(ae$pH, main = "pH")
boxplot(ae$`log volume`, main = "log volume")

# En las variables de abundancia, la gran cantidad de ceros genera
# una fuerte asimetría, lo que provoca que los boxplots resulten
# poco informativos (cajas comprimidas en torno a cero).
#
# Por este motivo, se opta por no utilizarlos para estas variables
# y analizar la distribución mediante proporción de ceros e histogramas.

# Calculamos proporcion de ceros

mean(ae$`Total mosquito emerged` == 0)
mean(ae$`Aedes aegypti` == 0)
mean(ae$`Aedes albopictus` == 0)

# Se observa una alta proporción de ceros en las variables de abundancia.
# Total mosquito emerged: 83% ceros
# Aegypti: 92% ceros
# Albopictus: 95% ceros

hist(ae$`Total mosquito emerged`, main="Total emerged")
hist(ae$`Aedes aegypti`, main="Aedes aegypti")
hist(ae$`Aedes albopictus`, main="Aedes albopictus")

# Se observa una alta proporción de ceros en las variables de abundancia,
# lo que indica una distribución altamente asimétrica y sugiere la presencia
# de exceso de ceros.
#
# Esto implica que el modelo debe tener en cuenta el exceso de ceros,
# ya que la distribución observada no es compatible con modelos simples.


# --------------------------------
# Paso 2: Homogeneidad de varianza
# --------------------------------
# Se evaluó la homogeneidad de varianza mediante boxplots de las variables de abundancia
# en función de distintas variables explicativas.
# Dado que las variables de abundancia tienen exceso de ceros, esto achata los boxplots,
# para salvar este inconveniente y poder observar diferencias en los boxplot, se analiza 
# la abundancia condicionada a la presencia.

ae_tot <- ae[ae$`Total mosquito emerged` > 0, ]
ae_aeg <- ae[ae$`Aedes aegypti` > 0, ]
ae_albo <- ae[ae$`Aedes albopictus` > 0, ]

x11()
par(mfrow = c(3,1))
boxplot(`Total mosquito emerged` ~ Season, data=ae_tot)
boxplot(`Aedes aegypti` ~ Season, data=ae_aeg)
boxplot(`Aedes albopictus` ~ Season, data=ae_albo)


x11()
par(mfrow = c(3,1))
boxplot(`Total mosquito emerged` ~ Microhabitat, data = ae_tot)
boxplot(`Aedes aegypti` ~ Microhabitat, data = ae_aeg)
boxplot(`Aedes albopictus` ~ Microhabitat, data = ae_albo)

x11()
par(mfrow = c(3,1))
boxplot(`Total mosquito emerged` ~ Macrohabitat, data = ae_tot)
boxplot(`Aedes aegypti` ~ Macrohabitat, data = ae_aeg)
boxplot(`Aedes albopictus` ~ Macrohabitat, data = ae_albo)


# Al analizar únicamente los valores positivos, se observan diferencias en la
# distribución de la abundancia entre estaciones.

# La variable total muestra mayor dispersión en feb–march y july–september,
# indicando variación estacional en la abundancia.

# Aedes aegypti presenta un patrón similar al total, con presencia en varias estaciones.

# En cambio, Aedes albopictus muestra un comportamiento más concentrado,
# con mayor abundancia en july–september y baja presencia en el resto.

# Ambas especies muestran dinámicas diferentes, por lo que el uso de la abundancia
# total podría ocultar patrones específicos.


# ----------------------------
# Paso 3: Distribución de la variable respuesta
# ----------------------------

table(ae$Prevalence)
prop.table(table(ae$Prevalence))


# Se analizó la distribución de las variables respuesta.

# Prevalence: variable dicotómica (0/1), con predominio de ceros (81%),
# lo que indica baja frecuencia de presencia.

# Total mosquito emerged: presenta una distribución altamente asimétrica,
# con fuerte concentración en valores cercanos a cero y una cola larga
# hacia valores altos.

# Aedes aegypti y Aedes albopictus muestran patrones similares,
# con alta proporción de ceros y presencia ocasional de valores elevados.

# En conjunto, las variables de conteo no siguen una distribución normal,
# presentando asimetría y exceso de ceros, lo que deberá ser considerado
# en el modelado.


# ----------------------------
# Paso 4: Exceso de ceros
# ----------------------------

# Proporción de ceros

cat("Proporción en Total mosquito emerged: ")
mean(ae$`Total mosquito emerged` == 0)

cat("Proporción en Aedes aegypti: ")
mean(ae$`Aedes aegypti` == 0)

cat("Proporcion en Aedes albopictus: ")
mean(ae$`Aedes albopictus` == 0)

# Total mosquito emerged: 83%
# Aedes aegypti: 92%
# Aedes albopictus: 95%

boxplot(ae$Temperature ~ (ae$`Total mosquito emerged` > 0))
boxplot(ae$`log volume` ~ (ae$`Total mosquito emerged` > 0))

# Si bien se eliminaron los criaderos sin agua (log volume = 0),
# aún se observa una alta proporción de ceros en las variables de abundancia.

# Estos ceros podrían responder a distintos procesos.

# Por un lado, es posible que existan ceros estructurales, asociados a condiciones
# ambientales que limitan completamente la presencia de mosquitos.

# Sin embargo, el análisis de variables como la temperatura muestra un alto
# solapamiento entre observaciones con y sin presencia, lo que sugiere que
# estas variables por sí solas no permiten identificar claramente dichos casos.

# Por otro lado, se observan ceros biológicos, donde, aun existiendo condiciones
# potencialmente favorables, no se registra presencia de mosquitos.

# Es posible la coexistencia de múltiples procesos generadores de ceros, 
# aunque no es posible distinguirlos completamente a partir de los datos
# disponibles.

# ----------------------------
# Paso 5: Colinealidad
# ----------------------------

num_vars <- ae[, c("Temperature", "pH", "log volume")]
cor(num_vars)

pairs(num_vars)


# VIF
modelo <- lm(`Total mosquito emerged` ~ Temperature + pH + `log volume`, data = ae)
vif(modelo)


# Se evaluó la colinealidad entre las variables explicativas continuas
# (Temperature, pH y log volume) mediante coeficientes de correlación
# y gráficos de dispersión.

# Los coeficientes de correlación fueron bajos en todos los casos (r < 0.3),
# lo que indica ausencia de relaciones lineales fuertes entre las variables.

# Los gráficos de dispersión confirman esta observación, mostrando nubes
# de puntos sin patrones definidos.

# Además, se calcularon los factores de inflación de la varianza (VIF),
# obteniéndose valores cercanos a 1 en todas las variables,
# lo que confirma la ausencia de colinealidad relevante.

# ----------------------------
# Paso 6: Relacion Y - X
# ----------------------------


# Analisis de Prevalence

temp_bin <- cut(ae$Temperature, breaks = 6)

prop_pres <- tapply(ae$Prevalence == 1, temp_bin, mean)

x11()
par(mfrow = c(1,3), mar = c(6,4,2,1))
plot(prop_pres, type = "b", pch = 16,
     main = "Probabilidad de presencia vs temperatura",
     ylab = "Probabilidad de presencia",
     xlab = "",
     xaxt = "n")

axis(1, at = 1:length(prop_pres), labels = names(prop_pres), las = 2)


ph_bin <- cut(ae$pH, breaks = 5)

prop_ph <- tapply(ae$Prevalence == 1, ph_bin, mean)

plot(prop_ph, type = "b", pch = 16,
     main = "Probabilidad de presencia vs pH",
     ylab = "Probabilidad de presencia",
     xlab = "",
     xaxt = "n")

axis(1, at = 1:length(prop_ph), labels = names(prop_ph), las = 2)

vol_bin <- cut(ae$`log volume`, breaks = 6)

prop_vol <- tapply(ae$Prevalence == 1, vol_bin, mean)

plot(prop_vol, type = "b", pch = 16,
     main = "Probabilidad de presencia vs log volume",
     ylab = "Probabilidad de presencia",
     xlab = "",
     xaxt = "n")

axis(1, at = 1:length(prop_vol), labels = names(prop_vol), las = 2)


# Analizamos la abundancia (criaderos con mosquitos)

ae_pos <- ae[ae$`Total mosquito emerged` > 0, ]

# crear intervalos
temp_bin <- cut(ae_pos$Temperature, breaks = 6)

# promedio por intervalo
prom_temp <- tapply(ae_pos$`Total mosquito emerged`, temp_bin, mean)

x11()
par(mfrow = c(1,3), mar = c(6,4,2,1))

# graficar
plot(prom_temp, type = "b", pch = 16,
     main = "Abundancia promedio vs temperatura",
     xlab = "",
     ylab = "",
     xaxt = "n")

# agregar etiquetas del eje X
axis(1, at = 1:length(prom_temp), labels = names(prom_temp), las = 2)

ph_bin <- cut(ae_pos$pH, breaks = 5)
prom_ph <- tapply(ae_pos$`Total mosquito emerged`, ph_bin, mean)

plot(prom_ph, type = "b", pch = 16,
     main = "Abundancia promedio vs pH",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_ph), labels = names(prom_ph), las = 2)

vol_bin <- cut(ae_pos$`log volume`, breaks = 6)
prom_vol <- tapply(ae_pos$`Total mosquito emerged`, vol_bin, mean)

plot(prom_vol, type = "b", pch = 16,
     main = "Abundancia promedio vs log volume",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_vol), labels = names(prom_vol), las = 2)



# Analizamos la abundancia de Aedes aegyptu

ae_pos <- ae[ae$`Aedes aegypti` > 0, ]

# crear intervalos
temp_bin <- cut(ae_pos$Temperature, breaks = 6)

# promedio por intervalo
prom_temp <- tapply(ae_pos$`Aedes aegypti`, temp_bin, mean)

x11()
par(mfrow = c(1,3), mar = c(6,4,2,1))

# graficar
plot(prom_temp, type = "b", pch = 16,
     main = "aegypti promedio vs temperatura",
     xlab = "",
     ylab = "",
     xaxt = "n")

# agregar etiquetas del eje X
axis(1, at = 1:length(prom_temp), labels = names(prom_temp), las = 2)

ph_bin <- cut(ae_pos$pH, breaks = 5)
prom_ph <- tapply(ae_pos$`Aedes aegypti`, ph_bin, mean)

plot(prom_ph, type = "b", pch = 16,
     main = "aegypti promedio vs pH",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_ph), labels = names(prom_ph), las = 2)

vol_bin <- cut(ae_pos$`log volume`, breaks = 6)
prom_vol <- tapply(ae_pos$`Aedes aegypti`, vol_bin, mean)

plot(prom_vol, type = "b", pch = 16,
     main = "aegypti promedio vs log volume",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_vol), labels = names(prom_vol), las = 2)


# Analizamos la abundancia de Aedes albopictus

ae_pos <- ae[ae$`Aedes albopictus` > 0, ]

# crear intervalos
temp_bin <- cut(ae_pos$Temperature, breaks = 6)

# promedio por intervalo
prom_temp <- tapply(ae_pos$`Aedes albopictus`, temp_bin, mean)

x11()
par(mfrow = c(1,3), mar = c(6,4,2,1))

# graficar
plot(prom_temp, type = "b", pch = 16,
     main = "albopictus promedio vs temperatura",
     xlab = "",
     ylab = "",
     xaxt = "n")

# agregar etiquetas del eje X
axis(1, at = 1:length(prom_temp), labels = names(prom_temp), las = 2)

ph_bin <- cut(ae_pos$pH, breaks = 5)
prom_ph <- tapply(ae_pos$`Aedes albopictus`, ph_bin, mean)

plot(prom_ph, type = "b", pch = 16,
     main = "albopictus promedio vs pH",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_ph), labels = names(prom_ph), las = 2)

vol_bin <- cut(ae_pos$`log volume`, breaks = 6)
prom_vol <- tapply(ae_pos$`Aedes albopictus`, vol_bin, mean)

plot(prom_vol, type = "b", pch = 16,
     main = "albopictus promedio vs log volume",
     xlab = "",
     ylab = "",
     xaxt = "n")

axis(1, at = 1:length(prom_vol), labels = names(prom_vol), las = 2)


# En cuanto a la prevalencia, se observan patrones no lineales en función
# de las variables ambientales. La probabilidad de prevalencia tiende a
# concentrarse en ciertos rangos de temperatura, presenta un comportamiento
# no lineal respecto al pH, y aumenta con el logaritmo del volumen de agua.

# En relación a la abundancia (considerando únicamente valores positivos),
# también se observan patrones no lineales. Se identifican
# máximos en rangos intermedios de temperatura y volumen, así como
# variaciones no lineales en función del pH.

# Se observan diferencias en los patrones entre Aedes aegypti
# y Aedes albopictus, lo que sugiere que ambas especies responden
# de manera distinta a las condiciones ambientales.

# Esto indica que no es adecuado asumir un comportamiento similar
# entre especies y que esta diferencia deberá considerarse en el modelado.


# ----------------------------
# Paso 7: Interacciones
# ----------------------------

levels(ae$Season)
table(ae$Season)

ae$temp_grupo <- cut(ae$Temperature, breaks = 3)
table(ae$temp_grupo)

x11()
par(mfrow = c(2,2))
boxplot(ae$`Total mosquito emerged` ~ ae$temp_grupo,
        subset = ae$Season == "feb-march",
        main = "january-march",
        xlab = "Temperature",
        ylab = "Abundancia")

boxplot(ae$`Total mosquito emerged` ~ ae$temp_grupo,
        subset = ae$Season == "april-june",
        main = "april-june",
        xlab = "Temperature",
        ylab = "Abundancia")

boxplot(ae$`Total mosquito emerged` ~ ae$temp_grupo,
        subset = ae$Season == "july-september",
        main = "july-september",
        xlab = "Temperature",
        ylab = "Abundancia")

boxplot(ae$`Total mosquito emerged` ~ ae$temp_grupo,
        subset = ae$Season == "october-december",
        main = "october-december",
        xlab = "Temperature",
        ylab = "Abundancia")


# Se exploró la relación entre la abundancia y la temperatura en función
# de la estación del año.

# Se observan diferencias claras en la distribución de la abundancia
# entre estaciones. En particular, la estación july–september presenta
# mayor cantidad de valores positivos y mayor dispersión, mientras que
# en las demás estaciones predominan los valores cercanos a cero.

# Esto indica que la estación no solo afecta el nivel de abundancia,
# sino que también podría modificar la relación entre la temperatura
# y la respuesta.


# ----------------------------
# Paso 8: Independencia
# ----------------------------

# Las observaciones no son independientes debido a la forma en que se tomaron los datos.

# Por un lado, dentro de cada grilla se realizaron muchas mediciones,
# por lo que esas observaciones comparten el mismo contexto ambiental.

# Por otro lado, cada grilla fue muestreada en 4 estaciones del año,
# generando grupos de mediciones correspondientes a cada visita (grilla-estación),
# donde las observaciones dentro de la misma visita son aún más similares entre sí.

# Esta estructura deberá ser tenida en cuenta en el modelado.

# ====================================
# Conclusiones del análisis exploratorio
# ====================================

# La variable respuesta presenta una alta proporción de ceros,
# una distribución asimétrica y relaciones no lineales con las variables explicativas.

# Además, se identificaron diferencias entre la presencia de mosquitos
# y la abundancia cuando están presentes, lo que sugiere que se trata
# de dos procesos distintos.

# Estas características indican que no es adecuado utilizar modelos lineales simples.

# En consecuencia, se requieren modelos que consideren la naturaleza de conteo
# de los datos, el exceso de ceros y la posible separación entre presencia
# y abundancia.