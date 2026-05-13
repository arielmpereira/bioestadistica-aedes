# ==========================================
# TP2 Bioestadística - Modelado Abundancia
# ==========================================


library(readxl)
library(dplyr)
library(lme4)
library(glmmTMB)
library(DHARMa)

# Ajustar esta ruta al directorio de trabajo donde se encuentra el repositorio
setwd("/home/ariel/Repos/bioestadistica-aedes")

# Cargar base
datos <- suppressWarnings(
  datos <- read_excel("aedes_data.xlsx")
)
dim(datos)
str(datos)


# ===============================================================
# Paso 1: Filtrado de datos y seleccion de variables
# ==============================================================

summary(datos$`log volume`)
sum(is.na(datos$`log volume`))

datos |>
  filter(`log volume` == 0) |> 
  filter(`Final volume water(ml) without cm` != 0) |> 
  select(`Final volume water(ml) without cm`, `log volume`)

# Nos quedamos solo con las grillas tipo Index, porque son las que fueron
# relevadas en distintas estaciones.

# También excluimos `log volume` = 0 para excluir criaderos sin agua.
# En la base, cuando el volumen de agua es 0 ml, `log volume` también aparece
# como 0. Osea, en la base, `log volume` = 0 corresponde a criaderos secos.

# Estos criaderos secos son ceros estructurales claros: sin agua no puede haber
# larvas. Sin embargo, entre los criaderos con agua todavía pueden quedar ceros
# por otras causas, como temperatura, pH, tipo de criadero u otras condiciones
# no observadas.

# Por eso, luego del filtrado, los ceros restantes pueden tener origen mixto,
# lo que justifica explorar modelos inflados en cero.

# ===============================
# Selección de variables
# ===============================
#
# Para modelar abundancia, las variables seleccionadas son:
#
# Variables respuesta:
# - `Total mosquito emerged`
# - `Aedes aegypti`
# - `Aedes albopictus`

# Variables explicativas:
# - Season
# - Macrohabitat
# - Microhabitat
# - Temperature
# - pH
# - log volume

# Las variables continuas se estandarizan para facilitar la comparación
# entre efectos:
# - temp_std
# - pH_std
# - logvol_std

# Como las grillas tipo Index fueron relevadas en distintas estaciones,
# se considera que puede existir dependencia entre observaciones de una
# misma grilla. Por eso, Grid_no se evalúa como posible efecto aleatorio.

datos_mod <- datos %>%
  # filtramos grillas index y criaderos activos
  filter(
    Grid_type == "Index",
    `log volume` > 0
  ) %>%
  # seleccionamos variables
  select(
    Prevalence,
    `Total mosquito emerged`,
    `Aedes aegypti`,
    `Aedes albopictus`,
    Season,
    Temperature,
    pH,
    `log volume`,
    Macrohabitat,
    Microhabitat,
    Grid_no
  ) %>%
  # Convertimos variables categóricas
  mutate(
    Season = as.factor(Season),
    Macrohabitat = as.factor(Macrohabitat),
    Microhabitat = as.factor(Microhabitat),
    Grid_no = as.factor(Grid_no)
  )

dim(datos_mod)

# Estandarizamos variables predictoras continuos

datos_mod <- datos_mod %>%
  mutate(
    temp_std = as.numeric(scale(Temperature)),
    pH_std = as.numeric(scale(pH)),
    logvol_std = as.numeric(scale(`log volume`))
  )

# Inspección rápida

dim(datos_mod)
str(datos_mod)

# summary(datos_mod)
colSums(is.na(datos_mod))


# =================================================
# Paso 2: Evaluacion de ceros
# =================================================

# Ceros falsos: Se supone que existen ceros falsos pero no son evidentes
# entonces no podemos eliminarlos.

# Ceros estructurales: eliminamos ceros estructurales evidentes con
# la condición de volumen de agua = 0

# Aunque filtramos estos ceros estructurales
# podrian seguir existiendo condiciones que generen ceros
# estructurales

# Por ejemplo:
# temperaturas muy altas o bajas
# pH extremo, etc
# como conclusion, parte de los ceros podrian ser estructurales


# ====================================================
# Paso 3: Evaluación de sobredispersion e inflacion de ceros
# ====================================================

resumen_abundancia <- data.frame(
  respuesta = c("Total mosquito emerged", "Aedes aegypti","Aedes albopictus"),
  
  n = c(
    length(datos_mod$`Total mosquito emerged`),
    length(datos_mod$`Aedes aegypti`),
    length(datos_mod$`Aedes albopictus`)
  ),
  
  proporcion_ceros = c(
    mean(datos_mod$`Total mosquito emerged` == 0, na.rm = TRUE),
    mean(datos_mod$`Aedes aegypti` == 0, na.rm = TRUE),
    mean(datos_mod$`Aedes albopictus` == 0, na.rm = TRUE)
  ),
  
  media = c(
    mean(datos_mod$`Total mosquito emerged`, na.rm = TRUE),
    mean(datos_mod$`Aedes aegypti`, na.rm = TRUE),
    mean(datos_mod$`Aedes albopictus`, na.rm = TRUE)
  ),
  
  varianza = c(
    var(datos_mod$`Total mosquito emerged`, na.rm = TRUE),
    var(datos_mod$`Aedes aegypti`, na.rm = TRUE),
    var(datos_mod$`Aedes albopictus`, na.rm = TRUE)
  )
)

resumen_abundancia$indice_dispersion <- resumen_abundancia$varianza / resumen_abundancia$media

resumen_abundancia

# Interpretación:
# Las tres respuestas presentan una proporción muy alta de ceros:
# - Total mosquito emerged: 83.3%
# - Aedes aegypti: 91.9%
# - Aedes albopictus: 95.7%
#
# Además, el índice de dispersión es mucho mayor que 1 en los tres casos:
# - Total mosquito emerged: 11.8
# - Aedes aegypti: 14.6
# - Aedes albopictus: 12.1
#
# En una distribución Poisson, la media y la varianza deberían ser similares,
# por lo que el índice de dispersión debería estar cerca de 1.
# Estos valores indican sobredispersión y sugieren que un modelo Poisson simple
# no sería adecuado.
#
# Por eso, en los pasos siguientes se comparan modelos binomiales negativos
# y modelos binomiales negativos inflados en cero.


#====================================
# Paso 4: Comparación de modelos
#====================================
#
# Se comparan modelos con la misma estructura de covariables,
# cambiando solamente la familia o la inclusión del componente de inflación
# de ceros.
#
# conteo:
# respuesta ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no)
#
# inflación en ceros:
# ziformula = ~ Season  
#
# Grid_no se incluye como efecto aleatorio porque las grillas tipo Index fueron
# relevadas en distintas estaciones.

# ------------------------------
# 4.1 Abundancia total
# ------------------------------

modelo_total_pois <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = poisson,
  data = datos_mod
)

modelo_total_nb <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)

modelo_total_zinb <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_total_pois, modelo_total_nb, modelo_total_zinb)

# ------------------------------
# 4.2 Abundancia de Aedes aegypti
# ------------------------------

modelo_aegypti_pois <- glmmTMB(
  `Aedes aegypti` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = poisson,
  data = datos_mod
)

modelo_aegypti_nb <- glmmTMB(
  `Aedes aegypti` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_zinb <- glmmTMB(
  `Aedes aegypti` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_aegypti_pois, modelo_aegypti_nb, modelo_aegypti_zinb)

# ------------------------------
# 4.3 Abundancia de Aedes albopictus
# ------------------------------

modelo_albopictus_pois <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = poisson,
  data = datos_mod
)

modelo_albopictus_nb <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_zinb <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_albopictus_pois, modelo_albopictus_nb, modelo_albopictus_zinb)

# | respuesta              | AIC_poisson |  AIC_NB | AIC_ZINB | delta_Poisson_NB | delta_NB_ZINB |
# | ---------------------- | ----------: | ------: | -------: | ---------------: | ------------: |
# | Total mosquito emerged |     4154.14 | 1882.88 |  1836.12 |          2271.26 |         46.76 |
# | Aedes aegypti          |     2727.80 | 1070.36 |  1062.86 |          1657.44 |          7.50 |
# | Aedes albopictus       |     1354.90 |  611.74 |   596.06 |           743.16 |         15.68 |

# Interpretación:
#
# En las tres respuestas, el modelo Poisson presenta valores de AIC mucho más
# altos que el modelo binomial negativo. Esto confirma que la sobredispersión
# observada en el análisis exploratorio afecta fuertemente a los conteos.
#
# Los modelos ZINB presentan el menor AIC en las tres respuestas.
# La mejora respecto del modelo NB es clara para la abundancia total y para
# Aedes albopictus, y más moderada para Aedes aegypti.
#
# Por lo tanto, para continuar el análisis se utlizan modelos binomiales
# negativos inflados en cero, especialmente porque permiten modelar dos procesos:
# la abundancia esperada y la probabilidad de ceros extra.


# =============================================================
# Paso 5:  Modelos ZINB para la Abundancia Total
# =============================================================

# ----------------------------
# 5.1 Comparacion de modelos:
# ----------------------------

modelo_total_zinb_base <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_total_zinb_macro <- glmmTMB(
  `Total mosquito emerged` ~ Season + Macrohabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_total_zinb_micro <- glmmTMB(
  `Total mosquito emerged` ~ Season + Microhabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_total_zinb_base,
  modelo_total_zinb_macro,
  modelo_total_zinb_micro
)

# Para la abundancia total, agregar Macrohabitat o Microhabitat no mejora el
# ajuste del modelo según AIC. Por eso se conserva el modelo base.


# ------------------------------------------------------------
# 5.2 Simplificacion del modelo base para la Abundancia Total
# -------------------------------------------------------------

# Evaluamos si retirar la variable simplifica el modelo sin empeorar el ajuste.

modelo_total_sin_pH <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_total_sin_temp <- glmmTMB(
  `Total mosquito emerged` ~ Season + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_total_sin_logvol <- glmmTMB(
  `Total mosquito emerged` ~ Season + temp_std + pH_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_total_solo_season <- glmmTMB(
  `Total mosquito emerged` ~ Season + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_total_zinb_base,
  modelo_total_sin_pH,
  modelo_total_sin_temp,
  modelo_total_sin_logvol,
  modelo_total_solo_season
)


# Interpretación:
#
# Para la abundancia total, agregar Macrohabitat o Microhabitat no mejoró el
# ajuste, ya que ambos modelos aumentaron el AIC respecto del modelo base.
#
# Luego, a partir del modelo base, se evaluaron modelos reducidos retirando
# covariables.
#
# El modelo con menor AIC fue el modelo sin pH_std:
#
# `Total mosquito emerged` ~ Season + temp_std + logvol_std + (1 | Grid_no)
# ziformula = ~ Season
#
# El modelo sin pH tiene un parámetro menos y un AIC levemente menor. Por
# criterio de parsimonia, se selecciona como modelo final para la abundancia
# total.

modelo_total_final <- modelo_total_sin_pH

# -------------------------------------------
# 5.5 Diagnostico DHARMA
#--------------------------------------------

res_total_final <- simulateResiduals(modelo_total_final)

plot(res_total_final)

# ------------------------------------------------------------
# 5.4 Interpretacion del modelo para la Abundancia Total
# -------------------------------------------------------------

summary(modelo_total_final)

# Modelo final:
#
# `Total mosquito emerged` ~ Season + temp_std + logvol_std + (1 | Grid_no)
# ziformula = ~ Season
# family = nbinom2
#
# El modelo tiene dos partes:
#
# 1) Parte condicional:
#    Modela la abundancia esperada de mosquitos emergidos.
#    Como utiliza link log, los coeficientes se interpretan exponenciándolos.
#
# 2) Parte de inflación de ceros:
#    Modela la probabilidad de ceros extra.
#    Como utiliza una estructura logística, los coeficientes también pueden
#    exponenciarse para interpretarse como odds ratios.

# Parte condicional:
#
# En la parte de abundancia, Season feb-march presenta un efecto positivo
# respecto de la estación de referencia april-june. Manteniendo constantes las
# demás variables, la abundancia esperada en feb-march fue aproximadamente
# 2.5 veces mayor que en april-june.
#
# La temperatura estandarizada presenta un efecto negativo significativo.
# Por cada aumento de una desviación estándar en temperatura, la abundancia
# esperada se multiplica por aproximadamente 0.74, es decir, disminuye cerca
# de un 26%.
#
# El log-volumen estandarizado muestra una tendencia negativa, aunque con
# evidencia más débil. Por cada aumento de una desviación estándar en log-volumen,
# la abundancia esperada se multiplicaría por aproximadamente 0.85.
#
# Las estaciones july-september y october-december no muestran diferencias claras
# respecto de april-june en la parte condicional del modelo.

# Parte de inflación de ceros:
#
# En la parte de ceros extra, october-december presenta un efecto positivo claro.
# Esto indica que en october-december aumentan las chances de pertenecer al
# proceso de ceros extra. Exponenciando el coeficiente, esas chances son
# aproximadamente 4.5 veces mayores que en april-june.
#
# En july-september se observa una tendencia negativa, lo que sugiere menor
# probabilidad de ceros extra respecto de april-june, aunque la evidencia es
# marginal.
#
# El efecto aleatorio de Grid_no tuvo una varianza estimada prácticamente nula.
# Se mantuvo en el modelo para respetar la estructura longitudinal del muestreo,
# aunque su aporte a la variabilidad del modelo fue bajo.


# ============================================================================

# modelo_aegypti_zinb <- glmmTMB(
#   `Aedes aegypti` ~ Season + Microhabitat + temp_std + logvol_std + (1 | Season),
#   ziformula = ~ Season,
#   family = nbinom2,
#  data = datos_mod
# )

# ==================================================
# Paso 6: Modelos ZINB para la Abundancia de Aedes aegypti
# ==================================================

# ----------------------------
# 6.1 Comparacion de modelos:
# ----------------------------

modelo_aegypti_zinb_base <- glmmTMB(
  `Aedes aegypti` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_zinb_macro <- glmmTMB(
  `Aedes aegypti` ~ Season + Macrohabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_zinb_micro <- glmmTMB(
  `Aedes aegypti` ~ Season + Microhabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_aegypti_zinb_base,
  modelo_aegypti_zinb_macro,
  modelo_aegypti_zinb_micro
)


# Para Aedes aegypti, el modelo con Microhabitat presentó el menor AIC.
# Sin embargo, Microhabitat tiene varias categorías y algunas de ellas tienen
# muy pocas observaciones.
#
# Por esta razón, aunque el modelo con Microhabitat mejora el AIC, no se lo
# considera adecuado para realizar inferencias.
#
# Se decide continuar con el modelo que incluye Macrohabitat, ya que representa
# una estructura ecológica más simple, con menos categorías y mayor estabilidad
# para la interpretación.


# ------------------------------------------------------------
# 6.2 Simplificacion del modelo con Macrohabitat
# -------------------------------------------------------------

# Evaluamos si retirar alguna covariable simplifica el modelo
# sin empeorar el ajuste.

modelo_aegypti_macro_sin_pH <- glmmTMB(
  `Aedes aegypti` ~ Season + Macrohabitat + temp_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_macro_sin_temp <- glmmTMB(
  `Aedes aegypti` ~ Season + Macrohabitat + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_macro_sin_logvol <- glmmTMB(
  `Aedes aegypti` ~ Season + Macrohabitat + temp_std + pH_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_aegypti_zinb_macro,
  modelo_aegypti_macro_sin_pH,
  modelo_aegypti_macro_sin_temp,
  modelo_aegypti_macro_sin_logvol
)


# El modelo con menor AIC fue el modelo sin pH_std:
#
# `Aedes aegypti` ~ Season + Macrohabitat + temp_std + logvol_std + (1 | Grid_no)
# ziformula = ~ Season
#

modelo_aegypti_final <- modelo_aegypti_macro_sin_pH

# ------------------------------------------------------------
# 6.3 Diagnostico del modelo final con DHARMa
# -------------------------------------------------------------

res_aegypti_final <- simulateResiduals(modelo_aegypti_final)

x11()
plot(res_aegypti_final)


# El diagnóstico de residuos simulados con DHARMa no muestra problemas
# significativos de ajuste general. El test de uniformidad no detecta
# desviaciones significativas y el QQ plot se ajusta adecuadamente a la diagonal.
#
# El test de dispersión tampoco detecta sobredispersión significativa remanente,
# aunque el valor queda relativamente cercano al umbral convencional.
#
# El gráfico de residuos contra valores predichos no muestra patrones
# sistemáticos importantes y DHARMa indica que no se detectan problemas
# significativos.
#
# Por lo tanto, el modelo seleccionado para Aedes aegypti se considera
# aceptable para continuar con la interpretación.

# ------------------------------------------------------------
# 6.4 Interpretacion del modelo para la Abundancia Total
# -------------------------------------------------------------

summary(modelo_aegypti_final)

# ------------------------------------------------------------
# 6.4 Interpretacion del modelo para Aedes aegypti
# -------------------------------------------------------------

summary(modelo_aegypti_final)

# Modelo final:
#
# `Aedes aegypti` ~ Season + Macrohabitat + temp_std + logvol_std + (1 | Grid_no)
# ziformula = ~ Season
# family = nbinom2
#
# El modelo tiene dos partes:
#
# 1) Parte condicional:
#    Modela la abundancia esperada de Aedes aegypti.
#    Como utiliza link log, los coeficientes se interpretan exponenciándolos.
#
# 2) Parte de inflación de ceros:
#    Modela la probabilidad de ceros extra.
#    Como utiliza una estructura logística, los coeficientes pueden interpretarse
#    como odds ratios al exponenciarlos.

# Parte condicional:
#
# En la parte de abundancia, Season feb-march presenta un efecto positivo
# respecto de la estación de referencia april-june. Manteniendo constantes las
# demás variables, la abundancia esperada en feb-march fue aproximadamente
# 3.1 veces mayor que en april-june.
#
# Macrohabitat Lake y Plantation presentan efectos negativos respecto de la
# categoría de referencia Barren Land. Esto sugiere menor abundancia esperada
# de Aedes aegypti en esos macrohábitats.
#
# La temperatura estandarizada presenta un efecto negativo significativo.
# Por cada aumento de una desviación estándar en temperatura, la abundancia
# esperada se multiplica por aproximadamente 0.69, es decir, disminuye cerca
# de un 31%.
#
# El log-volumen estandarizado muestra una tendencia negativa, aunque con
# evidencia marginal. Por cada aumento de una desviación estándar en log-volumen,
# la abundancia esperada tendería a multiplicarse por aproximadamente 0.72.
#
# Las estaciones july-september y october-december no muestran diferencias claras
# respecto de april-june en la parte condicional del modelo.

# Parte de inflación de ceros:
#
# En la parte de ceros extra, october-december presenta un efecto positivo claro.
# Esto indica que en october-december aumentan las chances de pertenecer al
# proceso de ceros extra. Exponenciando el coeficiente, esas chances son
# aproximadamente 3.2 veces mayores que en april-june.
#
# Las estaciones feb-march y july-september no muestran diferencias claras
# respecto de april-june en la probabilidad de ceros extra.
#
# El efecto aleatorio de Grid_no tuvo una varianza estimada prácticamente nula.
# Se mantuvo en el modelo para respetar la estructura longitudinal del muestreo,
# aunque su aporte a la variabilidad del modelo fue bajo.



# ==============================================================================


# =====================================================
# Paso 7: Modelos ZINB para la Abundancia de Albopictus
# =====================================================


# ----------------------------
# 7.1 Comparacion de modelos:
# ----------------------------

modelo_albopictus_zinb_base <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_zinb_macro <- glmmTMB(
  `Aedes albopictus` ~ Season + Macrohabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_zinb_micro <- glmmTMB(
  `Aedes albopictus` ~ Season + Microhabitat + temp_std + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_albopictus_zinb_base,
  modelo_albopictus_zinb_macro,
  modelo_albopictus_zinb_micro
)

# Para Aedes albopictus, el modelo con menor AIC fue el modelo base,
# que incluye Season, temp_std, pH_std, logvol_std y el efecto aleatorio
# de Grid_no.
#
# Agregar Macrohabitat o Microhabitat no mejoró el ajuste del modelo, ya que
# en ambos casos el AIC aumentó.
#
# Por lo tanto, para Aedes albopictus se continúa con el modelo base como punto
# de partida para la simplificación.

# ------------------------------------------------------------
# 7.2 Simplificacion del modelo base para Aedes albopictus
# -------------------------------------------------------------

# Evaluamos si retirar alguna covariable simplifica el modelo sin empeorar
# el ajuste.

modelo_albopictus_sin_pH <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_sin_temp <- glmmTMB(
  `Aedes albopictus` ~ Season + pH_std + logvol_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_sin_logvol <- glmmTMB(
  `Aedes albopictus` ~ Season + temp_std + pH_std + (1 | Grid_no),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(
  modelo_albopictus_zinb_base,
  modelo_albopictus_sin_pH,
  modelo_albopictus_sin_temp,
  modelo_albopictus_sin_logvol
)

# El modelo con menor AIC fue el modelo sin logvol_std:
#
# `Aedes albopictus` ~ Season + temp_std + pH_std + (1 | Grid_no)
# ziformula = ~ Season
#
# La diferencia de AIC con el modelo base completo es pequeña, pero el modelo
# sin logvol_std tiene un parámetro menos y un AIC levemente menor. Por criterio
# de parsimonia, se selecciona como modelo final para Aedes albopictus.

modelo_albopictus_final <- modelo_albopictus_sin_logvol

# ------------------------------------------------------------
# 7.3 Diagnostico del modelo final con DHARMa
# -------------------------------------------------------------

res_albopictus_final <- simulateResiduals(modelo_albopictus_final)

x11()
plot(res_albopictus_final)


# El diagnóstico de residuos simulados con DHARMa no muestra problemas
# significativos de ajuste general. El test de uniformidad no detecta
# desviaciones significativas y el QQ plot se ajusta adecuadamente a la diagonal.
#
# El test de dispersión tampoco detecta sobredispersión significativa remanente,
# aunque el valor queda relativamente cercano al umbral convencional.
#
# El test de outliers no muestra desviaciones significativas.
#
# El gráfico de residuos contra valores predichos no muestra patrones
# sistemáticos importantes y DHARMa indica que no se detectan problemas
# significativos.
#
# Por lo tanto, el modelo seleccionado para Aedes albopictus se considera
# aceptable para continuar con la interpretación.


# ------------------------------------------------------------
# 7.4 Interpretacion del modelo para la Abundancia de Albopictus
# -------------------------------------------------------------

summary(modelo_albopictus_final)

# ------------------------------------------------------------
# 7.4 Interpretacion del modelo para Aedes albopictus
# -------------------------------------------------------------

summary(modelo_albopictus_final)

# Modelo final:
#
# `Aedes albopictus` ~ Season + temp_std + pH_std + (1 | Grid_no)
# ziformula = ~ Season
# family = nbinom2
#
# El modelo tiene dos partes:
#
# 1) Parte condicional:
#    Modela la abundancia esperada de Aedes albopictus.
#    Como utiliza link log, los coeficientes se interpretan exponenciándolos.
#
# 2) Parte de inflación de ceros:
#    Modela la probabilidad de ceros extra.
#    Como utiliza una estructura logística, los coeficientes pueden interpretarse
#    como odds ratios al exponenciarlos.

# Parte condicional:
#
# En la parte de abundancia, las estaciones no muestran diferencias claras
# respecto de la estación de referencia april-june.
#
# La temperatura presenta un coeficiente positivo, pero sin evidencia estadística
# suficiente para afirmar un efecto claro sobre la abundancia esperada.
#
# El pH muestra una tendencia positiva marginal. Por cada aumento de una
# desviación estándar en pH, la abundancia esperada tendería a multiplicarse por
# aproximadamente 1.38. Como el valor de p queda en el límite, este efecto debe
# interpretarse con prudencia.
#
# El efecto aleatorio de Grid_no presenta una varianza apreciable, lo que indica
# variabilidad entre grillas no explicada completamente por las covariables del
# modelo.

# Parte de inflación de ceros:
#
# En la parte de ceros extra, july-september presenta un efecto negativo claro.
# Esto indica que durante july-september disminuyen fuertemente las chances de
# pertenecer al proceso de ceros extra respecto de april-june.
#
# Exponenciando el coeficiente, las chances de ceros extra en july-september son
# aproximadamente 0.045 veces las de april-june.
#
# Las estaciones feb-march y october-december no muestran diferencias claras
# respecto de april-june en la probabilidad de ceros extra.


############### FIN ##############################
