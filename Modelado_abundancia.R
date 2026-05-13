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
# Paso 3: evaluar sobredispersion e inflacion de ceros
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
# Por lo tanto, para continuar el análisis se priorizan modelos binomiales
# negativos inflados en cero, especialmente porque permiten modelar dos procesos:
# la abundancia esperada y la probabilidad de ceros extra.


# =====================================================
# Paso 5: Comparación ecológica de modelos ZINB
# =====================================================

# Una vez observado que los modelos ZINB tienen mejor desempeño general,
# se comparan diferentes estructuras ecológicas manteniendo la misma familia.

# ------------------------------
# 5.1 Abundancia total
# ------------------------------

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


# Interpretación:
#
# Para la abundancia total, el modelo con menor AIC fue el modelo ZINB base,
# que incluye Season, temp_std, pH_std, logvol_std y el efecto aleatorio de Grid_no.
#
# Agregar Macrohabitat o Microhabitat no mejoró el ajuste del modelo, ya que en
# ambos casos el AIC aumentó.
#
# Por lo tanto, para la abundancia total se conserva el modelo ZINB base.

# ------------------------------
# 5.2 Abundancia de Aedes aegypti
# ------------------------------

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

# ------------------------------
# 5.3 Abundancia de Aedes albopictus
# ------------------------------

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



# Modelo NB

modelo_nb <- glmmTMB(
  `Total mosquito emerged` ~  Season + Microhabitat + temp_std + logvol_std + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)


# Interpretación:
#
# Para Aedes aegypti, el modelo con menor AIC fue el modelo que incorpora
# Microhabitat. La mejora respecto del modelo base fue moderada, lo que sugiere
# que el tipo de criadero aporta información para explicar la abundancia de esta
# especie.
#
# Para Aedes albopictus, el modelo con menor AIC fue el modelo base. La inclusión
# de Macrohabitat o Microhabitat no mejoró el ajuste, ya que ambos aumentaron el
# AIC.
#
# Esto sugiere que las dos especies no responden exactamente igual a las
# covariables ecológicas: Aedes aegypti parece estar más asociado al tipo de
# criadero, mientras que para Aedes albopictus el modelo base resulta suficiente.

# | Respuesta                | Modelo seleccionado           | Comentario                                    |
# | ------------------------ | ----------------------------- | --------------------------------------------- |
# | `Total mosquito emerged` | `modelo_total_zinb_base`      | Macrohabitat y Microhabitat no mejoran el AIC |
# | `Aedes aegypti`          | `modelo_aegypti_zinb_micro`   | Microhabitat mejora moderadamente el AIC      |
# | `Aedes albopictus`       | `modelo_albopictus_zinb_base` | Macrohabitat y Microhabitat no mejoran el AIC |


# Modelos seleccionados para interpretación

modelo_total_final <- modelo_total_zinb_base
modelo_aegypti_final <- modelo_aegypti_zinb_micro
modelo_albopictus_final <- modelo_albopictus_zinb_base


summary(modelo_total_final)
summary(modelo_aegypti_final)
summary(modelo_albopictus_final)

# Interpretación preliminar de los modelos finales:
#
# El modelo de abundancia total y el modelo de Aedes albopictus entregan errores
# estándar, valores z y p-valores, por lo que pueden interpretarse.
#
# En cambio, el modelo de Aedes aegypti con Microhabitat presenta errores
# estándar NaN en todos los coeficientes. Esto indica problemas de estimación,
# probablemente asociados a la complejidad del modelo, el exceso de ceros y/o
# categorías de Microhabitat con pocos datos.
#
# Por esta razón, aunque el modelo con Microhabitat tuvo menor AIC, no se lo
# considera confiable para inferencia directa. Se evaluará una versión más simple
# antes de seleccionar el modelo final para Aedes aegypti.

# Modelo ZINB
# Season
# Macrohabitat
# Microhabitat
# Temperature
# pH
# log volume

modelo_zinb <- glmmTMB(
  `Total mosquito emerged` ~  Season  + Microhabitat + temp_std + logvol_std + (1 | Grid_no),
  
  ziformula = ~  Season,
  family = nbinom2,
  data = datos_mod
)

# Paso 5: Comparacion con AIC

AIC(modelo_nb, modelo_zinb)

# La diferencia de AIC (29) es considerablemente mayor a 10, lo que indica una
# mejora sustancial en el ajuste del modelo ZINB respecto al modelo NB.

# Este resultado sugiere que la inclusión de un componente de inflación de ceros
# permite capturar mejor la estructura de los datos.


# ==========================
# Diagnóstico de modelos con DHARMa
# ==========================

# Simula residuos del modelo.
# DHARMa compara lo que el modelo predice con lo que realmente se observó.

res_zinb <- simulateResiduals(modelo_zinb)

x11()
plot(res_zinb)

# Gráfico diagnóstico DHARMa

# En el gráfico QQ, los residuos simulados se alinean adecuadamente con la línea
# esperada, lo que sugiere que la distribución asumida por el modelo es
# consistente con la estructura de los datos.

# Los tests estadísticos asociados no resultan significativos

# Uniformidad de residuos (KS test p = 0.74):
# p > 0.05 No se detectan problemas importantes

# Test de dispersión (p = 0.72):
# p > 0.05 La dispersion parece adecuada

# Test de outliers (p = 0.38):
# p > 0.05 Cantidad de outliers razonable

# Por otro lado, el gráfico de residuos versus valores predichos no muestra
# patrones sistemáticos relevantes. Las curvas de cuantiles se mantienen
# aproximadamente horizontales y dentro de las bandas de confianza,
# lo que indica que el modelo captura adecuadamente la relación entre
# la variable respuesta y las covariables.


summary(modelo_zinb)


# ==========================
# Interpretación del modelo ZINB
# ==========================

# El modelo ZINB tiene dos componentes:
# 1) El modelo condicional, que explica la abundancia de larvas.

# 2) El modelo de inflación de ceros, que explica la
# probabilidad de ceros extra en los datos.

# --------------------------
# Efecto aleatorio
# --------------------------

# La varianza asociada a Grid_no es prácticamente nula
# (Std.Dev ≈ 0.0001).

# Esto sugiere que, una vez consideradas las variables incluidas en el modelo,
# no queda una variabilidad importante entre grillas.

# --------------------------
# Modelo condicional: abundancia
# --------------------------

# El intercepto representa la abundancia esperada en escala logarítmica
# para la categoría de referencia de Season y Microhabitat,
# con temperatura y volumen en su valor promedio.

# Intercepto: 0.94 (p = 0.015)

# exp(0.94) = 2.56

# Esto indica que, en la condición de referencia,
# la abundancia esperada es aproximadamente 2.6 larvas.

# --------------------------
# Season
# --------------------------

# Seasonfeb-march: 1.08 (p < 0.001)

# Presenta un efecto positivo y significativo.
# Esto indica que en feb-march la abundancia esperada de larvas
# es mayor respecto a la estación de referencia.

# exp(1.08) ≈ 2.97

# La abundancia esperada es aproximadamente 3 veces mayor
# que en la estación de referencia.

# Seasonjuly-september: 0.18 (p = 0.49)

# No se detectan diferencias significativas respecto a la estación de referencia.

# Seasonoctober-december: -0.22 (p = 0.56)

# Tampoco se observan diferencias significativas respecto
# a la estación de referencia.

# --------------------------
# Microhabitat
# --------------------------

# Ninguna categoría de Microhabitat presenta efectos
# claramente significativos al nivel 0.05.

# Grinding stones muestra una tendencia positiva marginal
# (Estimate = 0.68  p = 0.063).

# exp(0.68) ≈ 1.97

# Esto podría indicar una abundancia aproximadamente
# 2 veces mayor respecto al microhábitat de referencia,
# aunque la evidencia no es concluyente.

# --------------------------
# Variables continuas
# --------------------------

# temp_std: -0.37 (p = 0.003)

# La temperatura presenta un efecto negativo significativo.

# Esto indica que, a medida que aumenta la temperatura,
# la abundancia esperada de larvas tiende a disminuir,
# manteniendo constantes las demás variables.

# exp(-0.37) = 0.68

# La abundancia esperada disminuye aproximadamente un 32%
# por cada aumento de una desviación estándar en temperatura.

# logvol_std: -0.14 (p = 0.35)

# No se detecta un efecto significativo del volumen sobre la abundancia.

# --------------------------
# Modelo de inflación de ceros
# --------------------------

# Este componente modela la probabilidad de generar ceros extra,
# es decir, observaciones donde no aparecen larvas.

# Intercepto: 1.03 (p < 0.001)

# Indica una probabilidad importante de ceros extra
# en la condición de referencia.

# Seasonfeb-march: 0.44 (p = 0.16)

# No muestra diferencias significativas respecto a la estación de referencia.

# Seasonjuly-september: -0.47 (p ≈ 0.085)

# Presenta una tendencia a menor probabilidad de ceros extra,
# aunque la evidencia no es concluyente.

# Seasonoctober-december: 1.54 (p < 0.001)

# Presenta un efecto positivo y significativo sobre la inflación de ceros.

# exp(1.54) ≈ 4.67

# Esto indica que las odds de obtener ceros extra son aproximadamente
# 4.7 veces mayores respecto a la estación de referencia.

# ==========================
# Conclusión final
# ==========================

# El modelo ZINB fue seleccionado porque permitió modelar simultáneamente:
# - la abundancia de larvas
# - y el exceso de ceros observado en los datos.

# Los resultados sugieren que la abundancia está asociada
# principalmente a la estación y a la temperatura.

# En particular, feb-march presenta una abundancia esperada
# considerablemente mayor, mientras que temperaturas más altas
# se asocian a menor abundancia.

# Además, october-december presenta una mayor probabilidad
# de ceros extra, lo que sugiere condiciones menos favorables
# para la presencia de larvas en esa estación.

# En conjunto, el modelo indica que la dinámica de abundancia
# depende tanto de factores ambientales como de procesos
# asociados al exceso de ceros en los datos.




# ==========================
# Modelo abundancia Aedes aegypti
# ==========================

modelo_aegypti_nb <- glmmTMB(
  `Aedes aegypti` ~ Season + Microhabitat + temp_std + logvol_std + (1 | Season),
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_zinb <- glmmTMB(
  `Aedes aegypti` ~ Season + Microhabitat + temp_std + logvol_std + (1 | Season),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_aegypti_nb, modelo_aegypti_zinb)

# La comparación entre modelos NB y ZINB para Aedes aegypti mostró una diferencia
# de AIC de 2 (< 10), lo que indica que ambos modelos presentan un
# ajuste equivalente.

# En este contexto, se optó por el modelo NB por su mayor parsimonia,
# evitando la inclusión innecesaria de un componente de inflación de ceros.

# Esto sugiere que, a nivel de esta especie, no se evidencia un proceso
# adicional de generación de ceros que requiera ser modelado explícitamente.

# ==========================
# Diagnóstico de modelos con DHARMa
# ==========================

res_aegypti_nb <- simulateResiduals(modelo_aegypti_nb)

x11()
plot(res_aegypti_nb)

# ==========================
# Diagnóstico DHARMa
# ==========================

# En el gráfico QQ, los residuos simulados siguen de manera adecuada
# la línea esperada, lo que sugiere que la distribución asumida por el
# modelo es compatible con la estructura observada en los datos.

# Los tests estadísticos asociados no resultan significativos:

# Uniformidad de residuos (KS test p = 0.74):
# p > 0.05
# No se detectan desviaciones importantes respecto a la distribución esperada.

# Test de dispersión (p = 0.72):
# p > 0.05
# No se observan problemas importantes de sobredispersión o subdispersión.

# Test de outliers (p = 0.38):
# p > 0.05
# La cantidad de valores extremos observada es consistente con lo esperado
# bajo el modelo.

# En el gráfico de residuos versus valores predichos no se observan
# patrones sistemáticos fuertes.

# Las curvas de cuantiles se mantienen aproximadamente horizontales
# y dentro de las bandas de confianza, lo que indica que el modelo
# representa razonablemente bien la relación entre la abundancia
# de Aedes aegypti y las variables explicativas incluidas.

# Se observan algunos puntos extremos aislados en residuos altos,
# pero no parecen generar una desviación importante del ajuste general.

# En conjunto, los diagnósticos sugieren que el modelo NB presenta
# un ajuste adecuado y no evidencia problemas importantes en los residuos.

summary(modelo_aegypti_nb)

# ==========================
# Interpretación del modelo NB
# Aedes aegypti
# ==========================

# --------------------------
# Efecto aleatorio
# --------------------------

# La varianza asociada al efecto aleatorio de Season es prácticamente nula
# (Std.Dev ≈ 0.000004).

# Esto indica que, una vez consideradas las variables incluidas en el modelo,
# no queda una variabilidad importante entre estaciones a nivel aleatorio.

# --------------------------
# Modelo condicional: abundancia
# --------------------------

# El intercepto representa la abundancia esperada de Aedes aegypti
# en escala logarítmica para la categoría de referencia de Season
# y Microhabitat, con temperatura y volumen en sus valores promedio.

# Intercepto: -1.70 (p = 0.036)

# exp(-1.70) ≈ 0.18

# Esto indica que, en la condición de referencia,
# la abundancia esperada de Aedes aegypti es baja.

# --------------------------
# Season
# --------------------------

# Seasonfeb-march: 0.99 (p ≈ 0.091)

# Presenta una tendencia positiva marginal.
# Esto sugiere una posible mayor abundancia respecto
# a la estación de referencia, aunque la evidencia
# no es concluyente.

# exp(0.99) ≈ 2.69

# La abundancia esperada sería aproximadamente
# 2.7 veces mayor que en la estación de referencia.

# Seasonjuly-september: 0.06 (p = 0.91)

# No se detectan diferencias claras respecto
# a la estación de referencia.

# Seasonoctober-december: -1.58 (p = 0.012)

# Presenta un efecto negativo significativo.

# Esto indica que la abundancia esperada de
# Aedes aegypti es menor en october-december
# respecto a la estación de referencia.

# exp(-1.58) ≈ 0.20

# La abundancia esperada es aproximadamente
# 80% menor respecto a la estación de referencia.

# --------------------------
# Microhabitat
# --------------------------

# MicrohabitatGrinding stones: 2.70 (p = 0.003)

# Presenta un efecto positivo significativo.

# Esto indica que los grinding stones presentan
# una abundancia esperada considerablemente mayor
# respecto al microhábitat de referencia.

# exp(2.70) ≈ 14.9

# La abundancia esperada sería aproximadamente
# 15 veces mayor que en el microhábitat de referencia.

# Las demás categorías de Microhabitat no presentan
# efectos significativos claros.

# En Plant axils y Tree holes aparecen coeficientes
# extremadamente grandes y errores estándar muy altos,
# lo que probablemente indica muy pocos datos o exceso
# de ceros en esas categorías.

# --------------------------
# Variables continuas
# --------------------------

# temp_std: -0.65 (p = 0.003)

# La temperatura presenta un efecto negativo significativo.

# Esto indica que, a medida que aumenta la temperatura,
# la abundancia esperada de Aedes aegypti tiende a disminuir,
# manteniendo constantes las demás variables.

# exp(-0.65) ≈ 0.52

# La abundancia esperada disminuye aproximadamente un 48%
# por cada aumento de una desviación estándar en temperatura.

# logvol_std: 0.30 (p = 0.29)

# No se detecta un efecto claro del volumen
# sobre la abundancia de Aedes aegypti.

# ==========================
# Conclusión final
# ==========================

# El modelo NB sugiere que la abundancia de Aedes aegypti
# está principalmente asociada a la estación, al tipo de
# microhábitat y a la temperatura.

# En particular, october-december presenta menor abundancia
# esperada, mientras que los grinding stones muestran una
# abundancia considerablemente mayor respecto al microhábitat
# de referencia.

# Además, temperaturas más altas se asocian con menor
# abundancia esperada de Aedes aegypti.

# Los diagnósticos DHARMa no evidenciaron problemas importantes
# en los residuos, lo que sugiere que el modelo presenta
# un ajuste adecuado para los datos analizados.


# ==========================
# Modelo abundancia Aedes albopictus
# ==========================

modelo_albopictus_nb <- glmmTMB(
  `Aedes albopictus` ~  Microhabitat + temp_std + logvol_std + (1 | Season),
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_zinb <- glmmTMB(
  `Aedes albopictus` ~ Macrohabitat + temp_std + logvol_std + (1 | Season),
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_albopictus_nb, modelo_albopictus_zinb)

# ==========================
# Selección de modelo - Aedes albopictus
# ==========================

# La comparación entre modelos NB y ZINB muestra una diferencia de AIC
# considerable (49)

# Este valor es mayor a 10, lo que indica una mejora sustancial en el ajuste
# del modelo ZINB respecto al modelo NB.

# En consecuencia, se selecciona el modelo ZINB para modelar la abundancia
# de Aedes albopictus.

# Este resultado sugiere que, a diferencia de Aedes aegypti, en esta especie
# existe evidencia de un proceso adicional que genera ceros, lo que requiere
# ser modelado explícitamente mediante un componente de inflación de ceros.

res_albopictus_zinb <- simulateResiduals(modelo_albopictus_zinb)

x11()
plot(res_albopictus_zinb)

# ==========================
# Diagnóstico DHARMa
# ==========================

# En el gráfico QQ, los residuos simulados siguen adecuadamente
# la línea esperada, lo que indica que la distribución asumida
# por el modelo es compatible con la estructura observada en los datos.

# Los tests estadísticos asociados no resultan significativos:

# Uniformidad de residuos (KS test p = 0.48):
# p > 0.05
# No se detectan desviaciones importantes respecto a la distribución esperada.

# Test de dispersión (p = 0.43):
# p > 0.05
# No se observan problemas importantes de sobredispersión
# o subdispersión residual.

# Test de outliers (p = 0.38):
# p > 0.05
# La cantidad de valores extremos observada es consistente
# con lo esperado bajo el modelo.

# En el gráfico de residuos versus valores predichos
# no se observan patrones sistemáticos fuertes.

# Las curvas de cuantiles se mantienen relativamente horizontales
# y dentro de las bandas de confianza, lo que sugiere que
# el modelo representa razonablemente bien la relación entre
# la abundancia de Aedes albopictus y las variables explicativas.

# Se observan algunos residuos altos aislados,
# pero no parecen generar desviaciones importantes
# del ajuste general.

# En conjunto, los diagnósticos sugieren que el modelo ZINB
# presenta un ajuste adecuado y no evidencia problemas
# importantes en los residuos.


summary(modelo_albopictus_zinb)

# ==========================
# Interpretación del modelo ZINB
# Aedes albopictus
# ==========================

# El modelo ZINB tiene dos componentes:
# 1) El modelo condicional, que explica la abundancia de larvas.
# 2) El modelo de inflación de ceros, que explica la probabilidad
#    de ceros extra en los datos.

# --------------------------
# Efecto aleatorio
# --------------------------

# La varianza asociada al efecto aleatorio de Season es prácticamente nula
# (Std.Dev ≈ 0.00003).

# Esto indica que, una vez consideradas las variables incluidas en el modelo,
# no queda una variabilidad importante entre estaciones a nivel aleatorio.

# --------------------------
# Modelo condicional: abundancia
# --------------------------

# El intercepto representa la abundancia esperada de Aedes albopictus
# en escala logarítmica para la categoría de referencia de Macrohabitat,
# con temperatura y volumen en sus valores promedio.

# Intercepto: 2.16 (p < 0.001)

# exp(2.16) ≈ 8.74

# Esto indica que, en la condición de referencia,
# la abundancia esperada de Aedes albopictus es relativamente alta.

# --------------------------
# Macrohabitat
# --------------------------

# MacrohabitatHigh dense: -1.97 (p = 0.001)

# Presenta un efecto negativo significativo.

# Esto indica que la abundancia esperada de
# Aedes albopictus es menor en zonas High dense
# respecto al macrohábitat de referencia.

# exp(-1.97) ≈ 0.14

# La abundancia esperada es aproximadamente
# 86% menor respecto a la categoría de referencia.

# MacrohabitatLake: -0.59 (p = 0.27)

# No se detectan diferencias claras respecto
# al macrohábitat de referencia.

# MacrohabitatLow dense: 0.07 (p = 0.90)

# No presenta diferencias significativas.

# MacrohabitatMedium dense: -0.86 (p = 0.11)

# Presenta una tendencia negativa,
# aunque la evidencia no es concluyente.

# exp(-0.86) ≈ 0.42

# Esto podría indicar una abundancia esperada
# aproximadamente 58% menor.

# MacrohabitatPlantation: -0.19 (p = 0.66)

# No muestra diferencias claras respecto
# a la categoría de referencia.

# --------------------------
# Variables continuas
# --------------------------

# temp_std: 0.42 (p ≈ 0.062)

# Presenta una tendencia positiva marginal.

# Esto sugiere que, a medida que aumenta la temperatura,
# la abundancia esperada de Aedes albopictus podría aumentar,
# aunque la evidencia no es concluyente.

# exp(0.42) ≈ 1.53

# La abundancia esperada sería aproximadamente
# 1.5 veces mayor por cada aumento de una desviación estándar
# en temperatura.

# logvol_std: -0.33 (p = 0.19)

# No se detecta un efecto claro del volumen
# sobre la abundancia de Aedes albopictus.

# --------------------------
# Modelo de inflación de ceros
# --------------------------

# Este componente modela la probabilidad de obtener
# ceros extra en los datos.

# Intercepto: 5.01 (p < 0.001)

# Indica una probabilidad importante de ceros extra
# en la estación de referencia.

# Seasonfeb-march: -1.12 (p = 0.33)

# No muestra diferencias claras respecto
# a la estación de referencia.

# Seasonjuly-september: -3.12 (p = 0.002)

# Presenta un efecto negativo significativo
# sobre la inflación de ceros.

# exp(-3.12) ≈ 0.04

# Esto indica que las odds de obtener ceros extra
# son aproximadamente 96% menores respecto
# a la estación de referencia.

# En otras palabras, durante july-september
# disminuye considerablemente la probabilidad
# de observar ausencia estructural de larvas.

# Seasonoctober-december: -0.77 (p = 0.49)

# No presenta diferencias significativas
# respecto a la estación de referencia.

# ==========================
# Conclusión final
# ==========================

# El modelo ZINB sugiere que la abundancia de
# Aedes albopictus está principalmente asociada
# al macrohábitat y parcialmente a la temperatura.

# En particular, las zonas High dense presentan
# una abundancia considerablemente menor respecto
# al macrohábitat de referencia.

# Además, july-september muestra una reducción importante
# en la probabilidad de ceros extra, lo que sugiere
# condiciones más favorables para la presencia
# de Aedes albopictus durante esa estación.

# Los diagnósticos DHARMa no evidenciaron problemas
# importantes en los residuos, lo que indica
# un ajuste adecuado del modelo.


