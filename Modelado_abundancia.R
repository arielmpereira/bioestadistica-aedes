# ==========================================
# TP2 Bioestadística - Modelado Abundancia
# ==========================================


library(readxl)
library(dplyr)
library(lme4)
library(glmmTMB)
library(DHARMa)

# Cargar base
datos <- suppressWarnings(
  read.csv("/home/ariel/Repos/bioestadistica-aedes/datos_criadero.csv")
)
dim(datos)
str(datos)


# ==========================
# 1. Preparación de datos
# ==========================

# Se decidió excluir del análisis aquellas observaciones con volumen de agua 
# igual a cero, ya que en estos casos la presencia de larvas es imposible, 
# lo que corresponde a ceros estructurales.
# Además, las variables como temperatura y pH, no representan mediciones reales 
# en ausencia de agua, sino valores nulos que podrían inducir interpretaciones 
# erróneas en el modelo.




datos_mod <- datos %>%
  # filtramos grillas index y criaderos activos
  filter(
    Grid_type == "Index",
    `log.volume` > 0
  ) %>%
  # seleccionamos variables
  select(
    Prevalence,
    `Total.mosquito.emerged`,
    `Aedes.aegypti`,
    `Aedes.albopictus`,
    Season,
    Temperature,
    pH,
    `log.volume`,
    Macrohabitat,
    Microhabitat,
    Grid_no,
    coocurrencia
  ) %>%
  # Convertimos variables categóricas
  mutate(
    Season = as.factor(Season),
    Macrohabitat = as.factor(Macrohabitat),
    Microhabitat = as.factor(Microhabitat),
    Grid_no = as.factor(Grid_no)
  )


# Estandarizamos variables predictoras continuos

datos_mod <- datos_mod %>%
  mutate(
    temp_std = as.numeric(scale(Temperature)),
    pH_std = as.numeric(scale(pH)),
    logvol_std = as.numeric(scale(`log.volume`))
  )

# Inspección rápida

dim(datos_mod)
str(datos_mod)

# summary(datos_mod)
colSums(is.na(datos_mod))



# Paso 1: Detectar y clasificar los ceros

# Se supone que existen ceros falsos pero no son evidentes
# entonces no podemos eliminar ceros falsos

# Eliminamos ceros estructurales evidentes con
# la condición de volumen de agua = 0

# Aunque filtramos estos ceros estructurales
# podrian seguir existiendo condiciones que generen ceros
# estructurales

# Por ejemplo:
# temperaturas muy altas o bajas
# pH extremo, etc
# como conclusion, parte de los ceros podrian ser estructurales

# Paso 2: identificar covariables adecuadas

# Para modelar abundancia, las covariables seleccionadas son:
  
# Season
# Macrohabitat
# Microhabitat
# Temperature
# pH
# log volume
# Grid_no como efecto aleatorio

# Variable respuesta:
# `Total mosquito emerged`

# Paso 3: evaluar sobredispersion e inflacion de ceros

table(datos_mod$`Total.mosquito.emerged` == 0)
prop.table(table(datos_mod$`Total.mosquito.emerged` == 0))

# 71% de ceros a pesar de excluir recipientes sin agua

media <- mean(datos_mod$`Total.mosquito.emerged`)
varianza <- var(datos_mod$`Total.mosquito.emerged`)

indice_dispersion <- varianza / media
indice_dispersion

# Indice de dispersion 13.5 (>>1)
# Existe sobredispersion

# Paso 4: elegir modelos adecuados

# A pesar de excluir los criaderos sin agua, la variable de abundancia presenta
# una elevada proporción de ceros (71%) y una fuerte sobredispersión (d = 13.5).

# Si bien estos ceros pueden explicarse en parte por la variabilidad del proceso,
# no puede descartarse la existencia de condiciones no observadas que limiten
# estructuralmente la presencia de larvas, incluso en criaderos con agua.

# En este contexto, se consideró apropiado evaluar modelos inflados en cero,
# los cuales permiten modelar simultáneamente la ocurrencia de ceros estructurales
# y la abundancia cuando el proceso está activo.

# Por lo tanto, se compararon modelos de binomial negativa (NB) y 
# binomial negativa inflada en ceros (ZINB), con el objetivo de determinar 
# cuál describe mejor los datos.


# Modelo NB

modelo_nb <- glmmTMB(
  `Total.mosquito.emerged` ~  Season + Macrohabitat + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)

# Modelo ZINB

modelo_zinb <- glmmTMB(
  `Total.mosquito.emerged` ~  Season + Macrohabitat + (1 | Grid_no),
  
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

# Paso 5: Comparacion con AIC

AIC(modelo_nb, modelo_zinb)

# La diferencia de AIC (45) es considerablemente mayor a 10, lo que indica una
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

# En el gráfico QQ, los residures_zinb <- simulateResiduals(modelo_zinb)

x11()
plot(res_zinb)os simulados se alinean adecuadamente con la línea
# esperada, lo que sugiere que la distribución asumida por el modelo es
# consistente con la estructura de los datos.

# Los tests estadísticos asociados no resultan significativos
# KS test p = 0.38; test de dispersión p = 0.84; test de outliers p = 0.24,
# indicando ausencia de desviaciones importantes, sobredispersión residual
# o valores atípicos no explicados.

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
# 2) El modelo de inflación de ceros, que explica la probabilidad de ceros extra.

# --------------------------
# Efecto aleatorio
# --------------------------

# La varianza asociada a Grid_no es prácticamente nula.
# Esto sugiere que, una vez consideradas Season y Macrohabitat,
# no queda una variabilidad relevante entre grillas.

# --------------------------
# Modelo condicional: abundancia
# --------------------------

# El intercepto representa la abundancia esperada en escala logarítmica
# para la categoría de referencia de Season y Macrohabitat.

# Seasonfeb-march tiene un efecto positivo y significativo
# (Estimate = 1.03; p < 0.001).
# Esto indica que en feb-march la abundancia esperada de larvas es mayor
# que en la estación de referencia.

# exp(1.03) ≈ 2.8
# Por lo tanto, la abundancia esperada en feb-march es aproximadamente
# 2.8 veces mayor que en la estación de referencia.

# Seasonjuly-september muestra un efecto positivo marginal
# (Estimate = 0.43; p ≈ 0.076).
# Esto sugiere una posible tendencia a mayor abundancia,
# aunque la evidencia no es concluyente.

# Seasonoctober-december no muestra un efecto significativo.
# No se detectan diferencias claras respecto a la estación de referencia.

# En relación con Macrohabitat, la categoría Lake presenta un efecto
# negativo significativo (Estimate = -0.99; p ≈ 0.016).
# Esto indica menor abundancia esperada de larvas respecto al macrohábitat
# de referencia.

# exp(-0.99) ≈ 0.37
# Esto equivale a una abundancia esperada aproximadamente 63% menor.

# La categoría Medium dense también presenta un efecto negativo significativo
# (Estimate = -0.88; p ≈ 0.024), indicando menor abundancia esperada
# respecto al macrohábitat de referencia.

# Las categorías High dense, Low dense y Plantation no presentan efectos
# significativos claros.

# --------------------------
# Modelo de inflación de ceros
# --------------------------

# En el componente de inflación de ceros, ninguna categoría de Season
# resulta significativa al nivel convencional de 0.05.

# Sin embargo, october-december muestra una tendencia marginal
# (Estimate = 0.64; p ≈ 0.063), lo que podría indicar una mayor
# probabilidad de ceros en esa estación.

# Esta tendencia debe interpretarse con cautela, ya que no alcanza
# significancia estadística convencional.

# ==========================
# Conclusión final
# ==========================

# El modelo ZINB fue seleccionado como modelo final porque presentó un mejor
# ajuste que el modelo NB según el criterio AIC y mostró diagnósticos adecuados
# mediante DHARMa.

# Los diagnósticos no evidenciaron problemas importantes de distribución de
# residuos, sobredispersión residual, valores atípicos ni exceso de ceros no
# explicado.

# La abundancia de larvas estuvo principalmente asociada a la estacionalidad
# y al macrohábitat.

# En particular, feb-march presentó mayor abundancia esperada de larvas,
# mientras que los macrohábitats Lake y Medium dense mostraron menor abundancia
# respecto a la categoría de referencia.

# El componente de inflación de ceros permitió capturar adecuadamente la
# proporción de ceros observada en los datos, aunque las variables incluidas
# en este componente no mostraron efectos significativos fuertes.

# En conjunto, el modelo sugiere que la dinámica de abundancia larval está
# estructurada principalmente por factores temporales y por el contexto ambiental
# general del criadero.


# ==========================
# Modelo abundancia Aedes aegypti
# ==========================

modelo_aegypti_nb <- glmmTMB(
  Aedes.aegypti ~ Season + Macrohabitat,
  family = nbinom2,
  data = datos_mod
)

modelo_aegypti_zinb <- glmmTMB(
  Aedes.aegypti ~ Season + Macrohabitat,
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_aegypti_nb, modelo_aegypti_zinb)

# La comparación entre modelos NB y ZINB para Aedes aegypti mostró una diferencia
# de AIC menor a 2 (ΔAIC ≈ 1.5), lo que indica que ambos modelos presentan un
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

# El diagnóstico con DHARMa muestra un buen comportamiento general del modelo.

# En el gráfico QQ, los residuos simulados se alinean adecuadamente con la línea
# esperada, lo que indica que la distribución asumida por el modelo es consistente
# con los datos.

# Los tests asociados no resultan significativos:
# KS test p = 0.849
# test de dispersión p = 0.392
# test de outliers p = 0.16

# Esto indica que no se detectan desviaciones importantes, sobredispersión
# residual ni valores atípicos no explicados por el modelo.

# El gráfico de residuos versus valores predichos no muestra problemas
# significativos. Las curvas de cuantiles se mantienen aproximadamente dentro
# de las bandas de confianza, por lo que no se observan patrones sistemáticos
# relevantes.

# En conjunto, estos resultados sugieren que el modelo NB describe
# adecuadamente la abundancia de Aedes aegypti.

# A diferencia del modelo de abundancia total, en esta especie no se observa
# evidencia clara de que sea necesario un componente adicional de inflación
# de ceros.

summary(modelo_aegypti_nb)

# ==========================
# Interpretación del modelo NB - Aedes aegypti
# ==========================

# El modelo ajustado corresponde a una binomial negativa (NB) sin componente
# de inflación de ceros, seleccionada por parsimonia dado que el modelo ZINB
# no mejoró sustancialmente el ajuste.

# --------------------------
# Intercepto
# --------------------------

# El intercepto (0.27) representa la abundancia esperada (en escala log)
# para la categoría de referencia de Season y Macrohabitat.

# No resulta significativo (p = 0.79), lo que indica alta incertidumbre
# en la estimación del valor base.

# --------------------------
# Season
# --------------------------

# Seasonfeb-march: 1.17 (p = 0.108)
# Muestra un efecto positivo relativamente grande, lo que sugiere una mayor
# abundancia de Aedes aegypti en esta estación respecto a la referencia.
# Sin embargo, el efecto no es estadísticamente significativo.

# exp(1.17) ≈ 3.2
# Esto indicaría aproximadamente 3 veces más abundancia, aunque con alta
# incertidumbre.

# Seasonjuly-september: efecto prácticamente nulo (Estimate ≈ 0, p ≈ 1)
# No se observan diferencias respecto a la categoría de referencia.

# Seasonoctober-december: efecto pequeño y no significativo (p = 0.90)
# Tampoco se detectan diferencias claras.

# --------------------------
# Macrohabitat
# --------------------------

# Ninguna de las categorías de Macrohabitat resulta significativa.

# Lake: -1.63 (p = 0.14)
# Presenta un efecto negativo relativamente grande, sugiriendo menor abundancia,
# pero no significativo.

# exp(-1.63) ≈ 0.20
# Esto implicaría una reducción importante en abundancia, aunque con alta
# incertidumbre.

# Resto de categorías (High dense, Low dense, Medium dense, Plantation)
# muestran efectos cercanos a cero y no significativos.

# --------------------------
# Dispersión
# --------------------------

# El parámetro de dispersión es bajo (0.073), lo que indica que la varianza
# de los datos es relativamente cercana a la media en comparación con el
# modelo de abundancia total.

# ==========================
# Conclusión final
# ==========================

# El modelo NB ajustado para Aedes aegypti no muestra efectos estadísticamente
# significativos de las variables Season ni Macrohabitat sobre la abundancia.

# Si bien se observan tendencias (particularmente un aumento en feb-march y una
# posible menor abundancia en ambientes tipo Lake), la evidencia no es suficiente
# para afirmar relaciones claras entre estas variables y la abundancia de la especie.

# A diferencia del modelo de abundancia total, donde la estacionalidad y el
# macrohábitat resultaron relevantes, en el caso de Aedes aegypti la variabilidad
# observada no puede ser explicada de manera concluyente por las covariables
# consideradas.

# Esto sugiere que la dinámica de abundancia de Aedes aegypti podría estar
# influenciada por otros factores no incluidos en el modelo o presentar una
# mayor variabilidad intrínseca.

# ==========================
# Modelo abundancia Aedes albopictus
# ==========================

modelo_albopictus_nb <- glmmTMB(
  Aedes.albopictus ~ Season + Macrohabitat + logvol_std,
  family = nbinom2,
  data = datos_mod
)

modelo_albopictus_zinb <- glmmTMB(
  Aedes.albopictus ~ Season + Macrohabitat + logvol_std,
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

AIC(modelo_albopictus_nb, modelo_albopictus_zinb)

# ==========================
# Selección de modelo - Aedes albopictus
# ==========================

# La comparación entre modelos NB y ZINB muestra una diferencia de AIC
# considerable (ΔAIC ≈ 15).

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
# Diagnóstico del modelo Aedes albopictus
# ==========================

# La comparación entre modelos NB y ZINB mostró una diferencia de AIC
# considerable (ΔAIC ≈ 15), lo que indica un mejor ajuste del modelo ZINB.

# Esto sugiere la presencia de un proceso adicional que genera ceros,
# justificando la inclusión del componente de inflación de ceros.

# --------------------------
# Diagnóstico con DHARMa
# --------------------------

# En el gráfico QQ, los residuos simulados se alinean adecuadamente con la línea
# esperada, lo que indica que la distribución asumida por el modelo es consistente
# con los datos.

# Los tests asociados no resultan significativos:
# KS test p = 0.45
# test de dispersión p = 0.576
# test de outliers p = 0.7

# Esto indica que no se detectan desviaciones importantes, sobredispersión
# residual ni valores atípicos no explicados.

# El gráfico de residuos vs valores predichos no muestra problemas relevantes.
# Las curvas de cuantiles se mantienen dentro de las bandas de confianza,
# lo que sugiere que el modelo captura adecuadamente la relación entre
# la variable respuesta y las covariables.

# En conjunto, estos resultados indican que el modelo ZINB describe
# adecuadamente la estructura de los datos para Aedes albopictus.

summary(modelo_albopictus_zinb)

# ==========================
# Interpretación del modelo ZINB - Aedes albopictus
# ==========================

# El modelo ajustado corresponde a una binomial negativa con inflación de ceros,
# seleccionada por su mejor ajuste respecto al modelo NB.

# --------------------------
# Intercepto
# --------------------------

# El intercepto (2.30) representa la abundancia esperada en escala logarítmica
# para la categoría de referencia de Season y Macrohabitat, con volumen promedio.

# Es significativo (p < 0.01), indicando que el valor base está bien estimado.

# --------------------------
# Season
# --------------------------

# Ninguna de las categorías de Season resulta significativa en el modelo condicional.

# Esto indica que, a diferencia del modelo de abundancia total,
# la estacionalidad no parece explicar directamente la cantidad de
# Aedes albopictus cuando la especie está presente.

# --------------------------
# Macrohabitat
# --------------------------

# High dense: -1.82 (p = 0.004)
# Efecto negativo significativo, indicando menor abundancia en este tipo de ambiente.

# exp(-1.82) ≈ 0.16
# Esto implica aproximadamente un 84% menos de abundancia respecto a la categoría de referencia.

# Medium dense: efecto negativo marginal (p ≈ 0.10)
# Sugiere una posible reducción en abundancia, aunque no concluyente.

# Resto de categorías: no significativas.

# --------------------------
# Volumen
# --------------------------

# logvol_std: 0.52 (p = 0.0016)
# Efecto positivo y significativo.

# Esto indica que a mayor volumen de agua, mayor abundancia de Aedes albopictus.

# exp(0.52) ≈ 1.68
# Aproximadamente un 68% más de abundancia por unidad estandarizada de volumen.

# --------------------------
# Modelo de inflación de ceros
# --------------------------

# Intercepto significativo (p < 0.001), lo que indica una probabilidad base
# de ceros relativamente alta.

# Seasonjuly-september: -1.48 (p = 0.022)
# Efecto negativo significativo.

# Esto indica que en esta estación disminuye la probabilidad de ceros,
# es decir, es más probable encontrar la especie.

# exp(-1.48) ≈ 0.23
# Aproximadamente un 77% menos probabilidad de ausencia.

# Resto de categorías: no significativas.


# ==========================
# Conclusión final - Aedes albopictus
# ==========================

# El modelo ZINB seleccionado describe adecuadamente la abundancia de
# Aedes albopictus, capturando tanto la variabilidad en los conteos como
# la presencia de ceros adicionales.

# A diferencia de Aedes aegypti, en esta especie se detecta un patrón claro
# de inflación de ceros, lo que indica una mayor intermitencia en su presencia
# en los criaderos.

# La abundancia de Aedes albopictus se asocia principalmente con el volumen
# de agua disponible y con el tipo de macrohábitat, mostrando menor abundancia
# en ambientes de alta densidad.

# Por otro lado, la estacionalidad no afecta significativamente la abundancia,
# pero sí influye en la probabilidad de presencia, particularmente en la
# estación july-september, donde la especie es más frecuente.

# En conjunto, estos resultados sugieren que Aedes albopictus responde a
# condiciones estructurales del ambiente (como el volumen de agua), y presenta
# una dinámica más discontinua, alternando entre presencia y ausencia según
# las condiciones.


