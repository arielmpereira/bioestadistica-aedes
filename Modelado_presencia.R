# ==========================================
# TP2 Bioestadística - Modelado Presencia
# ==========================================


# Justificación del modelo de presencia

# La variable respuesta es presencia/ausencia, por lo que no se puede usar 
# un modelo lineal clásico. En su lugar, se utiliza un modelo logístico.

# Además, las observaciones no son independientes, ya que están agrupadas 
# por grilla y por visita (grilla-estación).

# Por este motivo, se utiliza un modelo mixto (GLMM), que permite incorporar 
# esta estructura mediante efectos aleatorios.

# En este caso, se modela la dependencia a nivel de cada visita (grilla-estación),
# que es donde se observa mayor variabilidad en los datos.


library(readxl)
library(dplyr)
library(lme4)
library(DHARMa)

# Cargar base
datos <- suppressWarnings(
  read_excel("aedes_data.xlsx")
)
dim(datos)


# ==========================
# 1. Preparación de datos
# ==========================

# Filtramos igual que en el AE:
# - solo grillas Index
# - solo criaderos activos, es decir, con volumen de agua > 0



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


# =========================================
# Modelo de prevalencia (logístico)
# =========================================

modelo1_presencia <- glmer(
  Prevalence ~ temp_std + pH_std + logvol_std + Season + (1 | Grid_no),
  data = datos_mod,
  family = binomial
)

modelo2_presencia <- glmer(
  Prevalence ~ temp_std + pH_std + logvol_std + Season + (1 | Grid_no:Season),
  data = datos_mod,
  family = binomial
)

AIC(modelo1_presencia, modelo2_presencia)

# Se comparan dos estructuras de efectos aleatorios.
# El modelo 1 considera dependencia general por grilla.
# El modelo 2 considera dependencia por visita, es decir, por cada combinación
# grilla-estación.

#  906 - 875 = 30 : mejora importante del modelo 2


summary(modelo1_presencia)
summary(modelo2_presencia)


# ===============================================================
# Interpretacion del modelo 2
# ===============================================================

# Random effects

# Std.Dev = 1.178
# Existe una alta variabilidad entre las visitas (grilla-estación).

# Esto indica que la presencia no depende solo de las variables ambientales,
# sino también del momento y lugar en que se realiza el muestreo (visita).

# Es decir, dos mediciones dentro de la misma grilla pero en distintas estaciones
# pueden comportarse de manera diferente.

# Fixed effects

# temp_std: -0.505 (p < 0.001)
# La temperatura tiene un efecto negativo significativo.

# Esto indica que, a medida que aumenta la temperatura, la probabilidad
# de presencia tiende a disminuir, manteniendo constantes las demás variables.

# Calculo de odds:
# exp(-0.505) ≈ 0.60 → las odds de presencia disminuyen aproximadamente un 40%.


# logvol_std: +0.430 (p < 0.001)
# El volumen de agua tiene un efecto positivo significativo.

# lo que indica que criaderos con mayor volumen tienden a tener mayor
# probabilidad de presencia.

# Calculo de odds:
# exp(0.430) ≈ 1.54 → las odds aumentan aproximadamente un 54%.


# pH_std: -0.200 (p = 0.06)
# No hay evidencia fuerte de que el pH influya en la presencia,
# aunque se observa una leve tendencia negativa.


# Season:

# april-june:
# referencia

# feb-march: -0.45 (p = 0.31)
# No se detectan diferencias claras respecto a la estación de referencia.

# july-september: 0.48 (p = 0.25)
# Tampoco se observan diferencias claras, aunque hay una leve tendencia positiva.

# october-december: -2.02 (p < 0.001)
# Probabilidad de presencia significativamente menor respecto
# a la estación de referencia.

# Calculo de odds:
# exp(-2.02) ≈ 0.13
# Esto significa que las odds de presencia son aproximadamente 87% menores
# que en la estación de referencia (april-june).


# Intercepto: -1.50 (p < 0.001)
# El intercepto representa la log-odds de presencia para la situación de referencia:
# april-june, con temperatura, pH y log volume en su valor promedio.

# En términos de odds:
# exp(-1.50) ≈ 0.22

# Esto indica que, en la condición de referencia,
# la probabilidad de presencia es baja.


# ===============================================================
# Comparación de modelos de presencia
# ===============================================================

#                    Modelo 1 (Grid)        Modelo 2 (Grid:Season)
# AIC                    906.7                    875.6
#
# Random effect
# Std.Dev                0.70                     1.18
#
# temp_std         -0.316 (p = 0.009)      -0.505 (p = 0.001)
# logvol_std        0.424 (p < 0.001)       0.430 (p < 0.001)
# pH_std           -0.156 (p = 0.089)      -0.200 (p = 0.062)
#
# Season (oct-dec) -1.72 (p < 0.001)       -2.02 (p < 0.001)




# ===============================================================
# Grafico de residuos (Dharma)
# ===============================================================

res <- simulateResiduals(modelo2_presencia)

x11()
plot(res)

# Se analizaron los residuos simulados con DHARMa.

# No se observan desviaciones importantes ni patrones sistemáticos,
# lo que sugiere un buen ajuste del modelo.

# ==========================================================
# Interpretación del modelo GLMM
# ==========================================================

# El modelo muestra que la presencia depende tanto de variables ambientales 
# como de la unidad de muestreo (visita: grilla-estación).

# El efecto aleatorio de la grilla indica que no todas las grillas se comportan igual. 
# Es decir, hay diferencias entre lugares que no están explicadas solo por las
# variables que medimos.

# En cuanto a los efectos fijos:
  
# - La temperatura tiene un efecto negativo significativo (p < 0.01). 
# En estos datos, a mayor temperatura, la probabilidad de presencia tiende a bajar.

# - El volumen de agua (log volume) tiene un efecto positivo significativo (p < 0.001), 
# lo que indica que criaderos con más agua tienen mayor probabilidad de presencia.

# - El pH no muestra un efecto claro (p ≈ 0.09), por lo que no hay evidencia
# fuerte de que influya.

# - En cuanto a la estación, octubre–diciembre presenta una menor probabilidad
# de presencia respecto a la estación de referencia, mientras que las otras 
# estaciones no muestran diferencias claras.

# En resumen, la presencia no depende solo de la temperatura o el volumen, sino 
# también del lugar (grilla), lo que justifica el uso de un modelo mixto.

