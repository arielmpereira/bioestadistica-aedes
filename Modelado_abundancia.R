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
  `Total.mosquito.emerged` ~ temp_std + logvol_std + Season + (1 | Grid_no),
  family = nbinom2,
  data = datos_mod
)

# Modelo ZINB

modelo_zinb <- glmmTMB(
  `Total.mosquito.emerged` ~ temp_std + logvol_std + Season + (1 | Grid_no),
  
  ziformula = ~ Season,
  family = nbinom2,
  data = datos_mod
)

# Paso 5: Comparacion con AIC

AIC(modelo_nb, modelo_zinb)

res_nb <- simulateResiduals(modelo_nb)
res_zinb <- simulateResiduals(modelo_zinb)

x11()
testZeroInflation(res_nb)
testZeroInflation(res_zinb)


# Diagnostico con DHARMa


# NB
res_nb <- simulateResiduals(modelo_nb)
plot(res_nb)

# ZINB
res_zinb <- simulateResiduals(modelo_zinb)
plot(res_zinb)

testZeroInflation(res_nb)
testZeroInflation(res_zinb)

testDispersion(res_nb)
testDispersion(res_zinb)

# Comparación de modelos

# Se compararon modelos de binomial negativa (NB) y binomial negativa inflada en
# ceros (ZINB) con distintas especificaciones para el componente de inflación de ceros.

# El modelo ZINB que incorpora la variable Season en el componente de inflación
# de ceros presentó un mejor ajuste (AIC = 1835.5) en comparación con el modelo NB
# (AIC = 1880.9), con una diferencia ΔAIC = 45.

# A diferencia de modelos más complejos, esta especificación permite capturar la
# variación en la probabilidad de ceros de manera parsimoniosa, evitando
# sobreajustes en el componente estructural.

# Estos resultados sugieren que la ocurrencia de ceros en la abundancia de larvas
# está asociada a la estacionalidad, lo que refleja la influencia de condiciones
# ambientales sobre la presencia del fenómeno.

# En consecuencia, se selecciona este modelo ZINB como el más adecuado para
# describir la estructura de los datos.


summary(modelo_zinb)

# Interpretacion de parametros

El modelo ZINB permite distinguir entre dos procesos: la ocurrencia de ceros y la abundancia de larvas cuando estas están presentes.

En el componente de inflación de ceros, la variable Season resultó relevante, indicando que la probabilidad de ausencia de larvas depende de la estacionalidad. En particular, la estación october–december presentó una mayor probabilidad de ceros, sugiriendo condiciones desfavorables para la presencia del fenómeno.

En el componente de conteo, la abundancia de larvas se asoció negativamente con la temperatura, y mostró un aumento significativo durante la estación feb–march. Esto sugiere que, una vez que el sistema permite la presencia de larvas, su abundancia responde a condiciones ambientales específicas.

En conjunto, estos resultados indican que la dinámica del sistema está determinada tanto por procesos que regulan la ocurrencia del fenómeno como por factores que afectan su magnitud.
