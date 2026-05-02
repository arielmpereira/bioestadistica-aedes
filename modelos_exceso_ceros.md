# Comprendiendo ceros falsos, estructurales y aleatorios en ecología

Resumen del paper:
What does a zero mean? Understanding false, random and structural zeros in ecology
Anabel Blasco‐Moreno | Marta Pérez‐Casany | Pedro Puig | Maria Morante | Eva Castells

## Resumen:

* **Los ceros generan dos problemas típicos:**

  * **Overdispersion** → la variabilidad es mayor de lo esperado.
  * **Zero inflation** → hay más ceros de los que el modelo “espera”.

* **Hay 3 tipos de ceros:**

  * **Falsos** → errores de muestreo o medición (ej: no detectaste algo que sí estaba).
  * **Estructurales** → el evento no puede ocurrir por cómo funciona el sistema (ej: especie que no interactúa).
  * **Aleatorios** → el evento podía ocurrir, pero no ocurrió por azar.

***

## 1. Introducción

Los datos de conteo son muy comunes en ecología y suelen presentar dos características principales:

* Una varianza mayor que la media (sobredispersión)
* Un exceso de ceros respecto de lo esperado bajo una distribución de conteo

Estas propiedades pueden conducir a una sobreestimación de los parámetros y a inferencias ecológicas poco confiables.

Un valor cero en el dataset puede tener distintos significados, por lo que su interpretación resulta fundamental para la elección del modelo.

**Clasificación de ceros:**

* **Ceros falsos:** se deben a errores de observación o del diseño experimental. Siempre que sea posible, deben ser identificados y removidos durante el análisis exploratorio (AE).

* **Ceros verdaderos:** responden a la naturaleza del proceso y pueden clasificarse en:

  * **Ceros estructurales:** asociados a restricciones ecológicas o evolutivas que impiden que el evento ocurra.
  * **Ceros aleatorios:** producto de la variabilidad del muestreo; el evento podría ocurrir, pero no se observa en ese caso particular.

En la práctica, distinguir entre ceros falsos y estructurales puede resultar difícil.

***

**Formas de tratar con los ceros:**

* **Transformación logarítmica de la variable respuesta:** puede reducir la sobredispersión, pero no garantiza normalidad ni homocedasticidad.
* **Modelos GLM con distribución Poisson:** asumen igualdad entre media y varianza, por lo que no permiten modelar sobredispersión.
* **Distribución binomial negativa (NB):** modela la varianza como una función de la media, permitiendo capturar sobredispersión y un exceso moderado de ceros.

Una forma de evaluar la sobredispersión respecto a Poisson es mediante el índice de dispersión:

d=E(Y)Var(Y)​

**Interpretación del índice de dispersión:**

* Si **d > 1**, existe sobredispersión.

Este criterio también permite diferenciar entre variantes de la distribución binomial negativa:

* **NB tipo 1:** la varianza crece proporcionalmente a la media.
* **NB tipo 2:** la varianza crece de forma cuadrática con la media.

En la práctica, cuando la dispersión varía entre grupos, suele ser más adecuado utilizar NB tipo 2.


Cuando la distribución binomial negativa no logra explicar el exceso de ceros, se presentan dos escenarios posibles:

1. **No existe una explicación biológica:**\
   Todos los ceros son aleatorios. En este caso, se pueden utilizar distribuciones que asignan mayor probabilidad a valores cero.
2. **Existe una explicación biológica:**\
   Algunos ceros son estructurales. Por ejemplo, una especie puede no estar presente en determinados hábitats debido a restricciones ecológicas. En este caso, los ceros forman parte del sistema y no son producto del azar.

En este segundo escenario, se utilizan modelos inflados en ceros (Zero-Inflated).

Estos modelos asumen la existencia de dos procesos generadores de datos:

1. Un proceso estructural, en el cual el evento no puede ocurrir.
2. Un proceso de conteo, que genera valores positivos y ceros aleatorios.

***

### Modelos para exceso de ceros

**Modelos Zero-Inflated (ZI):**

* ZIP (Poisson inflado en ceros)
* ZINB (Binomial negativa inflada en ceros)

Estos modelos combinan dos procesos:

1. Un proceso que genera ceros estructurales
2. Un proceso de conteo que incluye ceros aleatorios y valores positivos

Por lo tanto, su uso solo está justificado cuando existe evidencia de ceros estructurales.


**Modelos Hurdle (o Zero-Altered):**

Separan el proceso en dos componentes:

* Un modelo para la ocurrencia de ceros## Introducción

Datos de conteo son muy comunes en ecologia y poseen dos caracteristicas:

* Una varianza mas grande que la media (sobredispersion)

* Un exceso de ceros comparado con la cantidad esperada por una distribución de conteo.

Esto produce sobre estimacion de parametros e inferencia ecologica pobre.

Un cero en el dataset puede tener diferentes significados:

**Clasificación de ceros:**

* **Ceros falsos:** los fasos ceros se deben a errores de observación o del diseño experimental. Estos ceros deben ser removidos durante el AE cuando es posible.
  
* **Ceros verdaderos:** se deben a la naturaleza del proceso. Se pueden clasificar de dos formas: ceros estructurales y ceros aleatorios.

    * **Ceros estructurales:** estan asociados a restricciones ecologicas o evolutivas.
  
* Un modelo para los valores positivos

Estos modelos son adecuados cuando el proceso de conteo no puede generar ceros y, a diferencia de los modelos ZI, no distinguen entre tipos de ceros.


### Detección de exceso de ceros

El exceso de ceros puede evaluarse mediante un índice de zero inflation:

* Si el índice es mayor que cero, hay más ceros de los esperados.

Este análisis puede realizarse respecto a distribuciones de referencia como:

* Poisson
* Binomial negativa

Lo que permite orientar la elección de modelos inflados en ceros.

***

## 2. Teoría de los modelos inflados en cero

Los modelos inflados en cero asumen que los datos provienen de una mezcla de dos procesos:

1. Un proceso que genera **ceros estructurales**.
2. Un proceso de conteo que genera valores positivos y también puede generar **ceros aleatorios**.

La forma general del modelo es:

f(y)=ωf0​+(1−ω)fR​(y)

Donde:

* ω es la probabilidad de que la observación venga del proceso estructural.
* f0​ es la distribución degenerada en cero.
* fR​(y) es la distribución de conteo de referencia: Poisson o binomial negativa.
* 1−ω es la probabilidad de que la observación venga del proceso de conteo.


### Caso cero

Para Y=0, la probabilidad combina dos fuentes:

P(Y=0)=ω+(1−ω)fR​(0)

Esto significa:

* una parte de los ceros viene del proceso estructural;
* otra parte viene del proceso de conteo.


### Caso positivo

Para Y>0, la probabilidad solo viene del proceso de conteo:

P(Y=y)=(1−ω)fR​(y)

Esto significa que los valores positivos no pueden venir del proceso estructural.


### ZIP: Poisson inflado en cero

Si la distribución de conteo es Poisson, el modelo se llama **ZIP**.

Y∼ZIP(ω,λ)

Donde:

* ω representa la proporción de ceros estructurales.
* λ representa la media del proceso de conteo.


### ZINB: Binomial negativa inflada en cero

Si la distribución de conteo es binomial negativa, el modelo se llama **ZINB**.

Y∼ZINB(ω,λ,α)

Donde:

* ω representa los ceros estructurales.
* λ representa la media del proceso de conteo.
* α representa la sobredispersión.


### Incorporación de covariables

El paper señala que los parámetros del modelo pueden depender de covariables.

Para la parte de conteo se usa normalmente un enlace log:

λi​=exp(β0​+β1​X1i​+⋯+βr​Xri​)

Esto modela el valor esperado del conteo.

Para la parte estructural se usa un enlace logístico:

ωi​=1+exp(γ0​+γ1​Z1i​+⋯+γk​Zki​)exp(γ0​+γ1​Z1i​+⋯+γk​Zki​)​

Esto modela la probabilidad de que la observación sea un cero estructural.


### Score tests

El paper explica que los índices de dispersión y zero inflation sirven para mirar los datos en bruto, pero no alcanzan cuando ya hay covariables en el modelo.

Por eso propone **score tests**, que permiten evaluar si hace falta un modelo más complejo sin tener que ajustar primero todos los modelos complejos.

Se usan para comparar:

* Poisson vs NB: detecta sobredispersión.
* Poisson vs ZIP: detecta exceso de ceros respecto a Poisson.
* ZIP vs ZINB: detecta si además de ceros estructurales hay sobredispersión que requiere NB.

Idea simple:

> El score test pregunta si un modelo simple alcanza o si hay evidencia estadística para pasar a uno más complejo.


### Goodness-of-fit tests

Después de ajustar modelos, el paper propone comparar qué tan bien ajustan.

Para modelos anidados se puede usar:

* Likelihood Ratio Test

Ejemplo:

* Poisson vs NB
* ZIP vs ZINB

Para modelos no anidados o con problemas de borde se usan criterios de información:

* AIC
* BIC

El modelo preferible es el que tiene menor AIC o BIC, aunque estos criterios no son una prueba estadística directa.

También menciona:

* Test de Vuong
* Test de Clarke

Pero advierte que el test de Vuong puede ser problemático para evaluar zero inflation, porque el valor nulo está en el borde del espacio de parámetros.


***

## 3. Cómo analizar datos de conteo

### **Paso 1: Detectar y clasificar los ceros**

Primero se debe evaluar si existe un exceso de ceros en los datos.

Esto implica:

* Comparar la cantidad de ceros observados con la esperada bajo una distribución de referencia (Poisson o NB)
* Determinar si el exceso de ceros es relevante

Una vez detectado, los ceros deben **clasificarse** según su origen:

* **Ceros falsos** (errores de muestreo o medición): Eliminarlos si es posible.
* **Ceros estructurales** (el evento no puede ocurrir): 
* **Ceros aleatorios** (el evento podría ocurrir pero no ocurrió)

Este paso combina análisis estadístico y conocimiento del sistema.


### **Paso 2: Seleccionar las variables explicativas**

Se deben identificar las covariables que pueden explicar:

* La ocurrencia del evento (cero vs no cero)
* La magnitud del conteo

En modelos inflados en ceros, las covariables pueden ser distintas para:

* El componente estructural (probabilidad de ceros)
* El componente de conteo


### **Paso 3: Detectar sobredispersión e inflación de ceros**

Se evalúan dos propiedades de los datos:

**Sobredispersión**

d = Var(Y)​ / E(Y)

* Si d>1, hay sobredispersión, indica que Poisson no es adecuado


#### Exceso de ceros

Se compara la proporción de ceros observados con la esperada bajo Poisson o NB.

* Si hay más ceros de lo esperado → hay zero inflation

Este paso permite decidir si es necesario considerar modelos más complejos.


### **Paso 4: Elegir el modelo adecuado**

En función de los pasos anteriores:

* **Poisson:** sin sobredispersión ni exceso de ceros
* **NB:** con sobredispersión
* **ZIP / ZINB:** con exceso de ceros y presencia de ceros estructurales
* **Otras distribuciones:** exceso de ceros sin explicación estructural

La elección del modelo debe ser coherente con el sistema biológico.


### **Paso 5: Evaluar el ajuste del modelo**

Se comparan modelos utilizando:

* AIC / BIC
* Likelihood ratio tests (para modelos anidados)

También se pueden usar:

* Test de Vuong (con precaución)



