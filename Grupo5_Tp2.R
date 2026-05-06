# ============================================================
# Grupo 5 - TP2 Bioestadistica / LAyGD
# Analisis exploratorio inicial basado en Zuur et al. (2010)
# Articulo: Dharmamuthuraja et al. (2023), PLOS NTD
# Base: journal.pntd.0011702.s011.xlsx
# ============================================================
#
# Idea general del script:
# 1) Primero se controla la base completa, sin filtrar recipientes secos.
#    Esto permite entender cuantos ceros vienen de habitats sin agua.
# 2) Despues se reconstruye una unidad de analisis a nivel criadero.
#    No usamos Sl.no solo, porque se repite a lo largo de fechas/grillas.
# 3) Luego se sacan los recipientes sin agua y se vuelve a contar ceros.
#    Este es el punto pedido por el docente para justificar modelos cero-inflados.
# 4) Finalmente se comparan Poisson, binomial negativo, ZIP y ZINB
#    para tres respuestas: total de mosquitos, Ae. aegypti y Ae. albopictus.
# 5) Como avance adicional, se exploran relaciones no lineales con splines.

# ------------------------------------------------------------
# 1. Carga de paquetes
# ------------------------------------------------------------
# Si falta algun paquete, instalar antes de correr:
# install.packages(c("readxl", "dplyr", "tidyr", "ggplot2",
#                    "janitor", "skimr", "stringr", "forcats", "pscl"))

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(skimr)
library(stringr)
library(forcats)

# ------------------------------------------------------------
# 2. Carga de la base Excel
# ------------------------------------------------------------
# El archivo xlsx debe estar en la misma carpeta que este script.

archivo_base <- "journal.pntd.0011702.s011.xlsx"

hojas_excel <- excel_sheets(archivo_base)
cat("Hojas disponibles en el archivo Excel:\n")
print(hojas_excel)

hoja_principal <- hojas_excel[1]

# Se lee como texto para evitar que readxl convierta algunas fechas y deje
# otras como NA cuando la columna mezcla seriales de Excel y fechas "dd-mm-aaaa".
datos_raw <- read_excel(archivo_base, sheet = hoja_principal, col_types = "text")

datos <- datos_raw %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ str_squish(.x))) %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))

cat("\nBase leida desde la hoja:", hoja_principal, "\n")

# ------------------------------------------------------------
# 3. Revision general de la base
# ------------------------------------------------------------
# Siguiendo la logica de Zuur, antes de modelar hay que conocer la
# estructura de la base, las variables disponibles y los tipos de datos.

cat("\nDimensiones de la base original:\n")
print(dim(datos))

cat("\nNombres de variables luego de clean_names():\n")
print(names(datos))

cat("\nEstructura general:\n")
str(datos)

cat("\nVista con glimpse():\n")
glimpse(datos)

cat("\nResumen con skim():\n")
print(skim(datos))

# ------------------------------------------------------------
# 4. Preparacion minima de variables numericas y fecha
# ------------------------------------------------------------
# No se eliminan variables originales. Se crean versiones numericas para
# revisar volumen, temperatura y pH sin perder la informacion cruda.

extraer_numero <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, ",", "")
  as.numeric(str_extract(x, "-?[0-9]+\\.?[0-9]*"))
}

parsear_fecha_excel <- function(x) {
  x <- str_squish(as.character(x))
  x[x == ""] <- NA_character_
  salida <- rep(as.Date(NA), length(x))

  es_serial <- str_detect(x, "^[0-9]+\\.?[0-9]*$")
  salida[es_serial & !is.na(x)] <- as.Date(
    as.numeric(x[es_serial & !is.na(x)]),
    origin = "1899-12-30"
  )

  formatos <- c("%d-%m-%Y", "%d/%m/%Y", "%d-%m-%y", "%d/%m/%y", "%Y-%m-%d", "%m/%d/%Y")
  for (formato in formatos) {
    falta <- is.na(salida) & !is.na(x)
    salida[falta] <- as.Date(x[falta], format = formato)
  }

  salida
}

if ("collection_date" %in% names(datos)) {
  datos <- datos %>%
    mutate(collection_date = parsear_fecha_excel(collection_date))
}

datos <- datos %>%
  mutate(
    sl_no = extraer_numero(sl_no),
    grid_no = extraer_numero(grid_no),
    latititude = extraer_numero(latititude),
    longitude = extraer_numero(longitude),
    prevalence_of_wet_dry_habitats = extraer_numero(prevalence_of_wet_dry_habitats),
    capacity_num = extraer_numero(capacity_of_habitat_ml),
    actual_water_num = extraer_numero(actual_level_of_water_ml),
    final_volume_num = extraer_numero(final_volume_water_ml_without_cm),
    log_volume_num = extraer_numero(log_volume),
    temperature_num = extraer_numero(temperature),
    ph_num = extraer_numero(p_h)
  )

vars_conteo_originales <- c(
  "prevalence", "male", "female", "total_mosquito_emerged",
  "aedes_aegypti", "aedes_albopictus", "aedes_vitatus",
  "anopheles_culicifacies", "anopheles_sinensis", "anopheles_stephensi",
  "anopheles_vagus", "anopheles_varuna",
  "culex_bitaeniorhynchus", "culex_gelidus", "culex_mimeticus",
  "culex_quinquefasciatus", "culex_tritaeniorhynchus",
  "tripteroides_affinis", "tripteroides_aranoides",
  "armigeres_subalbatus"
)

datos <- datos %>%
  mutate(across(any_of(vars_conteo_originales), ~ suppressWarnings(as.numeric(.x))))

datos <- datos %>%
  mutate(
    immatures_bin = case_when(
      str_to_lower(immatures) %in% c("present", "presence", "yes", "y", "1") ~ 1,
      str_to_lower(immatures) %in% c("absent", "absence", "no", "n", "0") ~ 0,
      TRUE ~ suppressWarnings(as.numeric(immatures))
    )
  )

# ------------------------------------------------------------
# 5. Revision de unidad de analisis y posibles duplicados
# ------------------------------------------------------------
# La base puede mezclar dos niveles: el recipiente/criadero inspeccionado
# y la especie registrada dentro de ese recipiente. Por eso revisamos
# duplicados usando claves biologicamente razonables.
#
# Punto importante para la reunion:
# Sl.no NO se toma como identificador unico del criadero. En la planilla,
# Sl.no se repite y no alcanza para separar fecha, grilla, coordenadas y
# numero de contenedor. Por eso armamos una clave compuesta.

id_criadero <- c(
  "season", "grid_type", "grid_no", "collection_date",
  "latititude", "longitude", "container_number"
)
id_criadero <- id_criadero[id_criadero %in% names(datos)]

comparacion_identificadores <- tibble(
  indicador = c(
    "filas_originales",
    "sl_no_unicos",
    "criaderos_clave_compuesta"
  ),
  n = c(
    nrow(datos),
    n_distinct(datos$sl_no),
    n_distinct(datos %>% select(all_of(id_criadero)))
  )
)

cat("\nComparacion de posibles identificadores de criadero:\n")
print(comparacion_identificadores)

duplicados_criadero <- datos %>%
  count(across(all_of(id_criadero)), name = "n_filas") %>%
  filter(n_filas > 1) %>%
  arrange(desc(n_filas))

cat("\nPosibles recipientes repetidos en la base original:\n")
print(duplicados_criadero)

cat("\nInterpretacion: si un mismo recipiente aparece mas de una vez, no necesariamente es un error.\n")
cat("Puede indicar que el registro esta expandido por especie o por conteos asociados al mismo sitio.\n")

clave_registro_especie <- c(
  "collection_date", "latititude", "longitude", "breeding_habitat",
  "microhabitat", "container_number", "species"
)
clave_registro_especie <- clave_registro_especie[clave_registro_especie %in% names(datos)]

duplicados_registro_especie <- datos %>%
  count(across(all_of(clave_registro_especie)), name = "n_filas") %>%
  filter(n_filas > 1) %>%
  arrange(desc(n_filas))

cat("\nRegistros repetidos por fecha/coordenadas/criadero/microhabitat/especie:\n")
print(duplicados_registro_especie)

duplicados_exactos <- datos %>%
  count(across(everything()), name = "n_filas") %>%
  filter(n_filas > 1) %>%
  arrange(desc(n_filas))

cat("\nFilas exactamente duplicadas:\n")
print(duplicados_exactos)

# ------------------------------------------------------------
# 6. Valores faltantes
# ------------------------------------------------------------
# Revisamos la cantidad y el porcentaje de NA por variable. Tambien miramos
# si volumen, temperatura o pH tienen NA o ceros asociados a habitats secos.

na_resumen <- datos %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "cantidad_na") %>%
  mutate(porcentaje_na = 100 * cantidad_na / nrow(datos)) %>%
  arrange(desc(porcentaje_na))

cat("\nResumen de valores faltantes:\n")
print(na_resumen)

variables_con_muchos_na <- na_resumen %>%
  filter(porcentaje_na >= 20)

cat("\nVariables con 20% o mas de valores faltantes:\n")
print(variables_con_muchos_na)

agua_por_condicion <- datos %>%
  group_by(wet) %>%
  summarise(
    n = n(),
    temperatura_na = sum(is.na(temperature_num)),
    temperatura_cero = sum(temperature_num == 0, na.rm = TRUE),
    ph_na = sum(is.na(ph_num)),
    ph_cero = sum(ph_num == 0, na.rm = TRUE),
    volumen_na = sum(is.na(final_volume_num)),
    volumen_cero = sum(final_volume_num == 0, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nCeros y NA en variables de agua segun condicion humedo/seco:\n")
print(agua_por_condicion)

# ------------------------------------------------------------
# 7. Variables categoricas
# ------------------------------------------------------------

frecuencia_var <- function(base, variable) {
  if (!variable %in% names(base)) {
    cat("\nVariable no encontrada:", variable, "\n")
    return(invisible(NULL))
  }

  salida <- base %>%
    count(.data[[variable]], name = "n") %>%
    mutate(porcentaje = 100 * n / sum(n)) %>%
    arrange(desc(n))

  cat("\nFrecuencia de", variable, ":\n")
  print(salida)
}

vars_categoricas <- c(
  "season", "grid_type", "macrohabitat", "microhabitat",
  "breeding_habitat_type", "wet", "species"
)

invisible(lapply(vars_categoricas, frecuencia_var, base = datos))

# ------------------------------------------------------------
# 8. Variables numericas
# ------------------------------------------------------------

resumir_numericas <- function(base, variables) {
  variables <- variables[variables %in% names(base)]

  base %>%
    summarise(
      across(
        all_of(variables),
        list(
          n = ~ sum(!is.na(.x)),
          media = ~ ifelse(all(is.na(.x)), NA_real_, mean(.x, na.rm = TRUE)),
          mediana = ~ ifelse(all(is.na(.x)), NA_real_, median(.x, na.rm = TRUE)),
          varianza = ~ ifelse(sum(!is.na(.x)) > 1, var(.x, na.rm = TRUE), NA_real_),
          minimo = ~ ifelse(all(is.na(.x)), NA_real_, min(.x, na.rm = TRUE)),
          maximo = ~ ifelse(all(is.na(.x)), NA_real_, max(.x, na.rm = TRUE))
        ),
        .names = "{.col}__{.fn}"
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("variable", ".value"),
      names_sep = "__"
    )
}

vars_numericas <- c(
  "capacity_num", "actual_water_num", "final_volume_num",
  "temperature_num", "ph_num", "prevalence", "immatures_bin",
  "total_mosquito_emerged", "aedes_aegypti", "aedes_albopictus"
)

resumen_numericas <- resumir_numericas(datos, vars_numericas)

cat("\nResumen de variables numericas principales:\n")
print(resumen_numericas)

# ------------------------------------------------------------
# 9. Base agregada a nivel criadero/recipiente
# ------------------------------------------------------------
# Para presencia/ausencia larval y abundancia total, la unidad mas clara
# suele ser el criadero/recipiente. Sumamos conteos por especie dentro de
# cada recipiente y conservamos las caracteristicas ambientales.

primer_no_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  x[1]
}

max_no_na <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

sumar_conteo <- function(x) {
  sum(x, na.rm = TRUE)
}

datos_criadero <- datos %>%
  group_by(across(all_of(id_criadero))) %>%
  summarise(
    macrohabitat = primer_no_na(macrohabitat),
    month = primer_no_na(month),
    breeding_habitat = primer_no_na(breeding_habitat),
    microhabitat = primer_no_na(microhabitat),
    plant_species = primer_no_na(plant_species),
    breeding_habitat_type = primer_no_na(breeding_habitat_type),
    wet = primer_no_na(wet),
    prevalence_wet_dry = max_no_na(prevalence_of_wet_dry_habitats),
    capacity_num = primer_no_na(capacity_num),
    actual_water_num = primer_no_na(actual_water_num),
    final_volume_num = primer_no_na(final_volume_num),
    log_volume_num = primer_no_na(log_volume_num),
    temperature_num = primer_no_na(temperature_num),
    ph_num = primer_no_na(ph_num),
    prevalence = max_no_na(prevalence),
    immatures_presencia = max_no_na(immatures_bin),
    total_mosquito_emerged = sumar_conteo(total_mosquito_emerged),
    aedes_aegypti = sumar_conteo(aedes_aegypti),
    aedes_albopictus = sumar_conteo(aedes_albopictus),
    aedes_vitatus = sumar_conteo(aedes_vitatus),
    male = sumar_conteo(male),
    female = sumar_conteo(female),
    especies_detectadas = paste(
      sort(unique(na.omit(species[species != "Absent"]))),
      collapse = "; "
    ),
    n_filas_originales = n(),
    .groups = "drop"
  ) %>%
  mutate(
    aedes_total = aedes_aegypti + aedes_albopictus + aedes_vitatus,
    aedes_presencia = if_else(aedes_total > 0, 1, 0),
    mosquito_presencia = if_else(total_mosquito_emerged > 0, 1, 0),
    wet_bin = if_else(wet == "Y", 1, 0, missing = NA_real_),
    artificial_bin = if_else(breeding_habitat_type == "Artificial", 1, 0, missing = NA_real_)
  )

cat("\nDimension de la base original:\n")
print(dim(datos))

cat("\nDimension de la base agregada a nivel criadero:\n")
print(dim(datos_criadero))

# ------------------------------------------------------------
# 10. Chequeos de consistencia biologica
# ------------------------------------------------------------
# Estos controles ayudan a interpretar los ceros. Un cero en un habitat seco
# no significa lo mismo que un cero en un recipiente humedo y disponible.

cat("\nHabitats secos con presencia larval:\n")
print(datos_criadero %>% filter(wet == "N", prevalence == 1))

cat("\nHabitats humedos con temperatura, pH o volumen igual a cero:\n")
print(
  datos_criadero %>%
    filter(
      wet == "Y",
      temperature_num == 0 | ph_num == 0 | final_volume_num == 0
    )
)

cat("\nHabitats secos con temperatura, pH o volumen positivo:\n")
print(
  datos_criadero %>%
    filter(
      wet == "N",
      temperature_num > 0 | ph_num > 0 | final_volume_num > 0
    )
)

cat("\nPresencia larval pero sin mosquitos emergidos:\n")
print(datos_criadero %>% filter(prevalence == 1, total_mosquito_emerged == 0))

cat("\nMosquitos emergidos pero prevalence = 0:\n")
print(datos_criadero %>% filter(prevalence == 0, total_mosquito_emerged > 0))

# ------------------------------------------------------------
# 11. Distribucion de variables respuesta
# ------------------------------------------------------------

vars_respuesta_criadero <- c(
  "prevalence", "immatures_presencia", "total_mosquito_emerged",
  "aedes_total", "aedes_aegypti", "aedes_albopictus"
)

resumen_respuestas <- resumir_numericas(datos_criadero, vars_respuesta_criadero)

cat("\nResumen de variables respuesta a nivel criadero:\n")
print(resumen_respuestas)

for (v in vars_respuesta_criadero[vars_respuesta_criadero %in% names(datos_criadero)]) {
  print(
    ggplot(datos_criadero, aes(x = .data[[v]])) +
      geom_histogram(bins = 30, color = "white") +
      labs(
        title = paste("Distribucion de", v),
        x = v,
        y = "Frecuencia"
      ) +
      theme_minimal()
  )
}

for (v in c("total_mosquito_emerged", "aedes_total", "aedes_aegypti", "aedes_albopictus")) {
  if (v %in% names(datos_criadero)) {
    print(
      ggplot(datos_criadero, aes(x = season, y = .data[[v]])) +
        geom_boxplot(outlier.alpha = 0.4) +
        labs(
          title = paste(v, "por estacion"),
          x = "Estacion",
          y = v
        ) +
        theme_minimal()
    )

    print(
      ggplot(
        datos_criadero %>% mutate(macrohabitat = fct_infreq(as.factor(macrohabitat))),
        aes(x = macrohabitat, y = .data[[v]])
      ) +
        geom_boxplot(outlier.alpha = 0.4) +
        coord_flip() +
        labs(
          title = paste(v, "por macrohabitat"),
          x = "Macrohabitat",
          y = v
        ) +
        theme_minimal()
    )
  }
}

# ------------------------------------------------------------
# 12. Ceros y sobredispersion
# ------------------------------------------------------------
# En una Poisson ideal, la media y la varianza deberian ser parecidas.
# Si varianza/media > 1, hay indicios simples de sobredispersion.

diagnostico_conteo <- function(x, nombre_variable) {
  x <- x[!is.na(x)]

  if (length(x) == 0) {
    return(
      tibble(
        variable = nombre_variable,
        n = 0,
        cantidad_ceros = NA_integer_,
        proporcion_ceros = NA_real_,
        media = NA_real_,
        mediana = NA_real_,
        varianza = NA_real_,
        minimo = NA_real_,
        maximo = NA_real_,
        indice_varianza_media = NA_real_,
        comentario = "No hay datos no faltantes para esta variable."
      )
    )
  }

  media <- mean(x)
  varianza <- ifelse(length(x) > 1, var(x), NA_real_)
  indice <- ifelse(is.na(media) || media == 0, NA_real_, varianza / media)

  tibble(
    variable = nombre_variable,
    n = length(x),
    cantidad_ceros = sum(x == 0),
    proporcion_ceros = mean(x == 0),
    media = media,
    mediana = median(x),
    varianza = varianza,
    minimo = min(x),
    maximo = max(x),
    indice_varianza_media = indice,
    comentario = case_when(
      is.na(indice) ~ "No se puede calcular varianza/media porque la media es cero o falta informacion.",
      indice > 1 ~ "Hay indicios de sobredispersion respecto de Poisson.",
      TRUE ~ "No hay indicios fuertes de sobredispersion con este criterio simple."
    )
  )
}

vars_conteo_criadero <- c(
  "total_mosquito_emerged",
  "aedes_total", "aedes_aegypti", "aedes_albopictus"
)
vars_conteo_criadero <- vars_conteo_criadero[vars_conteo_criadero %in% names(datos_criadero)]

diag_conteos <- bind_rows(
  lapply(vars_conteo_criadero, function(v) diagnostico_conteo(datos_criadero[[v]], v))
)

cat("\nDiagnostico de ceros y sobredispersion:\n")
print(diag_conteos)

# ------------------------------------------------------------
# 13. Ceros despues de sacar recipientes sin agua y modelos ZIP/ZINB
# ------------------------------------------------------------
# A partir de la devolucion del docente, miramos los ceros luego de sacar
# recipientes sin agua. Si aun quedan muchos ceros, eso fortalece la necesidad
# de considerar modelos inflados en cero. Ajustamos tres respuestas:
# abundancia total, Ae. aegypti y Ae. albopictus.
#
# Esta parte NO reemplaza el analisis exploratorio de la base completa.
# Es un segundo paso: primero entendemos todos los datos, despues hacemos
# el recorte biologicamente relevante para abundancia larval.

if (!requireNamespace("pscl", quietly = TRUE)) {
  stop("Falta el paquete pscl. Instalar con: install.packages('pscl')")
}

if (!requireNamespace("MASS", quietly = TRUE)) {
  stop("Falta el paquete MASS. Suele venir con R; revisar instalacion.")
}

datos_modelos_humedos <- datos_criadero %>%
  filter(final_volume_num > 0) %>%
  mutate(
    # Estandarizar permite comparar variables en escalas distintas.
    # Usamos log-volumen porque el volumen crudo tiene valores extremos.
    temperature_z = as.numeric(scale(temperature_num)),
    ph_z = as.numeric(scale(ph_num)),
    log_volume_z = as.numeric(scale(log_volume_num))
  )

ceros_humedos <- tibble(
  filas_base_completa = nrow(datos_criadero),
  filas_humedas = nrow(datos_modelos_humedos),
  ceros_total = sum(datos_modelos_humedos$total_mosquito_emerged == 0),
  prop_ceros_total = mean(datos_modelos_humedos$total_mosquito_emerged == 0),
  ceros_aegypti = sum(datos_modelos_humedos$aedes_aegypti == 0),
  prop_ceros_aegypti = mean(datos_modelos_humedos$aedes_aegypti == 0),
  ceros_albopictus = sum(datos_modelos_humedos$aedes_albopictus == 0),
  prop_ceros_albopictus = mean(datos_modelos_humedos$aedes_albopictus == 0)
)

cat("\nCeros luego de sacar recipientes sin agua:\n")
print(ceros_humedos)

datos_modelos_humedos_index <- datos_modelos_humedos %>%
  filter(grid_type == "Index")

ceros_humedos_index <- tibble(
  filas_humedas_index = nrow(datos_modelos_humedos_index),
  prop_ceros_total = mean(datos_modelos_humedos_index$total_mosquito_emerged == 0),
  prop_ceros_aegypti = mean(datos_modelos_humedos_index$aedes_aegypti == 0),
  prop_ceros_albopictus = mean(datos_modelos_humedos_index$aedes_albopictus == 0)
)

cat("\nCeros en criaderos humedos del subconjunto Index:\n")
print(ceros_humedos_index)

ajustar_modelos_conteo <- function(respuesta, base) {
  # Formula para Poisson y binomial negativo:
  # una sola parte del modelo, con efectos de temperatura, pH y log-volumen.
  formula_glm <- as.formula(
    paste(respuesta, "~ temperature_z + ph_z + log_volume_z")
  )

  # Formula para ZIP/ZINB:
  # antes del "|" esta el componente de conteo;
  # despues del "|" esta el componente que modela ceros extra.
  formula_zi <- as.formula(
    paste(respuesta, "~ temperature_z + ph_z + log_volume_z | temperature_z + ph_z + log_volume_z")
  )

  # Poisson se usa como punto de partida, aunque esperamos que sea insuficiente
  # porque los conteos tienen muchos ceros y sobredispersion.
  m_poisson <- glm(formula_glm, data = base, family = poisson())

  # Binomial negativo relaja el supuesto Poisson de media similar a varianza.
  m_nb <- MASS::glm.nb(formula_glm, data = base)

  # ZIP permite ceros extra, pero mantiene conteos Poisson.
  m_zip <- pscl::zeroinfl(formula_zi, data = base, dist = "poisson")

  # ZINB combina ceros extra + sobredispersion. Es el candidato mas completo.
  m_zinb <- pscl::zeroinfl(formula_zi, data = base, dist = "negbin")

  list(
    poisson = m_poisson,
    nb = m_nb,
    zip = m_zip,
    zinb = m_zinb
  )
}

respuestas_modelo <- c(
  "total_mosquito_emerged",
  "aedes_aegypti",
  "aedes_albopictus"
)

modelos_conteo <- lapply(
  respuestas_modelo,
  ajustar_modelos_conteo,
  base = datos_modelos_humedos
)
names(modelos_conteo) <- respuestas_modelo

comparacion_modelos <- bind_rows(
  lapply(names(modelos_conteo), function(resp) {
    tibble(
      respuesta = resp,
      modelo = names(modelos_conteo[[resp]]),
      aic = sapply(modelos_conteo[[resp]], AIC)
    )
  })
) %>%
  arrange(respuesta, aic)

cat("\nComparacion AIC entre Poisson, NB, ZIP y ZINB:\n")
print(comparacion_modelos)

# AIC mas bajo indica mejor equilibrio entre ajuste y complejidad,
# siempre comparando modelos ajustados sobre la misma respuesta y base.
mejor_modelo_por_respuesta <- comparacion_modelos %>%
  group_by(respuesta) %>%
  slice_min(aic, n = 1, with_ties = FALSE) %>%
  ungroup()

interpretacion_modelos <- tibble(
  modelo = c("Poisson", "Binomial negativo", "ZIP", "ZINB"),
  que_evalua = c(
    "Modelo base para conteos; supone media similar a varianza.",
    "Modelo para conteos con sobredispersion.",
    "Modelo Poisson con componente adicional para ceros extra.",
    "Modelo binomial negativo con componente adicional para ceros extra."
  ),
  lectura_en_este_analisis = c(
    "Queda como comparacion, pero no como candidato principal: hay sobredispersion fuerte y AIC muy alto.",
    "Mejora mucho respecto de Poisson; es una alternativa solida porque permite varianza mayor que la media.",
    "Mejora respecto de Poisson, pero queda peor que NB y ZINB porque no resuelve bien la sobredispersion.",
    "Tiene el menor AIC en las tres respuestas; es el candidato mas completo para una etapa posterior, aunque no se lo toma como modelo final cerrado."
  )
)

cat("\nMejor modelo por respuesta segun AIC:\n")
print(mejor_modelo_por_respuesta)

cat("\nInterpretacion de las familias de modelos:\n")
print(interpretacion_modelos)

extraer_coef_zinb <- function(modelo, respuesta) {
  resumen <- coef(summary(modelo))

  bind_rows(
    as.data.frame(resumen$count) %>%
      tibble::rownames_to_column("termino") %>%
      mutate(respuesta = respuesta, componente = "conteo"),
    as.data.frame(resumen$zero) %>%
      tibble::rownames_to_column("termino") %>%
      mutate(respuesta = respuesta, componente = "cero_inflado")
  )
}

resumen_modelos_zinb <- bind_rows(
  lapply(names(modelos_conteo), function(resp) {
    extraer_coef_zinb(modelos_conteo[[resp]]$zinb, resp)
  })
)

cat("\nCoeficientes de los modelos ZINB:\n")
print(resumen_modelos_zinb)

saveRDS(modelos_conteo, "Grupo5_Tp2_modelos_conteo_humedos.rds")

# ------------------------------------------------------------
# 14. Exploracion inicial de relaciones no lineales
# ------------------------------------------------------------
# El docente sugirio pensar como modelar posibles relaciones no lineales
# entre temperatura, pH, volumen y las abundancias. Como avance, comparamos
# modelos lineales contra modelos con splines naturales de baja complejidad.
# Para no sobreajustar, usamos df = 3 y mantenemos el componente de cero
# inflado con terminos lineales.
#
# Interpretacion esperada:
# Si los splines bajan claramente el AIC, habria evidencia de curvatura.
# Si no bajan el AIC, conviene sostener una version lineal mas parsimoniosa.

crear_resumen_bines <- function(base, respuesta, variable_x, n_bines = 6) {
  # Antes de ajustar splines, resumimos la respuesta por rangos de la variable.
  # Esto ayuda a mirar si hay indicios descriptivos de relaciones curvas.
  base %>%
    filter(!is.na(.data[[respuesta]]), !is.na(.data[[variable_x]])) %>%
    mutate(grupo = ggplot2::cut_number(.data[[variable_x]], n = n_bines)) %>%
    group_by(grupo) %>%
    summarise(
      respuesta = respuesta,
      variable_x = variable_x,
      n = n(),
      media_respuesta = mean(.data[[respuesta]], na.rm = TRUE),
      mediana_respuesta = median(.data[[respuesta]], na.rm = TRUE),
      prop_ceros = mean(.data[[respuesta]] == 0, na.rm = TRUE),
      media_x = mean(.data[[variable_x]], na.rm = TRUE),
      .groups = "drop"
    )
}

variables_no_lineales <- c("temperature_num", "ph_num", "log_volume_num")

resumen_bines_no_lineal <- bind_rows(
  lapply(respuestas_modelo, function(resp) {
    bind_rows(
      lapply(variables_no_lineales, function(x) {
        crear_resumen_bines(datos_modelos_humedos, resp, x)
      })
    )
  })
)

cat("\nResumen por bines para explorar relaciones no lineales:\n")
print(resumen_bines_no_lineal)

ajustar_seguro <- function(expr) {
  # Algunos modelos con splines pueden fallar por pocos conteos positivos.
  # En vez de cortar todo el script, devolvemos NA para ese AIC.
  tryCatch(
    suppressWarnings(expr),
    error = function(e) e
  )
}

aic_seguro <- function(modelo) {
  if (inherits(modelo, "error")) return(NA_real_)
  AIC(modelo)
}

comparar_lineal_spline <- function(respuesta, base) {
  # Splines naturales con df = 3: permiten una curva suave sin demasiada
  # flexibilidad. Los aplicamos a temperatura, pH y log-volumen.
  formula_nb_spline <- as.formula(
    paste(
      respuesta,
      "~ splines::ns(temperature_z, df = 3) +",
      "splines::ns(ph_z, df = 3) +",
      "splines::ns(log_volume_z, df = 3)"
    )
  )

  formula_zinb_spline <- as.formula(
    paste(
      respuesta,
      "~ splines::ns(temperature_z, df = 3) +",
      "splines::ns(ph_z, df = 3) +",
      "splines::ns(log_volume_z, df = 3) |",
      "temperature_z + ph_z + log_volume_z"
    )
  )

  aic_nb_lineal <- comparacion_modelos %>%
    filter(.data$respuesta == .env$respuesta, modelo == "nb") %>%
    pull(aic)

  aic_zinb_lineal <- comparacion_modelos %>%
    filter(.data$respuesta == .env$respuesta, modelo == "zinb") %>%
    pull(aic)

  nb_spline <- ajustar_seguro(MASS::glm.nb(formula_nb_spline, data = base))
  zinb_spline <- ajustar_seguro(
    pscl::zeroinfl(
      formula_zinb_spline,
      data = base,
      dist = "negbin",
      control = pscl::zeroinfl.control(maxit = 1000)
    )
  )

  valores_aic <- c(
    aic_nb_lineal,
    aic_seguro(nb_spline),
    aic_zinb_lineal,
    aic_seguro(zinb_spline)
  )

  tibble(
    respuesta = respuesta,
    modelo = c("NB lineal", "NB con splines", "ZINB lineal", "ZINB con splines"),
    aic = valores_aic,
    convergio = !is.na(valores_aic),
    comentario = c(
      "Efectos lineales de temperatura, pH y log-volumen.",
      "Splines naturales df = 3 para temperatura, pH y log-volumen.",
      "ZINB lineal ya usado para evaluar ceros y sobredispersion.",
      "Splines en el componente de conteo; componente de cero inflado lineal para mantener parsimonia."
    )
  )
}

comparacion_lineal_spline <- bind_rows(
  lapply(
    respuestas_modelo,
    comparar_lineal_spline,
    base = datos_modelos_humedos
  )
) %>%
  arrange(respuesta, aic)

cat("\nComparacion exploratoria entre efectos lineales y splines:\n")
print(comparacion_lineal_spline)

saveRDS(comparacion_lineal_spline, "Grupo5_Tp2_comparacion_lineal_spline.rds")

# ------------------------------------------------------------
# 15. Exploracion por grupos
# ------------------------------------------------------------
# Estos resumenes no prueban causalidad. Sirven para detectar patrones que
# luego pueden entrar en modelos binomiales o de conteo.

resumen_estacion <- datos_criadero %>%
  group_by(season) %>%
  summarise(
    n_criaderos = n(),
    prevalencia_media = mean(prevalence, na.rm = TRUE),
    abundancia_media = mean(total_mosquito_emerged, na.rm = TRUE),
    abundancia_mediana = median(total_mosquito_emerged, na.rm = TRUE),
    aedes_media = mean(aedes_total, na.rm = TRUE),
    ae_aegypti_media = mean(aedes_aegypti, na.rm = TRUE),
    ae_albopictus_media = mean(aedes_albopictus, na.rm = TRUE),
    prop_humedos = mean(wet_bin, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nResumen por estacion:\n")
print(resumen_estacion)

resumen_macrohabitat <- datos_criadero %>%
  group_by(macrohabitat) %>%
  summarise(
    n_criaderos = n(),
    prevalencia_media = mean(prevalence, na.rm = TRUE),
    abundancia_media = mean(total_mosquito_emerged, na.rm = TRUE),
    aedes_media = mean(aedes_total, na.rm = TRUE),
    ae_aegypti_media = mean(aedes_aegypti, na.rm = TRUE),
    ae_albopictus_media = mean(aedes_albopictus, na.rm = TRUE),
    prop_humedos = mean(wet_bin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prevalencia_media))

cat("\nResumen por macrohabitat:\n")
print(resumen_macrohabitat)

resumen_microhabitat <- datos_criadero %>%
  group_by(microhabitat) %>%
  summarise(
    n_criaderos = n(),
    prevalencia_media = mean(prevalence, na.rm = TRUE),
    abundancia_media = mean(total_mosquito_emerged, na.rm = TRUE),
    aedes_media = mean(aedes_total, na.rm = TRUE),
    ae_aegypti_total = sum(aedes_aegypti, na.rm = TRUE),
    ae_albopictus_total = sum(aedes_albopictus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prevalencia_media))

cat("\nResumen por microhabitat:\n")
print(resumen_microhabitat)

resumen_tipo_habitat <- datos_criadero %>%
  group_by(breeding_habitat_type) %>%
  summarise(
    n_criaderos = n(),
    prevalencia_media = mean(prevalence, na.rm = TRUE),
    abundancia_media = mean(total_mosquito_emerged, na.rm = TRUE),
    aedes_media = mean(aedes_total, na.rm = TRUE),
    prop_humedos = mean(wet_bin, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nResumen por habitat natural/artificial:\n")
print(resumen_tipo_habitat)

resumen_aedes_estacion_micro <- datos_criadero %>%
  group_by(season, microhabitat) %>%
  summarise(
    n_criaderos = n(),
    ae_aegypti_total = sum(aedes_aegypti, na.rm = TRUE),
    ae_albopictus_total = sum(aedes_albopictus, na.rm = TRUE),
    ae_aegypti_media = mean(aedes_aegypti, na.rm = TRUE),
    ae_albopictus_media = mean(aedes_albopictus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(season, desc(ae_aegypti_total + ae_albopictus_total))

cat("\nAe. aegypti y Ae. albopictus por estacion y microhabitat:\n")
print(resumen_aedes_estacion_micro)

# Graficos simples por grupos
ggplot(datos_criadero, aes(x = season, y = prevalence)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(
    title = "Prevalencia media por estacion",
    x = "Estacion",
    y = "Prevalencia media"
  ) +
  theme_minimal()

ggplot(
  datos_criadero %>% mutate(macrohabitat = fct_reorder(as.factor(macrohabitat), prevalence, mean, na.rm = TRUE)),
  aes(x = macrohabitat, y = prevalence)
) +
  stat_summary(fun = mean, geom = "bar") +
  coord_flip() +
  labs(
    title = "Prevalencia media por macrohabitat",
    x = "Macrohabitat",
    y = "Prevalencia media"
  ) +
  theme_minimal()

ggplot(
  datos_criadero %>% mutate(microhabitat = fct_lump_n(as.factor(microhabitat), n = 12)),
  aes(x = microhabitat, y = aedes_total)
) +
  geom_boxplot(outlier.alpha = 0.4) +
  coord_flip() +
  labs(
    title = "Conteos de Aedes por microhabitat",
    x = "Microhabitat",
    y = "Total de Aedes"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 16. Correlaciones simples entre variables abioticas
# ------------------------------------------------------------
# Primer chequeo de colinealidad. Se usa solo como orientacion descriptiva.

datos_humedos <- datos_criadero %>%
  filter(wet == "Y")

vars_abioticas <- datos_humedos %>%
  select(any_of(c("final_volume_num", "log_volume_num", "temperature_num", "ph_num")))

cat("\nResumen de variables abioticas en habitats humedos:\n")
print(summary(vars_abioticas))

cat("\nCorrelacion simple entre variables abioticas:\n")
print(cor(vars_abioticas, use = "pairwise.complete.obs"))

# ------------------------------------------------------------
# 17. Posibles valores extremos
# ------------------------------------------------------------
# No se eliminan automaticamente. Solo se listan para evaluar si son errores,
# casos biologicamente reales o registros que necesitan aclaracion.

cat("\nValores mas altos de Aedes total:\n")
print(datos_criadero %>% arrange(desc(aedes_total)) %>% head(20))

cat("\nValores mas altos de volumen final:\n")
print(datos_criadero %>% arrange(desc(final_volume_num)) %>% head(20))

cat("\nValores mas altos de temperatura en habitats humedos:\n")
print(datos_humedos %>% arrange(desc(temperature_num)) %>% head(20))

cat("\nValores mas bajos de pH en habitats humedos:\n")
print(datos_humedos %>% arrange(ph_num) %>% head(20))

cat("\nValores mas altos de pH en habitats humedos:\n")
print(datos_humedos %>% arrange(desc(ph_num)) %>% head(20))

# ------------------------------------------------------------
# 18. Salidas utiles para el informe
# ------------------------------------------------------------

write.csv(na_resumen, "Grupo5_Tp2_resumen_NA.csv", row.names = FALSE)
write.csv(comparacion_identificadores, "Grupo5_Tp2_comparacion_identificadores.csv", row.names = FALSE)
write.csv(resumen_numericas, "Grupo5_Tp2_resumen_numericas.csv", row.names = FALSE)
write.csv(resumen_respuestas, "Grupo5_Tp2_resumen_respuestas.csv", row.names = FALSE)
write.csv(diag_conteos, "Grupo5_Tp2_diagnostico_ceros_sobredispersion.csv", row.names = FALSE)
write.csv(ceros_humedos, "Grupo5_Tp2_ceros_humedos.csv", row.names = FALSE)
write.csv(ceros_humedos_index, "Grupo5_Tp2_ceros_humedos_index.csv", row.names = FALSE)
write.csv(comparacion_modelos, "Grupo5_Tp2_comparacion_modelos_conteo.csv", row.names = FALSE)
write.csv(mejor_modelo_por_respuesta, "Grupo5_Tp2_mejor_modelo_por_respuesta.csv", row.names = FALSE)
write.csv(interpretacion_modelos, "Grupo5_Tp2_interpretacion_modelos.csv", row.names = FALSE)
write.csv(resumen_modelos_zinb, "Grupo5_Tp2_coeficientes_modelos_zinb.csv", row.names = FALSE)
write.csv(resumen_bines_no_lineal, "Grupo5_Tp2_resumen_bines_no_lineal.csv", row.names = FALSE)
write.csv(comparacion_lineal_spline, "Grupo5_Tp2_comparacion_lineal_spline.csv", row.names = FALSE)
write.csv(resumen_estacion, "Grupo5_Tp2_resumen_estacion.csv", row.names = FALSE)
write.csv(resumen_macrohabitat, "Grupo5_Tp2_resumen_macrohabitat.csv", row.names = FALSE)
write.csv(resumen_microhabitat, "Grupo5_Tp2_resumen_microhabitat.csv", row.names = FALSE)
write.csv(resumen_tipo_habitat, "Grupo5_Tp2_resumen_tipo_habitat.csv", row.names = FALSE)
write.csv(resumen_aedes_estacion_micro, "Grupo5_Tp2_resumen_aedes_estacion_micro.csv", row.names = FALSE)

# ------------------------------------------------------------
# 19. Cierre del analisis exploratorio
# ------------------------------------------------------------

variables_muchos_ceros <- diag_conteos %>%
  filter(proporcion_ceros >= 0.50) %>%
  pull(variable)

variables_sobredispersas <- diag_conteos %>%
  filter(indice_varianza_media > 1) %>%
  pull(variable)

cierre_ae <- tibble(
  aspecto = c(
    "Unidad de analisis",
    "Valores faltantes",
    "Ceros",
    "Sobredispersion",
    "Modelos inflados en cero",
    "Relaciones no lineales",
    "Predictoras candidatas",
    "Recomendacion posterior"
  ),
  resultado = c(
    "La base original debe contrastarse con la base agregada por criadero, porque puede haber mas de una fila por recipiente.",
    "Revisar especialmente variables ambientales de agua y su relacion con la condicion humedo/seco.",
    paste("Variables con al menos 50% de ceros:", paste(variables_muchos_ceros, collapse = ", ")),
    paste("Variables con varianza/media > 1:", paste(variables_sobredispersas, collapse = ", ")),
    "Luego de sacar recipientes sin agua, siguen quedando muchos ceros; se ajustaron modelos ZIP/ZINB para total de mosquitos, Ae. aegypti y Ae. albopictus con variables numericas estandarizadas.",
    "Como avance, se compararon efectos lineales contra splines naturales de baja complejidad para temperatura, pH y log-volumen.",
    "Estacion, macrohabitat, microhabitat, tipo natural/artificial, volumen, temperatura y pH.",
    "Para presencia/ausencia: modelos binomiales. Para conteos: evaluar binomial negativo y, si los ceros tienen base biologica, modelos hurdle o inflados en cero."
  )
)

cat("\nCierre del analisis exploratorio:\n")
print(cierre_ae)

write.csv(cierre_ae, "Grupo5_Tp2_cierre_analisis_exploratorio.csv", row.names = FALSE)
