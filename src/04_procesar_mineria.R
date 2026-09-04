# -----------------------------------------------------------------------
# valor_produccion_distrito_mensual.R
#
# Estima el VALOR de la producción minera metálica mensual por distrito,
# 2019-2026, cruzando:
#   - producción distrital ANUAL (por mineral)               [dato exacto]
#   - producción nacional MENSUAL (por mineral)              [dato exacto]
#   - cotización internacional MENSUAL (por mineral)         [dato exacto]
#   - tipo de cambio mensual                                 [dato exacto]
#
# LÓGICA (simple, en 3 pasos):
#   1. PARTICIPACIÓN: qué % de la producción nacional ANUAL de un mineral
#      aporta cada distrito, cada año (producción distrital / nacional).
#   2. MENSUALIZAR: esa participación se aplica a la producción nacional
#      MENSUAL (que sí varía mes a mes) para estimar cuánto produjo el
#      distrito cada mes. Supuesto: el distrito mantiene la misma
#      participación relativa todos los meses de un año.
#   3. VALORIZAR: producción distrital mensual x cotización del mes, con
#      conversión de unidades (Cuadro N°01 vs Cuadro N°02 de los metadatos).
#
# 2026 es un caso especial: el archivo de producción distrital ANUAL sólo
# llega a 2025 (2026 sigue en curso). Para esos meses se usa la ÚLTIMA
# participación conocida de cada distrito (la de 2025) aplicada a la
# producción nacional mensual de 2026, que sí está disponible. Las filas
# resultantes quedan marcadas con `es_estimado = TRUE`.
#
# Sólo se valorizan los 9 minerales con cotización internacional (COBRE,
# ORO, ZINC, PLATA, PLOMO, ESTAÑO, HIERRO, MOLIBDENO, MANGANESO). El
# archivo de producción nacional mensual trae 4 minerales adicionales
# (ARSÉNICO, BISMUTO, CADMIO, MAGNESIO) sin cotización publicada — se
# descartan porque no se les puede calcular un valor.
#
# De minerales_indicadores_macro.csv sólo se usa TIPO DE CAMBIO (para dar
# el valor también en soles); PBI, inflación, exportaciones, etc. no son
# necesarios para este cálculo y no se incorporan.
# -----------------------------------------------------------------------

library(tidyverse)
library(stringi)

# --- Rutas ---------------------------------------------------------------
archivo_cotizacion   <- "data/interim/minerales_cotizacion.csv"
archivo_macro        <- "data/interim/minerales_indicadores_macro.csv"
archivo_distrito     <- "data/interim/minerales_produccion_distrito_anual.csv"
archivo_nacional_mes <- "data/interim/minerales_produccion_nacional_mensual.csv"
archivo_ubigeos      <- "data/interim/ubigeos_2025.csv"

archivo_salida_rds <- "data/processed/valor_produccion_distrito_mensual.rds"

MESES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
           "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# --- Factores de conversión a USD (Cuadro N°01: unidad de volumen; ------
# --- Cuadro N°02: unidad de cotización) -----------------------------------
# LME/LBMA cotizan cobre, zinc, plomo, estaño en Ctvs.USD/lb; molibdeno en
# USD/lb; oro y plata en USD/oz troy; hierro y manganeso en USD/TM. El
# volumen viene en TMF para casi todos, salvo oro (gf) y plata (kgf).
LB_POR_TMF  <- 2204.622622       # 1 tonelada métrica = 2,204.62 libras
OZT_POR_GF  <- 1 / 31.1034768    # 1 onza troy = 31.10 gramos finos
OZT_POR_KGF <- 1000 / 31.1034768 # 1 kg fino = 1000 g finos

factor_conversion <- tribble(
  ~MINERAL,     ~factor_a_usd,
  "COBRE",      LB_POR_TMF / 100,   # Ctvs.USD/lb -> USD/lb, y TMF -> lb
  "ZINC",       LB_POR_TMF / 100,
  "PLOMO",      LB_POR_TMF / 100,
  "ESTAÑO",     LB_POR_TMF / 100,
  "MOLIBDENO",  LB_POR_TMF,         # ya en USD/lb, sólo TMF -> lb
  "ORO",        OZT_POR_GF,         # gf -> oz troy
  "PLATA",      OZT_POR_KGF,        # kgf -> oz troy
  "HIERRO",     1,                  # USD/TM y TMF ya son la misma base
  "MANGANESO",  1
)

# --- Cotizaciones a formato largo (anio, mes, MINERAL, precio) --------
cotizacion <- read_csv(archivo_cotizacion, locale = locale(encoding = "UTF-8")) %>%
  pivot_longer(-c(AÑO, MES), names_to = "MINERAL", values_to = "precio") %>%
  mutate(mes = match(MES, MESES)) %>%
  select(anio = AÑO, mes, MINERAL, precio)

# --- Tipo de cambio mensual (único dato usado de indicadores_macro) --
tipo_cambio <- read_csv(archivo_macro, locale = locale(encoding = "UTF-8")) %>%
  mutate(mes = match(MES, MESES)) %>%
  select(anio = AÑO, mes, tipo_cambio = `TIPO DE CAMBIO`)

# --- Producción nacional mensual a formato largo ----------------------
nacional_mensual <- read_csv(archivo_nacional_mes, locale = locale(encoding = "UTF-8")) %>%
  pivot_longer(-c(AÑO, MES), names_to = "MINERAL", values_to = "produccion_nacional") %>%
  mutate(mes = match(MES, MESES)) %>%
  select(anio = AÑO, mes, MINERAL, produccion_nacional) %>%
  filter(MINERAL %in% factor_conversion$MINERAL)  # sólo minerales con cotización

# Producción nacional ANUAL = suma de los 12 meses de cada año. Es el
# denominador de la participación distrital (paso 4).
nacional_anual <- nacional_mensual %>%
  group_by(anio, MINERAL) %>%
  summarise(produccion_nacional_anual = sum(produccion_nacional, na.rm = TRUE), .groups = "drop")

# --- Producción distrital anual a formato largo + participación ------
distrito_anual <- read_csv(archivo_distrito, locale = locale(encoding = "UTF-8")) %>%
  pivot_longer(cols = matches("^[0-9]{4}$"), names_to = "anio", values_to = "produccion_distrito_anual") %>%
  mutate(anio = as.integer(anio)) %>%
  filter(!is.na(produccion_distrito_anual))

participacion <- distrito_anual %>%
  left_join(nacional_anual, by = c("anio", "MINERAL")) %>%
  mutate(participacion = produccion_distrito_anual / produccion_nacional_anual) %>%
  select(anio, MINERAL, DEPARTAMENTO, PROVINCIA, DISTRITO, participacion)

# 2026 no tiene producción distrital anual todavía (año en curso): se
# reutiliza la última participación conocida de cada distrito (2025).
participacion_2026 <- participacion %>%
  filter(anio == max(anio)) %>%
  mutate(anio = max(anio) + 1L)

participacion <- participacion %>%
  mutate(es_estimado = FALSE) %>%
  bind_rows(participacion_2026 %>% mutate(es_estimado = TRUE))

# --- Mensualizar: participación x producción nacional mensual --------
produccion_distrito_mensual <- participacion %>%
  left_join(nacional_mensual, by = c("anio", "MINERAL"), relationship = "many-to-many") %>%
  filter(!is.na(produccion_nacional)) %>%   # 2026 sólo trae los meses ya publicados
  mutate(produccion_distrito_mensual = participacion * produccion_nacional)

# --- Valorizar: producción distrital mensual x cotización ------------
valor_produccion <- produccion_distrito_mensual %>%
  left_join(cotizacion, by = c("anio", "mes", "MINERAL")) %>%
  left_join(factor_conversion, by = "MINERAL") %>%
  left_join(tipo_cambio, by = c("anio", "mes")) %>%
  mutate(
    valor_produccion_usd = produccion_distrito_mensual * precio * factor_a_usd,
    valor_produccion_pen = valor_produccion_usd * tipo_cambio
  ) %>%
  select(anio, mes, DEPARTAMENTO, PROVINCIA, DISTRITO, MINERAL,
         produccion_distrito_mensual, precio, valor_produccion_usd,
         valor_produccion_pen, es_estimado)

# --- Agregar UBIGEO -----------------------------------------------------
# Cruce por nombre normalizado (mayúsculas, sin tildes) de departamento +
# provincia + distrito, no solo por distrito: así dos distritos homónimos en
# provincias distintas no se confunden entre sí.
normalizar <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("\\([^)]*\\)", " ") %>%
    str_replace_all("[._-]", " ") %>%
    str_replace_all("[^A-Z0-9 ]", " ") %>%
    str_squish()
}

ubigeos <- read_csv(archivo_ubigeos, locale = locale(encoding = "UTF-8"),
                    col_types = cols(.default = "c")) %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO_NOMBRE),
    prov_key = normalizar(PROVINCIA_NOMBRE),
    dist_key = normalizar(DISTRITO_NOMBRE)
  )

# Variante de escritura genuina detectada al comparar ambas fuentes (mismo
# caso ya visto en el panel de población): se homologa la llave de
# "valor_produccion" a la grafía usada en ubigeos_2025.
crosswalk_distritos <- tribble(
  ~dep_key, ~prov_key, ~dist_key_datos,               ~dist_key_ubigeo,
  "PASCO",  "PASCO",   "SAN FCO DE ASIS DE YARUSYAC", "SAN FRANCISCO DE ASIS DE YARUSYACAN"
)

valor_produccion <- valor_produccion %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO),
    prov_key = normalizar(PROVINCIA),
    dist_key = normalizar(DISTRITO)
  ) %>%
  left_join(crosswalk_distritos, by = c("dep_key", "prov_key", "dist_key" = "dist_key_datos")) %>%
  mutate(dist_key = coalesce(dist_key_ubigeo, dist_key)) %>%
  select(-dist_key_ubigeo) %>%
  left_join(ubigeos %>% select(dep_key, prov_key, dist_key, UBIGEO),
            by = c("dep_key", "prov_key", "dist_key")) %>%
  select(-dep_key, -prov_key, -dist_key) %>%
  relocate(UBIGEO, .before = DEPARTAMENTO)

sin_ubigeo <- valor_produccion %>% filter(is.na(UBIGEO)) %>%
  distinct(DEPARTAMENTO, PROVINCIA, DISTRITO)
if (nrow(sin_ubigeo) > 0) {
  message("\nDistritos sin UBIGEO (revisar variante de nombre):")
  print(sin_ubigeo)
}

valor_produccion <- valor_produccion %>%
  arrange(DEPARTAMENTO, PROVINCIA, DISTRITO, MINERAL, anio, mes)

# --- 7. Exportar (formato largo: 1 fila = distrito x mineral x mes) -----
dir.create(dirname(archivo_salida_rds), showWarnings = FALSE, recursive = TRUE)
write_rds(valor_produccion, archivo_salida_rds)

message(sprintf(
  "Listo: %s filas x %s columnas (%d-%d, %d distritos, %d minerales).\n%d filas de 2026 son estimadas (participación 2025 x producción nacional real de 2026).",
  format(nrow(valor_produccion), big.mark = ","), ncol(valor_produccion),
  min(valor_produccion$anio), max(valor_produccion$anio),
  n_distinct(valor_produccion$DEPARTAMENTO, valor_produccion$PROVINCIA, valor_produccion$DISTRITO),
  n_distinct(valor_produccion$MINERAL),
  sum(valor_produccion$es_estimado)
))
message("Archivos: ", archivo_salida_rds)

