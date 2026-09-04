library(tidyverse)
library(stringi)
library(fs)

archivo_poblacion <- path("data/interim/poblacion.csv")
archivo_ubigeos <- path("data/interim/ubigeos_2025.csv")
archivo_poblacion_rds <- path("data/processed/data_poblacion.rds")

# ---------------------------------------------------------------------------
# Carga de datos
# ---------------------------------------------------------------------------
poblacion <- read_csv(archivo_poblacion, locale = locale(encoding = "UTF-8"),
                      na = c("", "NA", "-")) %>%   # p.ej. "Alto Trujillo" trae "-" en años sin dato (distrito creado después)
  mutate(across(matches("^20[0-9]{2}$"), as.numeric)) %>%
  rename(DEPARTAMENTO_POB = DEPARTAMENTO,
         PROVINCIA_POB    = PROVINCIA,
         DISTRITO_POB     = DISTRITO)

ubigeo <- read_csv(archivo_ubigeos, locale = locale(encoding = "UTF-8"),
                   col_types = cols(.default = "c")) %>%
  rename(DEPARTAMENTO_UBI = DEPARTAMENTO_NOMBRE,
         PROVINCIA_UBI    = PROVINCIA_NOMBRE,
         DISTRITO_UBI     = DISTRITO_NOMBRE) %>%
  mutate(across(c(DEPARTAMENTO_UBI, PROVINCIA_UBI, DISTRITO_UBI), str_trim))

# ---------------------------------------------------------------------------
# Normalización de texto para el cruce
# mayúsculas, sin tildes/diéresis/ñ->n, sin contenido entre paréntesis,
# puntuación (.,_-) convertida a espacio, espacios múltiples colapsados.
# Esto NO se usa para mostrar los nombres finales, solo como llave de cruce.
# ---------------------------------------------------------------------------
normalizar <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%            # tildes, ñ, diéresis -> ASCII
    str_replace_all("\\([^)]*\\)", " ") %>%          # quita contenido entre paréntesis, ej. "(KICHKI)"
    str_replace_all("[._-]", " ") %>%                # puntuación de separación -> espacio
    str_replace_all("[^A-Z0-9 ]", " ") %>%           # cualquier otro símbolo raro -> espacio
    str_squish()                                     # colapsa espacios múltiples y recorta
}

poblacion <- poblacion %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO_POB),
    prov_key = normalizar(PROVINCIA_POB),
    dist_key = normalizar(DISTRITO_POB)
  )

ubigeo <- ubigeo %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO_UBI),
    prov_key = normalizar(PROVINCIA_UBI),
    dist_key = normalizar(DISTRITO_UBI)
  )

# ---------------------------------------------------------------------------
# Caso especial: Callao
# En "poblacion" la provincia figura como "CALLAO"; en "ubigeos_2025" figura
# como "PROV. CONST. DEL CALLAO". Como el departamento Callao tiene una sola
# provincia, homologamos la llave de provincia para ese departamento.
# ---------------------------------------------------------------------------
poblacion <- poblacion %>%
  mutate(prov_key = if_else(dep_key == "CALLAO", "CALLAO", prov_key))

ubigeo <- ubigeo %>%
  mutate(prov_key = if_else(dep_key == "CALLAO", "CALLAO", prov_key))

# ---------------------------------------------------------------------------
# Crosswalk manual de variantes de escritura genuinas (no resueltas por la
# normalización anterior) detectadas al comparar ambas fuentes.
# Se homologa la llave de "poblacion" a la grafía usada en "ubigeos_2025".
# dep_key / prov_key acotan cada caso para no afectar homónimos de otros
# departamentos/provincias.
# ---------------------------------------------------------------------------
crosswalk_distritos <- tribble(
  ~dep_key,    ~prov_key,        ~dist_key_pob,                  ~dist_key_ubi,
  "AMAZONAS",  "LUYA",           "SAN FRANCISCO DEL YESO",       "SAN FRANCISCO DE YESO",
  "ANCASH",    "HUARAZ",         "PAMPAS",                       "PAMPAS GRANDE",
  "AYACUCHO",  "LA MAR",         "ORONCOY",                      "ORONCCOY",
  "AYACUCHO",  "VICTOR FAJARDO", "HUAYA",                        "HUALLA",
  "LIMA",      "YAUYOS",         "AYAUCA",                       "ALLAUCA",
  "PASCO",     "PASCO",          "SAN FCO DE ASIS DE YARUSYAC",  "SAN FRANCISCO DE ASIS DE YARUSYACAN",
  "PUNO",      "SANDIA",         "SAN PEDRO DE PUTINA PUNCU",    "SAN PEDRO DE PUTINA PUNCO",
  "UCAYALI",   "ATALAYA",        "RAIMONDI",                     "RAYMONDI"
)

poblacion <- poblacion %>%
  left_join(crosswalk_distritos,
            by = c("dep_key", "prov_key", "dist_key" = "dist_key_pob")) %>%
  mutate(dist_key = coalesce(dist_key_ubi, dist_key)) %>%
  select(-dist_key_ubi)

# ---------------------------------------------------------------------------
# Cruce por llave compuesta (departamento + provincia + distrito), lo que
# resuelve homónimos de distrito y de provincia: dos distritos "Santa Rosa"
# en provincias distintas, o dos provincias del mismo nombre en
# departamentos distintos, solo calzan si TODA la llave coincide.
# ---------------------------------------------------------------------------
poblacion_ubigeo <- poblacion %>%
  left_join(
    ubigeo %>% select(UBIGEO,
                      COD_DEPARTAMENTO = DEPARTAMENTO,
                      COD_PROVINCIA    = PROVINCIA,
                      COD_DISTRITO     = DISTRITO,
                      DEPARTAMENTO_UBI, PROVINCIA_UBI, DISTRITO_UBI,
                      dep_key, prov_key, dist_key),
    by = c("dep_key", "prov_key", "dist_key")
  ) %>%
  select(-dep_key, -prov_key, -dist_key)

# ---------------------------------------------------------------------------
# Diagnóstico de cruce (no debería quedar ningún caso salvo excepciones
# reales de la fuente, p. ej. un distrito sin dato de población)
# ---------------------------------------------------------------------------
sin_ubigeo <- poblacion_ubigeo %>% filter(is.na(UBIGEO))
cat("Filas de población:", nrow(poblacion), "\n")
cat("Filas sin ubigeo asignado:", nrow(sin_ubigeo), "\n")
if (nrow(sin_ubigeo) > 0) {
  cat("Revisar manualmente (posible variante de nombre no cubierta por el crosswalk):\n")
  print(sin_ubigeo %>% select(DEPARTAMENTO_POB, PROVINCIA_POB, DISTRITO_POB))
}

# ---------------------------------------------------------------------------
# Quitar columnas de departamento/provincia/distrito (nombres y códigos) y
# pasar los años de formato ancho a largo. Queda solo UBIGEO como
# identificador territorial (ya contiene los códigos de dpto/prov/distrito).
# ---------------------------------------------------------------------------
poblacion_ubigeo <- poblacion_ubigeo %>%
  select(UBIGEO, matches("^20[0-9]{2}$")) %>%
  arrange(UBIGEO) %>%
  pivot_longer(
    cols = matches("^20[0-9]{2}$"),
    names_to = "ANIO",
    values_to = "POBLACION"
  ) %>%
  mutate(ANIO = as.integer(ANIO))

# ---------------------------------------------------------------------------
# Exportar
# ---------------------------------------------------------------------------

dir.create(dirname(archivo_poblacion_rds), recursive = TRUE, showWarnings = FALSE)
write_rds(poblacion_ubigeo, archivo_poblacion_rds)

