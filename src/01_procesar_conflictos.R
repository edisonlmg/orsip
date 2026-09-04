library(tidyverse)
library(xml2)
library(stringi)
library(fs)

# --- Configuración ---------------------------------------------------------
# Carpeta donde están todos los .kml, cada uno nombrado, p. ej.:
#   "Reporte de Conflictos Sociales - Enero 2026.kml"

carpeta_kml <- path("data/raw/conflictos/")
archivo_ubigeos <- path("data/interim/ubigeos_2025.csv")
archivo_conflictos_rds <- path("data/processed/data_conflictos.rds")

meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
           "Setiembre","Septiembre","Octubre","Noviembre","Diciembre")

# --- Función: extraer mes y año del nombre del archivo ---------------------
extraer_mes_anio <- function(nombre_archivo) {
  base <- tools::file_path_sans_ext(basename(nombre_archivo))
  patron_meses <- paste(meses, collapse = "|")
  m <- regmatches(base, regexpr(paste0("(", patron_meses, ")\\s+(\\d{4})"), base, ignore.case = TRUE))
  if (length(m) == 0 || nchar(m) == 0) {
    return(c(Mes = NA_character_, Anio = NA_character_))
  }
  partes <- strsplit(trimws(m), "\\s+")[[1]]
  c(Mes = partes[1], Anio = partes[2])
}

# --- Función: extraer todos los placemarks de un KML como data.frame -------
extraer_kml <- function(kml_path) {
  doc <- read_xml(kml_path)
  ns  <- xml_ns(doc)  # namespace por defecto: http://www.opengis.net/kml/2.2
  
  placemarks <- xml_find_all(doc, ".//d1:Placemark", ns)
  
  extraer_placemark <- function(pm) {
    nombre    <- xml_text(xml_find_first(pm, ".//d1:name", ns))
    direccion <- xml_text(xml_find_first(pm, ".//d1:address", ns))  # sin lat/long, solo dirección de texto
    
    data_nodes <- xml_find_all(pm, ".//d1:ExtendedData/d1:Data", ns)
    campos <- setNames(
      sapply(data_nodes, function(d) xml_text(xml_find_first(d, ".//d1:value", ns))),
      sapply(data_nodes, function(d) xml_attr(d, "name"))
    )
    
    as.list(c(
      "Denominación del caso" = nombre,
      "Dirección (texto)" = direccion,
      campos
    ))
  }
  
  lista_casos <- lapply(placemarks, extraer_placemark)
  if (length(lista_casos) == 0) return(NULL)
  
  # Unificar en data.frame (por si algún placemark tuviera campos distintos)
  todas_cols <- unique(unlist(lapply(lista_casos, names)))
  filas <- lapply(lista_casos, function(x) {
    x_completo <- setNames(as.list(rep(NA, length(todas_cols))), todas_cols)
    x_completo[names(x)] <- x
    as.data.frame(x_completo, stringsAsFactors = FALSE, check.names = FALSE)
  })
  df <- do.call(rbind, filas)
  
  # El mapa repite los mismos casos en varias capas (Todos / Activos / Socioambientales),
  # por eso hay más placemarks que casos reales dentro de UN MISMO archivo.
  # Deduplicamos por nombre de caso dentro de este KML.
  df <- df[!duplicated(df[["Denominación del caso"]]), ]
  
  mes_anio <- extraer_mes_anio(kml_path)
  mes <- unname(mes_anio["Mes"])
  mes <- recode(mes, "Septiembre" = "Setiembre")
  
  df$Mes  <- match(
    mes,
    c(
      "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
      "Julio", "Agosto", "Setiembre", "Octubre",
      "Noviembre", "Diciembre"
    )
  )
  
  df$Anio <- as.integer(unname(mes_anio["Anio"]))
  
  df
}

# --- Procesar todos los KML de la carpeta y apilar --------------------------
archivos_kml <- list.files(carpeta_kml, pattern = "\\.kml$", full.names = TRUE, ignore.case = TRUE)
cat("Archivos KML encontrados:", length(archivos_kml), "\n")
print(basename(archivos_kml))

resultados <- lapply(archivos_kml, function(f) {
  cat("Procesando:", basename(f), "... ")
  res <- tryCatch(extraer_kml(f), error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
    NULL
  })
  if (!is.null(res)) cat(nrow(res), "casos únicos\n")
  res
})

resultados <- Filter(Negate(is.null), resultados)

# Unificar columnas entre archivos (por si algún mes trajera campos distintos) y apilar
todas_cols <- unique(unlist(lapply(resultados, names)))
resultados_completos <- lapply(resultados, function(df) {
  faltantes <- setdiff(todas_cols, names(df))
  for (col in faltantes) df[[col]] <- NA
  df[todas_cols]
})
conflictos_consolidado <- do.call(rbind, resultados_completos)
rownames(conflictos_consolidado) <- NULL
conflictos_consolidado <- as_tibble(conflictos_consolidado) %>%
  # El KML trae "" (no NA) cuando un campo viene vacío (p. ej. casos
  # multirregión sin Provincia/Distrito puntual). Se homologa a NA real
  # para que los diagnósticos y el cruce de UBIGEO no lo traten como dato.
  mutate(across(where(is.character), ~ na_if(str_trim(.x), "")))

cat("\nTotal de filas apiladas (todos los meses):", nrow(conflictos_consolidado), "\n")

# ---------------------------------------------------------------------------
# Nombres de columna en formato "variable": mayúsculas, sin tildes ni
# caracteres especiales, sin espacios (guion bajo). Los nombres largos u
# oscuros del KML original se resumen en una variable clara y corta.
# any_of() evita error si algún mes no trae todas las columnas.
# ---------------------------------------------------------------------------
nombres_nuevos <- c(
  ANIO                       = "Anio",
  MES                        = "Mes",
  CASO                       = "Denominación del caso",
  ESTADO                     = "Estado",
  TIPO                       = "Tipo",
  ACTIVIDAD                  = "Actividad",
  FASE_CASO_ACTIVO           = "Fases (casos activos)",
  EMPRESA                    = "Empresa involucrada",
  MOMENTO_DIALOGO            = "Momento del diálogo",
  MECANISMO_DIALOGO          = "Mecanismo del diálogo",
  HECHO_VIOLENCIA            = "Al menos tuvieron un hecho de violencia",
  PARTICIPACION_DP_DIALOGO   = "Participación de la DP en el espacio de diálogo",
  DIALOGO_POST_CRISIS        = "Diálogo después de hecho de violencia (crisis)",
  COMPETENCIA_GOBIERNO       = "Principal competencias por nivel de gobierno",
  PRESENCIA_DP               = "Presencia de la Defensoria del Pueblo",
  MULTIRREGION_NACIONAL      = "Multiregión o nacional",
  PAIS                       = "País",
  DEPARTAMENTO               = "Dpto.",
  PROVINCIA                  = "Provincia",
  DISTRITO                   = "Distrito",
  LOCALIDAD                  = "Localidad",
  OTROS_LUGARES              = "Otros lugares involucrados",
  DIRECCION                  = "Dirección (texto)",
  ARCHIVO_ORIGEN             = "Archivo origen"
)

conflictos_consolidado <- conflictos_consolidado %>%
  rename(any_of(nombres_nuevos))

# ---------------------------------------------------------------------------
# Integrar UBIGEO de departamento, provincia y distrito por separado,
# cruzando por nombre normalizado contra ubigeos_2025.csv (misma lógica
# de normalización/homónimos usada para el dataset de población).
# ---------------------------------------------------------------------------
ubigeo <- read_csv(archivo_ubigeos, locale = locale(encoding = "UTF-8"),
                   col_types = cols(.default = "c")) %>%
  mutate(across(c(DEPARTAMENTO_NOMBRE, PROVINCIA_NOMBRE, DISTRITO_NOMBRE), str_trim))

normalizar <- function(x) {
  x %>%
    str_trim() %>%
    str_to_upper() %>%
    stri_trans_general("Latin-ASCII") %>%            # tildes, ñ, diéresis -> ASCII
    str_replace_all("\\([^)]*\\)", " ") %>%          # quita contenido entre paréntesis
    str_replace_all("[._-]", " ") %>%                # puntuación de separación -> espacio
    str_replace_all("[^A-Z0-9 ]", " ") %>%           # cualquier otro símbolo raro -> espacio
    str_squish()                                     # colapsa espacios múltiples y recorta
}

ubigeo <- ubigeo %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO_NOMBRE),
    prov_key = normalizar(PROVINCIA_NOMBRE),
    dist_key = normalizar(DISTRITO_NOMBRE)
  )

conflictos_consolidado <- conflictos_consolidado %>%
  mutate(
    dep_key  = normalizar(DEPARTAMENTO),
    prov_key = normalizar(PROVINCIA),
    dist_key = normalizar(DISTRITO)
  )

# Callao: el reporte de conflictos suele traer "Prov.Const.del Callao"; el
# departamento Callao tiene una sola provincia, así que homologamos la llave.
conflictos_consolidado <- conflictos_consolidado %>%
  mutate(prov_key = if_else(dep_key == "CALLAO", "CALLAO", prov_key))
ubigeo <- ubigeo %>%
  mutate(prov_key = if_else(dep_key == "CALLAO", "CALLAO", prov_key))

# Crosswalk de variantes de escritura genuinas detectadas en el reporte de
# conflictos frente a ubigeos_2025 (se amplía aquí si aparecen nuevas al
# incorporar más meses).
crosswalk_departamentos <- tribble(
  ~dep_key_conf,          ~dep_key_ubi,
  "LIMA METROPOLITANA",   "LIMA",
  "LIMA PROVINCIAS",      "LIMA"
)
crosswalk_provincias <- tribble(
  ~dep_key,   ~prov_key_conf, ~prov_key_ubi,
  "ANCASH",   "EL SANTA",     "SANTA"
)

# "Surco" es el nombre coloquial de "Santiago de Surco" (Lima, prov. Lima).
# OJO: existe un distrito oficial distinto llamado literalmente "Surco" en
# otra provincia de Lima (Yauyos) -> por eso el crosswalk va acotado a
# dep+prov exactos, para no confundir un homónimo real con el otro.
crosswalk_distritos_conf <- tribble(
  ~dep_key, ~prov_key, ~dist_key_conf, ~dist_key_ubi,
  "LIMA",   "LIMA",    "SURCO",        "SANTIAGO DE SURCO"
)

conflictos_consolidado <- conflictos_consolidado %>%
  left_join(crosswalk_departamentos, by = c("dep_key" = "dep_key_conf")) %>%
  mutate(dep_key = coalesce(dep_key_ubi, dep_key)) %>%
  select(-dep_key_ubi) %>%
  left_join(crosswalk_provincias, by = c("dep_key", "prov_key" = "prov_key_conf")) %>%
  mutate(prov_key = coalesce(prov_key_ubi, prov_key)) %>%
  select(-prov_key_ubi) %>%
  left_join(crosswalk_distritos_conf, by = c("dep_key", "prov_key", "dist_key" = "dist_key_conf")) %>%
  mutate(dist_key = coalesce(dist_key_ubi, dist_key)) %>%
  select(-dist_key_ubi)

# Tablas de UBIGEO por nivel geográfico
ubigeo_departamento <- ubigeo %>%
  distinct(dep_key, .keep_all = TRUE) %>%
  select(dep_key, UBIGEO_DEPARTAMENTO = DEPARTAMENTO)

ubigeo_provincia <- ubigeo %>%
  distinct(dep_key, prov_key, .keep_all = TRUE) %>%
  mutate(UBIGEO_PROVINCIA = paste0(DEPARTAMENTO, PROVINCIA)) %>%
  select(dep_key, prov_key, UBIGEO_PROVINCIA)

ubigeo_distrito <- ubigeo %>%
  select(dep_key, prov_key, dist_key, UBIGEO_DISTRITO = UBIGEO)

conflictos_consolidado <- conflictos_consolidado %>%
  left_join(ubigeo_departamento, by = "dep_key") %>%
  left_join(ubigeo_provincia, by = c("dep_key", "prov_key")) %>%
  left_join(ubigeo_distrito, by = c("dep_key", "prov_key", "dist_key")) %>%
  select(-dep_key, -prov_key, -dist_key)

# Diagnóstico: casos sin UBIGEO asignado en cada nivel (revisar/ampliar los
# crosswalks de arriba si aparecen variantes nuevas al sumar más meses)
cat("\nCasos sin UBIGEO_DEPARTAMENTO:", sum(is.na(conflictos_consolidado$UBIGEO_DEPARTAMENTO)), "\n")
cat("Casos sin UBIGEO_PROVINCIA (excluye Provincia vacía en el KML):",
    sum(is.na(conflictos_consolidado$UBIGEO_PROVINCIA) & !is.na(conflictos_consolidado$PROVINCIA)), "\n")
cat("Casos sin UBIGEO_DISTRITO (excluye Distrito vacío en el KML):",
    sum(is.na(conflictos_consolidado$UBIGEO_DISTRITO) & !is.na(conflictos_consolidado$DISTRITO)), "\n")

sin_ubigeo_distrito <- conflictos_consolidado %>%
  filter(is.na(UBIGEO_DISTRITO) & !is.na(DISTRITO)) %>%
  distinct(DEPARTAMENTO, PROVINCIA, DISTRITO)

if (nrow(sin_ubigeo_distrito) > 0) {
  cat("Distritos sin UBIGEO (revisar: puede ser variante de nombre nueva o\n")
  cat("la Provincia indicada en el reporte no es la oficial de ese distrito):\n")
  
  sin_ubigeo_distrito <- sin_ubigeo_distrito %>%
    mutate(dep_key = normalizar(DEPARTAMENTO), dist_key = normalizar(DISTRITO)) %>%
    left_join(crosswalk_departamentos, by = c("dep_key" = "dep_key_conf")) %>%
    mutate(dep_key = coalesce(dep_key_ubi, dep_key)) %>%
    select(-dep_key_ubi) %>%
    left_join(
      ubigeo %>% distinct(dep_key, dist_key, .keep_all = TRUE) %>%
        select(dep_key, dist_key, PROVINCIA_SUGERIDA = PROVINCIA_NOMBRE, UBIGEO_SUGERIDO = UBIGEO),
      by = c("dep_key", "dist_key")
    ) %>%
    select(-dep_key, -dist_key)
  
  print(sin_ubigeo_distrito)
}

# ---------------------------------------------------------------------------
# Orden final de columnas
# ---------------------------------------------------------------------------
cols_orden <- c(
  "ANIO", "MES",
  "CASO",
  "ESTADO", "TIPO", "ACTIVIDAD", "FASE_CASO_ACTIVO",
  "EMPRESA",
  "MOMENTO_DIALOGO", "MECANISMO_DIALOGO",
  "HECHO_VIOLENCIA", "PARTICIPACION_DP_DIALOGO", "DIALOGO_POST_CRISIS",
  "COMPETENCIA_GOBIERNO", "PRESENCIA_DP",
  "MULTIRREGION_NACIONAL",
  "PAIS",
  "DEPARTAMENTO", "UBIGEO_DEPARTAMENTO",
  "PROVINCIA", "UBIGEO_PROVINCIA",
  "DISTRITO", "UBIGEO_DISTRITO",
  "LOCALIDAD", "OTROS_LUGARES", "DIRECCION",
  "ARCHIVO_ORIGEN"
)

conflictos_consolidado <- conflictos_consolidado %>%
  select(any_of(cols_orden), everything()) %>%
  arrange(CASO, ANIO, MES)

# ---------------------------------------------------------------------------
# Exportar
# ---------------------------------------------------------------------------
dir.create(dirname(archivo_conflictos_rds), recursive = TRUE, showWarnings = FALSE)
saveRDS(conflictos_consolidado, archivo_conflictos_rds)

cat("\nArchivo generado:", as.character(archivo_conflictos_rds), "\n")
cat("Dimensiones finales:", nrow(conflictos_consolidado), "filas x", ncol(conflictos_consolidado), "columnas\n")


