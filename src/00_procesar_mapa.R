# -----------------------------------------------------------------------
# 00_load_shapefiles.R (Consolidado total)
#
# Genera las geometrías procesadas y la tabla de ubigeos.
# Lee los shapefiles crudos de data/raw/mapa/ y escribe únicamente
# en app/data/.
# -----------------------------------------------------------------------

library(dplyr)
library(sf)

# =======================================================================
# FUNCIONES DE NORMALIZACIÓN DE TEXTO Y FECHAS
# =======================================================================

#' Limpia espacios: convierte el espacio no-separable (U+00A0) y otros
#' espacios Unicode en espacio normal, colapsa repeticiones y recorta bordes.
norm_txt <- function(x) {
  x <- stringi::stri_replace_all_regex(x, "[\\p{Zs}\\t\\r\\n]+", " ")
  stringi::stri_trim_both(x)
}

# Clave de unión: mayúsculas, sin tildes ni diacríticos, espacios colapsados.
# Determinista y estable entre plataformas.
norm_key <- function(x) {
  x <- norm_txt(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringi::stri_trans_toupper(x)
  # deja solo letras, dígitos y espacios (elimina puntos, guiones, comillas)
  x <- stringi::stri_replace_all_regex(x, "[^A-Z0-9 ]", " ")
  x <- stringi::stri_replace_all_regex(x, " +", " ")
  x <- stringi::stri_trim_both(x)
  dplyr::if_else(x == "" | is.na(x), NA_character_, x)
}

# Mapea nombres de mes en español a número. Acepta mayúsculas, tildes,
# "setiembre"/"septiembre" y abreviaturas de tres letras.
mes_a_numero <- function(x, strict = TRUE) {
  k <- norm_key(x)
  tabla <- c(
    ENERO = 1L, FEBRERO = 2L, MARZO = 3L, ABRIL = 4L, MAYO = 5L,
    JUNIO = 6L, JULIO = 7L, AGOSTO = 8L, SEPTIEMBRE = 9L, SETIEMBRE = 9L,
    OCTUBRE = 10L, NOVIEMBRE = 11L, DICIEMBRE = 12L,
    ENE = 1L, FEB = 2L, MAR = 3L, ABR = 4L, MAY = 5L, JUN = 6L,
    JUL = 7L, AGO = 8L, SET = 9L, SEP = 9L, OCT = 10L, NOV = 11L, DIC = 12L
  )
  out <- unname(tabla[k])
  malos <- unique(x[is.na(out) & !is.na(x)])
  if (length(malos) > 0) {
    msg <- paste0(
      "Valores de mes no reconocidos: ",
      paste(sprintf('"%s"', utils::head(malos, 10)), collapse = ", "),
      if (length(malos) > 10) sprintf(" (y %d más)", length(malos) - 10) else ""
    )
    if (strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  as.integer(out)
}

# --- Índice mensual continuo -------------------------------------------
periodo_desde <- function(anio, mes) as.integer(anio) * 12L + as.integer(mes)

periodo_a_fecha <- function(periodo) {
  periodo <- as.integer(periodo)
  mes  <- periodo %% 12L
  anio <- periodo %/% 12L
  anio[mes == 0L] <- anio[mes == 0L] - 1L
  mes[mes == 0L]  <- 12L
  tibble::tibble(anio = anio, mes = mes, periodo = periodo)
}

# =======================================================================
# CARGA Y PROCESAMIENTO DE GEOMETRÍAS
# =======================================================================

#' Carga y procesa las tres capas administrativas.
#'
#' @param dir_raw Carpeta con las subcarpetas departamentos/, provincias/ y distritos/.
#' @param destinos Carpeta donde guardar los .rds (únicamente app/data).
#' @param keep Proporción de vértices a conservar por capa.
#' @param simplificar Si FALSE, omite ms_simplify.
#' @param encoding Codificación del .dbf.
#' @return Lista con las tres capas (invisible).
cargar_shapefiles <- function(dir_raw = "data/raw/mapa",
                              destinos = c("app/data", "data/processed"),
                              keep = c(departamental = 0.05, provincial = 0.05,
                                       distrital = 0.02),
                              simplificar = TRUE,
                              encoding = "LATIN1") {
  
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Se requiere el paquete sf.", call. = FALSE)
  }
  
  rutas <- c(
    departamental = file.path(dir_raw, "departamentos", "v_departamentos_2023.shp"),
    provincial    = file.path(dir_raw, "provincias",    "v_provincias_2023.shp"),
    distrital     = file.path(dir_raw, "distritos",     "v_distritos_2023.shp")
  )
  falta <- rutas[!file.exists(rutas)]
  if (length(falta) > 0L) {
    stop("No se encontraron los shapefiles:\n  ", paste(falta, collapse = "\n  "),
         "\nVer la sección 'Datos geoespaciales' del README.", call. = FALSE)
  }
  
  opciones <- if (is.null(encoding)) character(0) else paste0("ENCODING=", encoding)
  capas <- lapply(rutas, function(p) sf::st_read(p, quiet = TRUE, options = opciones))
  
  # --- Validación de codificación ----------------------------------------
  cols_nombre <- c("nombdep", "nombprov", "nombdist")
  corruptos <- unlist(lapply(names(capas), function(nm) {
    x <- capas[[nm]]
    cols <- intersect(cols_nombre, names(x))
    unlist(lapply(cols, function(cl) {
      v <- as.character(x[[cl]])
      malos <- v[!is.na(v) &
                   (stringi::stri_detect_fixed(v, "\ufffd") |
                      !stringi::stri_enc_isutf8(v))]
      if (length(malos) > 0) sprintf("%s$%s: %s", nm, cl,
                                     paste(unique(malos), collapse = ", "))
    }))
  }))
  
  if (length(corruptos) > 0L) {
    message("\nNOMBRES CON CODIFICACIÓN CORRUPTA:")
    message(paste(" -", corruptos, collapse = "\n"))
    stop("El .dbf no se está leyendo con la codificación correcta. Probar ",
         "encoding = ", if (identical(encoding, "LATIN1")) '"UTF-8"' else '"LATIN1"',
         ", o encoding = NULL. Nada de lo que sigue funciona con nombres rotos.",
         call. = FALSE)
  }
  
  message(sprintf("Leídas: %s",
                  paste(sprintf("%s (%d)", names(capas), vapply(capas, nrow, integer(1))),
                        collapse = ", ")))
  
  if (simplificar) {
    if (!requireNamespace("rmapshaper", quietly = TRUE)) {
      stop("Se requiere rmapshaper para simplificar. Usar simplificar = FALSE ",
           "para omitir este paso.", call. = FALSE)
    }
    for (nm in names(capas)) {
      capas[[nm]] <- rmapshaper::ms_simplify(capas[[nm]], keep = keep[[nm]],
                                             keep_shapes = TRUE)
    }
  }
  
  # --- Normalización de nombres y ubigeos --------------------------------
  ub <- function(dd, pp = NULL, len = 2L) {
    dd <- stringi::stri_pad_left(stringi::stri_replace_all_regex(as.character(dd), "\\D", ""), 2, "0")
    if (is.null(pp)) return(dd)
    pp <- stringi::stri_replace_all_regex(as.character(pp), "\\D", "")
    dplyr::if_else(stringi::stri_length(pp) >= len,
                   stringi::stri_pad_left(pp, len, "0"),
                   paste0(dd, stringi::stri_pad_left(pp, len - 2L, "0")))
  }
  
  capas$departamental <- capas$departamental |>
    dplyr::mutate(nombdep_norm = norm_key(nombdep),
                  ubigeo_dep   = ub(iddpto))
  
  capas$provincial <- capas$provincial |>
    dplyr::mutate(nombdep_norm  = norm_key(nombdep),
                  nombprov_norm = norm_key(nombprov),
                  ubigeo_dep    = ub(iddpto),
                  ubigeo_prov   = ub(iddpto, idprov, 4L))
  
  capas$distrital <- capas$distrital |>
    dplyr::mutate(nombdep_norm  = norm_key(nombdep),
                  nombprov_norm = norm_key(nombprov),
                  nombdist_norm = norm_key(nombdist),
                  ubigeo_dist   = stringi::stri_pad_left(
                    stringi::stri_replace_all_regex(as.character(ubigeo), "\\D", ""), 6, "0"),
                  ubigeo_prov   = stringi::stri_sub(ubigeo_dist, 1, 4),
                  ubigeo_dep    = stringi::stri_sub(ubigeo_dist, 1, 2))
  
  # --- Validación: la capa provincial es la tabla canónica de ubigeos -----
  if (file.exists("R/geo_ubigeo.R")) {
    source("R/geo_ubigeo.R")
    tabla <- construir_tabla_ubigeo(capas$provincial)
    message(sprintf("Validación de ubigeos provinciales: %d códigos únicos.", nrow(tabla)))
  }
  
  # --- Guardado -----------------------------------------------------------
  for (d in destinos) {
    if (!dir.exists(d)) dir.create(d, recursive = TRUE)
    saveRDS(capas$departamental, file.path(d, "geo_departamental.rds"))
    saveRDS(capas$provincial,    file.path(d, "geo_provincial.rds"))
    saveRDS(capas$distrital,     file.path(d, "geo_distrital.rds"))
    message("Guardado exitosamente en ", d, "/")
  }
  
  invisible(capas)
}

# =======================================================================
# EJECUCIÓN PRINCIPAL
# =======================================================================
capas <- cargar_shapefiles(
  dir_raw = "data/raw/mapa",
  destinos = c("app/data", "data/processed")
)


