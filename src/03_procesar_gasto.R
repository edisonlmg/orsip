# ---------------------------------------------------------------------------
# 00_construir_panel_canon_minero.R
#
# Panel mensual por gobierno local y UBIGEO.
#
# INDICADORES:
#
# monto_canon:
#   Recaudado mensual, Rubro 18, tipo de recurso MINERO/MINERA,
#   excluyendo Saldo de Balance (Genérica 9).
#
# monto_canon_acum:
#   Recaudado acumulado desde enero hasta el mes correspondiente.
#
# ejecucion_canon:
#   Devengado acumulado / PIM
#   ambos para Rubro 18 y tipo de recurso MINERO/MINERA.
#
# Se procesa un año a la vez: se carga el año completo con vroom,
# se filtra/agrega con dplyr, y se descarta el data frame grande
# antes de pasar al siguiente año.
# ---------------------------------------------------------------------------

library(vroom)
library(dplyr)
library(tidyr)
library(stringr)
library(stringi)

# ---------------------------------------------------------------------------
# CONFIGURACIÓN
# ---------------------------------------------------------------------------

DIR_GASTO   <- "data/raw/gasto"
DIR_INGRESO <- "data/raw/ingreso"

SALIDA_RDS <- "data/processed/gasto_canon_minero.rds"

ANIOS <- 2019:2026

NIVEL_GOBIERNO <- "M"
RUBRO_CANON <- "18"

MESES <- c(
  "ENERO", "FEBRERO", "MARZO", "ABRIL",
  "MAYO", "JUNIO", "JULIO", "AGOSTO",
  "SEPTIEMBRE", "OCTUBRE", "NOVIEMBRE", "DICIEMBRE"
)

# ---------------------------------------------------------------------------
# FUNCIONES AUXILIARES
# ---------------------------------------------------------------------------

# Normalización para buscar MINERO/MINERA en TIPO_RECURSO_NOMBRE.
norm_key <- function(x) {
  x <- as.character(x)
  x <- stri_trans_general(toupper(trimws(x)), "Latin-ASCII")
  x <- gsub("[^A-Z0-9 ]", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Códigos geográficos a texto de 2 caracteres.
codigo_2 <- function(x) {
  if (is.numeric(x)) return(sprintf("%02d", as.integer(x)))
  as.character(x)
}

# Localiza el archivo correspondiente al año (año cerrado o parcial-diario).
ruta_dataset_mef <- function(anio, dir_raw, base) {
  
  archivos <- c(
    file.path(dir_raw, sprintf("%d-%s.csv", anio, base)),
    file.path(dir_raw, sprintf("%d-%s-Diario.csv", anio, base))
  )
  
  existe <- file.exists(archivos)
  
  if (!any(existe)) {
    stop(
      sprintf(
        "No se encontró el archivo del año %d.\nSe buscó:\n%s",
        anio, paste(archivos, collapse = "\n")
      ),
      call. = FALSE
    )
  }
  
  archivos[which(existe)[1]]
}

# ---------------------------------------------------------------------------
# INGRESO RECAUDADO — un año
# ---------------------------------------------------------------------------
# Filtros: NIVEL_GOBIERNO == "M", RUBRO == "18",
#          TIPO_RECURSO_NOMBRE contiene MINERO/MINERA,
#          GENERICA != 9 (Saldo de Balance)
# Salida: anio, mes, ubigeo_dep, ubigeo_prov, ubigeo_dist, monto_canon
# ---------------------------------------------------------------------------

procesar_ingreso_anio <- function(anio) {
  
  ruta <- ruta_dataset_mef(anio, DIR_INGRESO, "Ingreso-Recaudado")
  
  message(sprintf("Ingreso %d: %s", anio, basename(ruta)))
  
  # Detectamos el nombre real de la columna de genérica (varía por año).
  encabezado <- names(vroom(ruta, n_max = 0, show_col_types = FALSE))
  
  col_generica <- intersect(
    c("GENERICA", "GENERICA_NOMBRE", "GENERICA_DET_NOMBRE"),
    encabezado
  )[1]
  
  if (is.na(col_generica)) {
    stop("No se encontró columna de Genérica en ", basename(ruta), call. = FALSE)
  }
  
  cols_monto <- paste0("MONTO_RECAUDADO_", MESES)
  
  cols <- c(
    "ANO_DOC", "NIVEL_GOBIERNO",
    "DEPARTAMENTO_EJECUTORA", "PROVINCIA_EJECUTORA", "DISTRITO_EJECUTORA",
    "RUBRO", "TIPO_RECURSO_NOMBRE",
    col_generica,
    cols_monto
  )
  
  # vroom selecciona por NOMBRE -> no hay riesgo de desalineamiento
  # de columnas por orden, a diferencia de leer por lotes sin header.
  d <- vroom(
    ruta,
    col_select = all_of(cols),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) %>%
    mutate(across(all_of(cols_monto), as.numeric))
  
  d <- d %>%
    filter(
      NIVEL_GOBIERNO == NIVEL_GOBIERNO,
      RUBRO == RUBRO_CANON,
      str_detect(norm_key(TIPO_RECURSO_NOMBRE), "MINERO|MINERA"),
      is.na(.data[[col_generica]]) | trimws(.data[[col_generica]]) != "9"
    ) %>%
    mutate(
      anio = as.integer(ANO_DOC),
      ubigeo_dep  = codigo_2(DEPARTAMENTO_EJECUTORA),
      ubigeo_prov = codigo_2(PROVINCIA_EJECUTORA),
      ubigeo_dist = codigo_2(DISTRITO_EJECUTORA)
    )
  
  resultado <- d %>%
    select(anio, ubigeo_dep, ubigeo_prov, ubigeo_dist, all_of(cols_monto)) %>%
    pivot_longer(
      cols = all_of(cols_monto),
      names_to = "mes_nombre",
      names_prefix = "MONTO_RECAUDADO_",
      values_to = "monto_canon"
    ) %>%
    mutate(mes = match(mes_nombre, MESES)) %>%
    group_by(anio, mes, ubigeo_dep, ubigeo_prov, ubigeo_dist) %>%
    summarise(monto_canon = sum(monto_canon, na.rm = TRUE), .groups = "drop")
  
  rm(d)
  gc(verbose = FALSE)
  
  resultado
}

# ---------------------------------------------------------------------------
# GASTO DEVENGADO — un año
# ---------------------------------------------------------------------------
# Filtros: NIVEL_GOBIERNO == "M", RUBRO == "18",
#          TIPO_RECURSO_NOMBRE contiene MINERO/MINERA
# Salida: devengado mensual + PIM anual, por anio/ubigeo
# ---------------------------------------------------------------------------

procesar_gasto_anio <- function(anio) {
  
  ruta <- ruta_dataset_mef(anio, DIR_GASTO, "Gasto-Devengado")
  
  message(sprintf("Gasto %d: %s", anio, basename(ruta)))
  
  cols_monto <- c("MONTO_PIM", paste0("MONTO_DEVENGADO_", MESES))
  
  cols <- c(
    "ANO_EJE", "NIVEL_GOBIERNO",
    "DEPARTAMENTO_EJECUTORA", "PROVINCIA_EJECUTORA", "DISTRITO_EJECUTORA",
    "RUBRO", "TIPO_RECURSO_NOMBRE",
    cols_monto
  )
  
  d <- vroom(
    ruta,
    col_select = all_of(cols),
    col_types = cols(.default = "c"),
    show_col_types = FALSE
  ) %>%
    mutate(across(all_of(cols_monto), as.numeric))
  
  d <- d %>%
    filter(
      NIVEL_GOBIERNO == "M",
      RUBRO == RUBRO_CANON
    ) %>%
    mutate(
      anio = as.integer(ANO_EJE),
      ubigeo_dep  = codigo_2(DEPARTAMENTO_EJECUTORA),
      ubigeo_prov = codigo_2(PROVINCIA_EJECUTORA),
      ubigeo_dist = codigo_2(DISTRITO_EJECUTORA)
    )
  
  cols_devengado <- paste0("MONTO_DEVENGADO_", MESES)
  
  devengado <- d %>%
    select(anio, ubigeo_dep, ubigeo_prov, ubigeo_dist, all_of(cols_devengado)) %>%
    pivot_longer(
      cols = all_of(cols_devengado),
      names_to = "mes_nombre",
      names_prefix = "MONTO_DEVENGADO_",
      values_to = "devengado_canon"
    ) %>%
    mutate(mes = match(mes_nombre, MESES)) %>%
    group_by(anio, mes, ubigeo_dep, ubigeo_prov, ubigeo_dist) %>%
    summarise(devengado_canon = sum(devengado_canon, na.rm = TRUE), .groups = "drop")
  
  pim <- d %>%
    group_by(anio, ubigeo_dep, ubigeo_prov, ubigeo_dist) %>%
    summarise(pim_canon = sum(MONTO_PIM, na.rm = TRUE), .groups = "drop")
  
  rm(d)
  gc(verbose = FALSE)
  
  list(devengado = devengado, pim = pim)
}

# ---------------------------------------------------------------------------
#  TODOS LOS AÑOS (uno a la vez)
# ---------------------------------------------------------------------------

message("\n========== INGRESO RECAUDADO ==========\n")

ingreso_lista <- vector("list", length(ANIOS))

for (i in seq_along(ANIOS)) {
  ingreso_lista[[i]] <- procesar_ingreso_anio(ANIOS[i])
}

ingreso <- bind_rows(ingreso_lista)
rm(ingreso_lista)
gc(verbose = FALSE)

message("\n========== GASTO DEVENGADO ==========\n")

devengado_lista <- vector("list", length(ANIOS))
pim_lista <- vector("list", length(ANIOS))

for (i in seq_along(ANIOS)) {
  res <- procesar_gasto_anio(ANIOS[i])
  devengado_lista[[i]] <- res$devengado
  pim_lista[[i]] <- res$pim
  rm(res)
}

devengado <- bind_rows(devengado_lista)
pim <- bind_rows(pim_lista)
rm(devengado_lista, pim_lista)
gc(verbose = FALSE)

# ---------------------------------------------------------------------------
# UNIR INGRESO + DEVENGADO + PIM
# ---------------------------------------------------------------------------

panel <- ingreso %>%
  full_join(
    devengado,
    by = c("anio", "mes", "ubigeo_dep", "ubigeo_prov", "ubigeo_dist")
  ) %>%
  left_join(
    pim,
    by = c("anio", "ubigeo_dep", "ubigeo_prov", "ubigeo_dist")
  ) %>%
  mutate(
    monto_canon = coalesce(monto_canon, 0),
    devengado_canon = coalesce(devengado_canon, 0),
    pim_canon = coalesce(pim_canon, 0)
  ) %>%
  arrange(ubigeo_dist, anio, mes)

# ---------------------------------------------------------------------------
# ACUMULADOS E INDICADOR DE EJECUCIÓN
# ---------------------------------------------------------------------------
# monto_canon_acum / devengado_canon_acum: acumulado enero -> mes, por año/distrito.
# ejecucion_canon: devengado acumulado / PIM (NA si PIM = 0).
# ---------------------------------------------------------------------------

panel <- panel %>%
  group_by(anio, ubigeo_dep, ubigeo_prov, ubigeo_dist) %>%
  arrange(mes, .by_group = TRUE) %>%
  mutate(
    monto_canon_acum = cumsum(monto_canon),
    devengado_canon_acum = cumsum(devengado_canon)
  ) %>%
  ungroup() %>%
  mutate(
    ejecucion_canon = if_else(
      pim_canon > 0,
      devengado_canon_acum / pim_canon,
      0
    )
  ) %>%
  select(
    anio, mes, ubigeo_dep, ubigeo_prov, ubigeo_dist,
    monto_canon, monto_canon_acum,
    devengado_canon, devengado_canon_acum,
    pim_canon, ejecucion_canon
  )

# ---------------------------------------------------------------------------
# GUARDAR
# ---------------------------------------------------------------------------

dir.create(dirname(SALIDA_RDS), showWarnings = FALSE, recursive = TRUE)

saveRDS(panel, SALIDA_RDS)

message(
  "\nListo.",
  "\nFilas: ", format(nrow(panel), big.mark = ","),
  "\nColumnas: ", ncol(panel),
  "\nArchivo: ", SALIDA_RDS
)



