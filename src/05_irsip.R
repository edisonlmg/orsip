# ==============================================================================
# IRSIP — Índice de Riesgo Social de la Inversión Pública
#
# Nivel: distrito-mes. Calcula el índice para el último mes con datos completos
#        en las cuatro fuentes, con la escala anclada al histórico del panel.
#
# Pesos derivados de los t-estadísticos de la Tabla 1, Especificación 3
# ("All Types of Social Protests") del modelo binomial negativo:
#
#   rezago de conflictos (t-1)       t = 11.0   ->  +0.611
#   ejecución de canon               t = -3.0   ->  -0.167   (protector)
#   canon per cápita                 t =  2.0   ->  +0.111
#   producción minera per cápita     t =  2.0   ->  +0.111
#
# Fuentes esperadas en CFG$dir_in:
#   gasto_canon_minero.rds                  1892 distritos x 2019-01..2026-12
#   data_conflictos.rds                     casos Defensoría del Pueblo, caso-mes
#   valor_produccion_distrito_mensual.rds   176 distritos mineros, mineral-mes
#   data_poblacion.rds                      ubigeo-año
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

options(dplyr.summarise.inform = FALSE)

# ==============================================================================
# CONFIGURACIÓN
# ==============================================================================

CFG <- list(
  
  dir_in  = "data/processed",
  dir_out = "data/outputs",
  
  archivos = list(
    gasto      = "gasto_canon_minero.rds",
    conflictos = "data_conflictos.rds",
    mineria    = "valor_produccion_distrito_mensual.rds",
    poblacion  = "data_poblacion.rds",
    ubigeos    = "data/interim/ubigeos_2025.csv"
  ),
  
  # --- Definición de "conflicto" ---------------------------------------------
  # ESTADO a contar. Los 'Latente' son casos sin acciones en curso; incluirlos
  # casi duplica el conteo. c("Activo","Reactivado") es la lectura restrictiva.
  conflictos_estados = c("Activo", "Reactivado"),
  
  # TIPO a contar. NULL = todos (equivale al "All Types of Social Protests" del
  # paper). Para acotar: c("Socioambiental").
  conflictos_tipos   = NULL,
  
  # Los casos multirregionales/nacionales no traen UBIGEO_DISTRITO (19% de las
  # filas) y quedan fuera por construcción: el índice es distrital.
  
  # --- Agregación de flujos --------------------------------------------------
  # "12m"     -> canon y valor minero como suma móvil de 12 meses (recomendado)
  # "mensual" -> monto del mes calendario
  # El canon mensual es fuertemente estacional (2025: S/ 2 497 MM en enero,
  # S/ 39 MM en marzo, S/ 4 935 MM en junio). Con "mensual" el índice mide en
  # buena parte el calendario de transferencias, no la exposición del distrito.
  agregacion_flujos = "12m",
  
  # --- Transformación previa a la normalización -------------------------------
  # "log"    -> log1p sobre canon_pc y mineria_pc (recomendado)
  # "winsor" -> recorte al percentil CFG$winsor_p
  # "ninguna"
  # Ambas variables son extremadamente asimétricas: unos pocos distritos
  # mineros chicos concentran los montos. Con winsorización al p99, 28 distritos
  # quedan empatados exactamente en el techo y dejan de ordenarse entre sí;
  # log1p comprime la cola sin generar empates.
  transformacion = "log",
  winsor_p       = 0.99,
  
  # Rezago de conflictos: "lineal" (fiel al conteo del modelo) o "log".
  # Con "log" el salto de 0 a 1 conflicto pesa mucho más que de 4 a 5.
  transformar_rezago = "lineal",
  
  # --- Normalización de la ejecución -----------------------------------------
  # ejecucion_canon es un ratio acumulado del año: el promedio nacional va de
  # 0.013 en enero a 0.756 en diciembre. Opciones:
  # "rank"     -> percentil del distrito dentro de su propio mes (recomendado).
  #               Media 0.5 en todos los meses, así que el índice no arrastra
  #               el calendario presupuestal.
  # "mes_cal"  -> min-max contra el mismo mes calendario de otros años. Deja un
  #               desplazamiento fuerte: con estos datos la media del IRSIP cae
  #               de 30 en enero a 13 en diciembre solo por el ciclo de gasto.
  norm_ejecucion = "rank",
  
  # --- Detección del último mes completo -------------------------------------
  # Un mes cuenta como "real" en una fuente si su nivel de actividad alcanza
  # esta fracción de la mediana de los 6 meses previos. Necesario porque el
  # panel de gasto está balanceado hasta 2026-12 con ceros: la ausencia de datos
  # no aparece como NA.
  umbral_actividad = 0.85,
  periodo_forzado  = NULL,          # p.ej. "2026-02"; NULL = detectar
  
  # --- Universo --------------------------------------------------------------
  # "or"  -> canon recaudado > 0 O PIM > 0   (regla pedida)
  # "and" -> intersección
  filtro_universo = "or",
  
  # --- Escala ----------------------------------------------------------------
  # Inicio del histórico usado para fijar límites de normalización y escala.
  # NULL = todo el panel. Útil para excluir 2019-2020 si distorsionan.
  inicio_norm = NULL,
  
  # "panel"    -> ancla en el mín/máx del índice bruto de TODO el histórico.
  #               Comparable mes a mes y usa el rango completo 0–100.
  # "teorica"  -> ancla en los límites teóricos de la suma ponderada
  #               [-0.167, 0.833]. Comparable, pero comprime todo a 15–30.
  # "empirica" -> mín/máx del propio mes. El peor distrito del mes siempre
  #               marca 100, así que rompe la comparación temporal.
  escala_final = "panel",
  escala_max   = 100,
  
  exportar_serie = TRUE             # CSV con la serie mensual completa
)

PESOS <- c(
  rezago_conflictos = 0.611,
  ejecucion_canon   = -0.167,
  canon_pc          = 0.111,
  mineria_pc        = 0.111
)

VARS <- names(PESOS)

# ==============================================================================
# UTILITARIOS
# ==============================================================================

msg <- function(...) cat(sprintf(...), "\n")

leer <- function(key) {
  val <- CFG$archivos[[key]]
  f <- if (file.exists(val)) {
    val
  } else {
    file.path(CFG$dir_in, val)
  }
  if (!file.exists(f)) stop(sprintf("No existe: %s", f), call. = FALSE)
  d <- if (grepl("\\.rds$", f, ignore.case = TRUE)) {
    readRDS(f)
  } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
    read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  } else {
    stop(sprintf("Formato no soportado: %s", f), call. = FALSE)
  }
  msg("  [ok] %-11s %7d filas x %2d cols", key, nrow(d), ncol(d))
  d
}

ym <- function(s) {
  s <- str_replace_all(str_trim(as.character(s)), "/", "-")
  if (str_detect(s, "^\\d{6}$")) s <- paste0(str_sub(s, 1, 4), "-", str_sub(s, 5, 6))
  ymd(paste0(s, "-01"))
}

# Suma móvil de 12 meses sobre una serie mensual completa y ordenada.
# NA en los primeros 11 meses (ventana incompleta).
roll12 <- function(x) {
  x  <- coalesce(as.numeric(x), 0)
  cs <- cumsum(x)
  out <- cs - dplyr::lag(cs, 12L, default = 0)
  if (length(out) >= 1) out[seq_len(min(11L, length(out)))] <- NA_real_
  out
}

transformar <- function(x) {
  switch(CFG$transformacion,
         "log"     = log1p(pmax(x, 0)),
         "winsor"  = pmin(pmax(x, quantile(x, 1 - CFG$winsor_p, na.rm = TRUE)),
                          quantile(x, CFG$winsor_p, na.rm = TRUE)),
         "ninguna" = x,
         stop("CFG$transformacion inválida", call. = FALSE))
}

minmax <- function(x, ref = x) {
  lo <- suppressWarnings(min(ref, na.rm = TRUE))
  hi <- suppressWarnings(max(ref, na.rm = TRUE))
  if (!is.finite(lo) || !is.finite(hi) || hi == lo) return(rep(0, length(x)))
  (pmin(pmax(x, lo), hi) - lo) / (hi - lo)
}

# Último mes "real" de una fuente: actividad del mes vs. mediana de los 6 previos
diag_actividad <- function(tbl, umbral = CFG$umbral_actividad) {
  tbl <- tbl %>% arrange(periodo)
  a <- tbl$act
  ref <- vapply(seq_along(a), function(i) {
    j <- seq_len(i - 1); j <- j[j >= i - 6]
    if (!length(j)) return(NA_real_)
    median(a[j], na.rm = TRUE)
  }, numeric(1))
  tbl %>% mutate(ref = ref,
                 ratio = if_else(is.na(ref) | ref == 0, NA_real_, act / ref),
                 real  = is.na(ratio) | ratio >= umbral)
}
ultimo_real <- function(tbl) max(diag_actividad(tbl)$periodo[diag_actividad(tbl)$real])

# ==============================================================================
# CARGA Y ESTANDARIZACIÓN
# ==============================================================================

msg("\n[1/8] Cargando %s", CFG$dir_in)
raw <- list(gasto      = leer("gasto"),
            conflictos = leer("conflictos"),
            mineria    = leer("mineria"),
            poblacion  = leer("poblacion"),
            ubigeos    = leer("ubigeos"))

# --- Gasto y canon ------------------------------------------------------------
# ejecucion_canon viene como fracción 0–1 (= devengado_canon_acum / pim_canon,
# verificado contra los datos). Se recorta a [0,1]: hay 11 observaciones con
# sobreejecución (máx. 1.26) y una levemente negativa.
gasto <- raw$gasto %>%
  transmute(
    ubigeo          = paste0(ubigeo_dep, ubigeo_prov, ubigeo_dist),
    periodo         = make_date(anio, mes, 1L),
    monto_canon     = as.numeric(monto_canon),
    devengado_canon = as.numeric(devengado_canon),
    pim_canon       = as.numeric(pim_canon),
    ejecucion_canon = pmin(pmax(as.numeric(ejecucion_canon), 0), 1)
  ) %>%
  group_by(ubigeo, periodo) %>%
  summarise(monto_canon     = sum(monto_canon),
            devengado_canon = sum(devengado_canon),
            pim_canon       = sum(pim_canon),
            ejecucion_canon = mean(ejecucion_canon)) %>%
  ungroup()

# --- Conflictos ---------------------------------------------------------------
conf <- raw$conflictos %>% filter(!is.na(UBIGEO_DISTRITO), !is.na(ANIO), !is.na(MES))
if (!is.null(CFG$conflictos_estados))
  conf <- conf %>% filter(ESTADO %in% CFG$conflictos_estados)
if (!is.null(CFG$conflictos_tipos))
  conf <- conf %>% filter(TIPO %in% CFG$conflictos_tipos)

conflictos <- conf %>%
  transmute(ubigeo  = str_pad(as.character(UBIGEO_DISTRITO), 6, pad = "0"),
            periodo = make_date(ANIO, MES, 1L),
            caso    = CASO) %>%
  distinct() %>%                            # un caso cuenta una vez por mes
  count(ubigeo, periodo, name = "conflictos_n")

# --- Minería ------------------------------------------------------------------
# El paper usa producción física per cápita; acá se usa valor de producción en
# soles, que es el agregado disponible y sumable entre minerales.
mineria <- raw$mineria %>%
  transmute(ubigeo  = str_pad(as.character(UBIGEO), 6, pad = "0"),
            periodo = make_date(as.integer(anio), mes, 1L),
            valor   = as.numeric(valor_produccion_pen)) %>%
  group_by(ubigeo, periodo) %>%
  summarise(valor_mineria = sum(valor, na.rm = TRUE)) %>%
  ungroup()

# --- Población ----------------------------------------------------------------
poblacion <- raw$poblacion %>%
  transmute(ubigeo    = str_pad(as.character(UBIGEO), 6, pad = "0"),
            anio      = as.integer(ANIO),
            poblacion = as.numeric(POBLACION)) %>%
  filter(!is.na(poblacion), poblacion > 0) %>%
  distinct(ubigeo, anio, .keep_all = TRUE)

# --- Catálogo de nombres ------------------------------------------------------
# Se toma como fuente principal el catálogo oficial de ubigeos (ubigeos_2025.csv),
# cubriendo el 100% de los 1892 distritos. Se complementa con minería y conflictos
# como respaldo.
nombres_oficial <- raw$ubigeos %>%
  transmute(
    ubigeo       = str_pad(as.character(UBIGEO), 6, pad = "0"),
    departamento = str_trim(DEPARTAMENTO_NOMBRE),
    provincia    = str_trim(PROVINCIA_NOMBRE),
    distrito     = str_trim(DISTRITO_NOMBRE)
  )

nombres_extra <- bind_rows(
  raw$mineria %>% transmute(ubigeo = str_pad(as.character(UBIGEO), 6, pad = "0"),
                            departamento = DEPARTAMENTO, provincia = PROVINCIA,
                            distrito = DISTRITO),
  raw$conflictos %>% filter(!is.na(UBIGEO_DISTRITO)) %>%
    transmute(ubigeo = str_pad(as.character(UBIGEO_DISTRITO), 6, pad = "0"),
              departamento = DEPARTAMENTO, provincia = PROVINCIA, distrito = DISTRITO)
) %>%
  filter(!is.na(distrito)) %>%
  mutate(across(c(departamento, provincia, distrito), ~ str_to_title(str_trim(.x))))

nombres <- bind_rows(nombres_oficial, nombres_extra) %>%
  filter(!is.na(distrito), distrito != "") %>%
  distinct(ubigeo, .keep_all = TRUE)

# ==============================================================================
# DISPONIBILIDAD: ÚLTIMO MES REAL POR FUENTE
# ==============================================================================

msg("\n[2/8] Evaluando disponibilidad por fuente")

# Gasto: se exigen las dos señales. El devengado tiene cobertura estable hasta
# el mes en curso, pero las transferencias de canon se cargan con rezago, así
# que el número de distritos con canon > 0 cae en los últimos meses.
act <- gasto %>% group_by(periodo) %>%
  summarise(n_dev = sum(devengado_canon != 0), n_canon = sum(monto_canon > 0))

u_gasto <- min(ultimo_real(act %>% transmute(periodo, act = n_dev)),
               ultimo_real(act %>% transmute(periodo, act = n_canon)))
u_conf  <- ultimo_real(conflictos %>% group_by(periodo) %>%
                         summarise(act = n_distinct(ubigeo)))
u_min   <- ultimo_real(mineria %>% group_by(periodo) %>%
                         summarise(act = sum(valor_mineria > 0, na.rm = TRUE)))
u_pob   <- make_date(max(poblacion$anio), 12L, 1L)

# El rezago de conflictos es t-1: con conflictos hasta u_conf, el índice se
# puede calcular hasta u_conf + 1 mes.
usables <- c(u_gasto, u_conf %m+% months(1), u_min, u_pob)
disponibilidad <- tibble(
  fuente       = c("gasto (devengado + canon)", "conflictos (rezago t-1)",
                   "minería", "población"),
  ultimo_dato  = format(c(u_gasto, u_conf, u_min, u_pob), "%Y-%m"),
  usable_hasta = format(usables, "%Y-%m")
)
print(as.data.frame(disponibilidad), row.names = FALSE)

if (!is.null(CFG$periodo_forzado)) {
  periodo_ref <- ym(CFG$periodo_forzado)
  msg("      Periodo forzado por configuración: %s", format(periodo_ref, "%Y-%m"))
} else {
  periodo_ref <- min(usables)
  msg("      Último mes con datos completos: %s  (lo limita: %s)",
      format(periodo_ref, "%Y-%m"), disponibilidad$fuente[which.min(usables)])
}

# ==============================================================================
# 5. PANEL DISTRITO-MES
# ==============================================================================

msg("\n[3/8] Armando panel distrito-mes")

panel <- gasto %>%
  filter(periodo <= periodo_ref) %>%
  complete(ubigeo, periodo = seq(min(periodo), max(periodo), by = "month")) %>%
  left_join(conflictos, by = c("ubigeo", "periodo")) %>%
  left_join(mineria,    by = c("ubigeo", "periodo")) %>%
  mutate(
    # Ausencia de registro = ausencia del fenómeno, no dato faltante: la
    # Defensoría solo lista distritos con casos (286 de 1892) y el padrón minero
    # solo distritos con producción (176 de 1892).
    conflictos_n  = coalesce(conflictos_n, 0),
    valor_mineria = coalesce(valor_mineria, 0),
    anio          = year(periodo)
  ) %>%
  left_join(poblacion, by = c("ubigeo", "anio")) %>%
  arrange(ubigeo, periodo)

msg("      %d distritos x %d meses (%s a %s)",
    n_distinct(panel$ubigeo), n_distinct(panel$periodo),
    format(min(panel$periodo), "%Y-%m"), format(max(panel$periodo), "%Y-%m"))

panel <- panel %>%
  group_by(ubigeo) %>%
  mutate(
    rezago_conflictos = lag(conflictos_n, 1L),
    canon_12m         = roll12(monto_canon),
    mineria_12m       = roll12(valor_mineria)
  ) %>%
  ungroup() %>%
  mutate(
    canon_flujo   = if (CFG$agregacion_flujos == "12m") canon_12m   else monto_canon,
    mineria_flujo = if (CFG$agregacion_flujos == "12m") mineria_12m else valor_mineria,
    # El canon devuelto (regularizaciones negativas) se trunca en 0: un canon
    # negativo no representa menor exposición al riesgo.
    canon_pc      = if_else(!is.na(poblacion), pmax(canon_flujo, 0)   / poblacion, NA_real_),
    mineria_pc    = if_else(!is.na(poblacion), pmax(mineria_flujo, 0) / poblacion, NA_real_),
    ejecucion_pct = 100 * ejecucion_canon
  )

# ==============================================================================
# UNIVERSO
# ==============================================================================

msg("\n[4/8] Filtro de universo: canon recaudado > 0 %s PIM > 0",
    toupper(CFG$filtro_universo))

panel <- panel %>%
  mutate(
    canon_ref   = if (CFG$agregacion_flujos == "12m") canon_12m else monto_canon,
    tiene_canon = coalesce(canon_ref, 0) > 0,
    tiene_pim   = coalesce(pim_canon, 0) > 0,
    en_universo = if (CFG$filtro_universo == "and") tiene_canon & tiene_pim
    else tiene_canon | tiene_pim
  )

universo <- panel %>% filter(en_universo)
if (!is.null(CFG$inicio_norm))
  universo <- universo %>% filter(periodo >= ym(CFG$inicio_norm))

corte_panel <- panel %>% filter(periodo == periodo_ref)
msg("      %d de %d distritos del mes de corte entran al universo",
    sum(corte_panel$en_universo), nrow(corte_panel))
msg("      (con canon: %d | con PIM: %d)",
    sum(corte_panel$tiene_canon), sum(corte_panel$tiene_pim))

# ==============================================================================
# NORMALIZACIÓN, PONDERACIÓN E ÍNDICE
# ==============================================================================

msg("\n[5/8] Normalizando y ponderando sobre el histórico del universo")

# Se normaliza y se calcula el índice bruto para TODO el panel del universo,
# no solo para el mes de corte: así los límites y la escala quedan fijos y el
# índice de distintos meses es comparable.
scored <- universo %>%
  mutate(t_rezago  = if (CFG$transformar_rezago == "log") log1p(rezago_conflictos)
         else rezago_conflictos,
         t_canon   = transformar(canon_pc),
         t_mineria = transformar(mineria_pc)) %>%
  mutate(
    n_rezago_conflictos = minmax(t_rezago),
    n_canon_pc          = minmax(t_canon),
    n_mineria_pc        = minmax(t_mineria)
  ) %>%
  # ejecucion_canon: ver CFG$norm_ejecucion. Con "rank" el valor normalizado es
  # la posición relativa del distrito entre sus pares del mismo mes, así que la
  # variable mide desempeño comparado y no el punto del año fiscal.
  group_by(grupo_ejec = if (CFG$norm_ejecucion == "rank") as.integer(periodo)
           else month(periodo)) %>%
  mutate(n_ejecucion_canon = if (CFG$norm_ejecucion == "rank")
    percent_rank(ejecucion_canon)
    else minmax(ejecucion_canon)) %>%
  ungroup() %>%
  select(-grupo_ejec) %>%
  mutate(
    contrib_rezago    = PESOS[["rezago_conflictos"]] * n_rezago_conflictos,
    contrib_ejecucion = PESOS[["ejecucion_canon"]]   * n_ejecucion_canon,
    contrib_canon     = PESOS[["canon_pc"]]          * n_canon_pc,
    contrib_mineria   = PESOS[["mineria_pc"]]        * n_mineria_pc,
    irsip_bruto = contrib_rezago + contrib_ejecucion + contrib_canon + contrib_mineria
  )

limites <- tibble(
  variable    = VARS,
  transf      = c(CFG$transformar_rezago, "ninguna",
                  CFG$transformacion, CFG$transformacion),
  ventana     = c("panel",
                  if (CFG$norm_ejecucion == "rank") "rank intra-mes" else "mes calendario",
                  "panel", "panel"),
  min_original = c(min(scored$rezago_conflictos, na.rm = TRUE),
                   min(scored$ejecucion_canon,   na.rm = TRUE),
                   min(scored$canon_pc,          na.rm = TRUE),
                   min(scored$mineria_pc,        na.rm = TRUE)),
  max_original = c(max(scored$rezago_conflictos, na.rm = TRUE),
                   max(scored$ejecucion_canon,   na.rm = TRUE),
                   max(scored$canon_pc,          na.rm = TRUE),
                   max(scored$mineria_pc,        na.rm = TRUE))
)
msg("      Límites de normalización:")
print(as.data.frame(limites), row.names = FALSE, digits = 5)

# --- Ancla de escala ----------------------------------------------------------
if (CFG$escala_final == "teorica") {
  lo <- sum(PESOS[PESOS < 0]); hi <- sum(PESOS[PESOS > 0])
} else if (CFG$escala_final == "panel") {
  lo <- min(scored$irsip_bruto, na.rm = TRUE)
  hi <- max(scored$irsip_bruto, na.rm = TRUE)
} else {
  b  <- scored$irsip_bruto[scored$periodo == periodo_ref]
  lo <- min(b, na.rm = TRUE); hi <- max(b, na.rm = TRUE)
}
msg("      Ancla de escala (%s): bruto [%.4f, %.4f] -> [0, %d]",
    CFG$escala_final, lo, hi, CFG$escala_max)

scored <- scored %>%
  mutate(irsip = round(pmin(pmax(CFG$escala_max * (irsip_bruto - lo) / (hi - lo), 0),
                            CFG$escala_max), 2))

# --- Mes de corte -------------------------------------------------------------
corte <- scored %>% filter(periodo == periodo_ref)

excluidos <- corte %>% filter(!complete.cases(across(all_of(VARS))))
corte     <- corte %>% filter(complete.cases(across(all_of(VARS))))
if (nrow(excluidos) > 0)
  msg("      %d distritos excluidos por NA en variables del índice (ver CSV)",
      nrow(excluidos))

corte <- corte %>%
  arrange(desc(irsip)) %>%
  mutate(ranking   = row_number(),
         percentil = round(100 * (1 - (ranking - 0.5) / n()), 1),
         nivel = cut(irsip, breaks = c(-Inf, 20, 40, 60, 80, Inf),
                     labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))) %>%
  left_join(nombres, by = "ubigeo")

# ==============================================================================
# SALIDA
# ==============================================================================

msg("\n[6/8] Escribiendo resultados")

resultado <- corte %>%
  transmute(
    ubigeo, periodo, departamento, provincia, distrito,
    ranking, irsip, nivel, percentil,
    # variables en escala original
    rezago_conflictos, ejecucion_pct, canon_pc, mineria_pc,
    monto_canon, canon_12m, pim_canon, devengado_canon, mineria_12m, poblacion,
    # normalizadas
    n_rezago_conflictos, n_ejecucion_canon, n_canon_pc, n_mineria_pc,
    # contribuciones en puntos del índice bruto
    contrib_rezago, contrib_ejecucion, contrib_canon, contrib_mineria,
    irsip_bruto
  )

dir.create(CFG$dir_out, recursive = TRUE, showWarnings = FALSE)
etq <- format(periodo_ref, "%Y%m")

write_csv(resultado,      file.path(CFG$dir_out, sprintf("irsip_%s.csv", etq)))
write_csv(disponibilidad, file.path(CFG$dir_out, sprintf("irsip_%s_disponibilidad.csv", etq)))
write_csv(limites,        file.path(CFG$dir_out, sprintf("irsip_%s_limites.csv", etq)))
if (nrow(excluidos) > 0)
  write_csv(excluidos %>% select(ubigeo, periodo, all_of(VARS), poblacion, pim_canon),
            file.path(CFG$dir_out, sprintf("irsip_%s_excluidos.csv", etq)))
if (isTRUE(CFG$exportar_serie))
  write_csv(scored %>% select(ubigeo, periodo, irsip, irsip_bruto, starts_with("contrib_")),
            file.path(CFG$dir_out, sprintf("irsip_serie_mensual_%s.csv", etq)))

msg("      %s", file.path(CFG$dir_out, sprintf("irsip_%s.csv", etq)))

# ==============================================================================
# DIAGNÓSTICO
# ==============================================================================

msg("\n[7/8] Diagnóstico")
msg("\n== IRSIP %s — %d distritos ==", format(periodo_ref, "%Y-%m"), nrow(resultado))
msg("   media %.1f | sd %.1f | mediana %.1f | p90 %.1f | p99 %.1f | máx %.1f",
    mean(resultado$irsip), sd(resultado$irsip), median(resultado$irsip),
    quantile(resultado$irsip, .90), quantile(resultado$irsip, .99),
    max(resultado$irsip))

msg("\n-- Cobertura de las variables en el mes de corte --")
print(resultado %>%
        summarise(`rezago > 0`   = sprintf("%d (%.1f%%)", sum(rezago_conflictos > 0),
                                           100 * mean(rezago_conflictos > 0)),
                  `minería > 0`  = sprintf("%d (%.1f%%)", sum(mineria_pc > 0),
                                           100 * mean(mineria_pc > 0)),
                  `canon pc > 0` = sprintf("%d (%.1f%%)", sum(canon_pc > 0),
                                           100 * mean(canon_pc > 0))) %>%
        as.data.frame(), row.names = FALSE)

msg("\n-- Aporte medio de cada variable al índice bruto --")
print(resultado %>%
        summarise(across(starts_with("contrib_"), mean)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "aporte_medio") %>%
        mutate(share = sprintf("%.1f%%", 100 * abs(aporte_medio) / sum(abs(aporte_medio)))) %>%
        as.data.frame(), row.names = FALSE, digits = 4)

msg("\n-- Top 20 riesgo --")
print(resultado %>%
        select(ranking, ubigeo, distrito, provincia, departamento, irsip, nivel,
               rezago_conflictos, ejecucion_pct, canon_pc) %>%
        head(20) %>% as.data.frame(), row.names = FALSE, digits = 4)

msg("\n-- Distribución por nivel --")
print(resultado %>% count(nivel, .drop = FALSE) %>% as.data.frame(), row.names = FALSE)

msg("\n-- Estabilidad de la escala: media del IRSIP por mes (últimos 14) --")
print(scored %>%
        group_by(mes = format(periodo, "%Y-%m")) %>%
        summarise(n = n(), media = round(mean(irsip, na.rm = TRUE), 1)) %>%
        tail(14) %>% as.data.frame(), row.names = FALSE)
msg("   Si la media se desplaza sistemáticamente dentro del año, revisar")
msg("   CFG$norm_ejecucion y CFG$agregacion_flujos.")

msg("\n[8/8] Listo.")

# ==============================================================================
# NOTAS DE IMPLEMENTACIÓN
#
# 1. Mes de corte. El panel de gasto está balanceado con ceros hasta 2026-12, de
#    modo que un chequeo de NA daría diciembre 2026 como "completo". Por eso la
#    detección compara actividad contra la mediana de los 6 meses previos. Con
#    los datos actuales el límite lo pone la fuente de conflictos, que termina
#    en 2026-01: el índice se calcula para 2026-02, aunque gasto llegue a
#    2026-06 y minería a 2026-05.
#
# 2. Rezago de conflictos. Solo 286 de los 1892 distritos aparecen alguna vez en
#    el registro de la Defensoría, y ~140 por mes. El rezago vale 0 para más del
#    90% del universo, así que la variable con 61% del peso no discrimina dentro
#    de ese grupo: entre distritos sin historial de conflicto el ranking lo
#    definen ejecución, canon y minería. Es una propiedad del índice heredada de
#    los pesos del paper, no un error de cálculo, pero conviene decirlo al
#    presentar resultados.
#
# 3. Filtro de universo. Con estos datos casi no acota: en 2026-02, 1889 de 1892
#    distritos tienen PIM > 0 y 1865 tienen canon > 0. Sirve como salvaguarda
#    para meses futuros, no como recorte efectivo.
#
# 4. Ejecución de canon y calendario fiscal. Es un acumulado del año, así que
#    normalizarla contra el histórico convierte a la variable en un efecto
#    mes-del-año: la media del IRSIP caía de 30 en enero a 13 en diciembre solo
#    por el ciclo de gasto. Con el percentil intra-mes ese desplazamiento
#    desaparece (la desviación estándar de la media mensual pasa de 5.3 a 0.2).
#
# 5. Nombres de distrito. Se cargan desde el catálogo oficial de ubigeos
#    (ubigeos_2025.csv), garantizando cobertura completa de departamento,
#    provincia y distrito para los 1892 distritos del país.
#
# 6. Población: falta el ubigeo 160405, que queda sin canon_pc ni mineria_pc y
#    sale al archivo de excluidos.
#
# 7. Producción minera: el dataset trae valor en soles, no producción física
#    como el modelo original. Con precios variables el valor mezcla volumen y
#    precio; para apegarse al paper habría que usar
#    produccion_distrito_mensual, que no es sumable entre minerales.
# ==============================================================================