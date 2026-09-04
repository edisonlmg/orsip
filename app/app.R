# =======================================================================
# ORSIP — tablero de riesgo social en la inversión pública
#
#  - La unidad es el DISTRITO-MES (Sección 2.1). El IRSIP se calcula a
#    nivel distrital; las inversiones individuales heredan el puntaje del
#    distrito donde se ejecutan (Sección 8.4).
#
#  - El puntaje SIEMPRE va con su descomposición (Sección 8.3): el hover a
#    nivel distrital muestra el IRSIP y el aporte en puntos de cada uno de
#    los cuatro componentes ponderados (Sección 2.3-2.6). Pero el COLOR del
#    polígono, en los tres niveles del mapa (distrital, provincial y
#    departamental), es el mismo semáforo de tres categorías: rojo si hay
#    alerta alta, naranja si el peor es media, amarillo en el resto (Bajo y
#    Muy bajo del IRSIP se agrupan como "Baja").
#
#  - A nivel departamental y provincial no existe un "IRSIP departamental":
#    agregar el índice geográficamente no tiene sustento en la metodología.
#    La categoría de esos niveles es la PEOR alerta que tenga algún
#    distrito adentro (basta un distrito en alerta alta para pintar todo
#    el territorio de rojo). A nivel distrital, la categoría es la propia
#    del distrito. El hover siempre muestra el desagregado detrás del
#    color (conteos a nivel prov/dep, IRSIP y su descomposición a nivel
#    distrital).
# =======================================================================

library(shiny)
library(bslib)
library(dplyr)
library(sf)
library(leaflet)
library(DT)
library(htmltools)
library(readxl)

# --- PALETA DEL PROYECTO -----------------------------------------------
teal  <- "#2D6A5E"
azul  <- "#1F3D6B"
crema <- "#F5F0EB"

theme_orsip <- bs_theme(
  version = 5,
  primary = teal,
  secondary = azul,
  bg = crema,
  fg = "#22303C",
  base_font = font_google("Montserrat")
)

# --- UTILIDADES ---------------------------------------------------------
fmt <- function(x, d = 0) {
  format(round(x, d), big.mark = ",", nsmall = d, scientific = FALSE)
}

# Normaliza un ubigeo a 6 dígitos con ceros a la izquierda.

normalizar_ubigeo <- function(x) {
  x <- trimws(as.character(x))
  ok <- grepl("^[0-9]{1,6}$", x)
  out <- rep(NA_character_, length(x))
  out[ok] <- formatC(as.integer(x[ok]), width = 6, flag = "0")
  out
}

# --- GEOMETRÍAS REALES (IGN/INEI 2023) ---------------------------------
# Se leen desde app/data/mapa/ (movidas ahí para diferenciarlas de los
# archivos de riesgo, que quedan en app/data/inundaciones/) y no desde
# ../data/processed/, para que la app funcione igual en local y en
# shinyapps.io.
geo_dep  <- readRDS("data/mapa/geo_departamental.rds")
geo_prov <- readRDS("data/mapa/geo_provincial.rds")
geo_dist <- readRDS("data/mapa/geo_distrital.rds")

# =======================================================================
# IRSIP — corte febrero 2026
# =======================================================================
datos <- read.csv("data/irsip_202602.csv",
                  colClasses = c(ubigeo = "character"),
                  fileEncoding = "UTF-8", stringsAsFactors = FALSE) |>
  mutate(
    ubigeo_prov = substr(ubigeo, 1, 4),
    ubigeo_dep  = substr(ubigeo, 1, 2),
    nivel = factor(nivel, levels = c("Muy bajo", "Bajo", "Medio", "Alto")),
    # Alerta de TRES franjas para la agregación departamental/provincial
    # (a diferencia de "nivel", que conserva las 4 franjas del IRSIP para
    # el filtro y la tabla). "Bajo" y "Muy bajo" se agrupan como "Baja".
    nivel_alerta = case_when(
      nivel == "Alto"  ~ "Alta",
      nivel == "Medio" ~ "Media",
      TRUE              ~ "Baja"
    ),
    nivel_alerta = factor(nivel_alerta, levels = c("Baja", "Media", "Alta"))
  )

# Ancla del panel histórico (Sección 2.6): fija, no se recalcula por corte
# para no romper la comparabilidad temporal de la serie.
ANCLA_MIN <- -0.167
ANCLA_MAX <- 0.547
RANGO_ANCLA <- ANCLA_MAX - ANCLA_MIN
# Piso común a todo distrito: la parte del IRSIP que no depende de sus
# variables sino sólo de dónde queda fijada el ancla del panel.
BASE_PTS <- round(-100 * ANCLA_MIN / RANGO_ANCLA, 1)

datos <- datos |>
  mutate(
    pts_rezago    = round(100 * contrib_rezago    / RANGO_ANCLA, 1),
    pts_ejecucion = round(100 * contrib_ejecucion / RANGO_ANCLA, 1),
    pts_canon     = round(100 * contrib_canon     / RANGO_ANCLA, 1),
    pts_mineria   = round(100 * contrib_mineria   / RANGO_ANCLA, 1)
  )

# Nombre del mes construido a mano (no vía locale del sistema: en
# shinyapps.io no está garantizado que el locale español esté instalado).
MESES_ES <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
              "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")
fecha_corte <- as.Date(datos$periodo[1])
PERIODO_LABEL <- sprintf("%s de %s", MESES_ES[as.integer(format(fecha_corte, "%m"))],
                         format(fecha_corte, "%Y"))
ANIO_POBLACION <- format(fecha_corte, "%Y")
N_DIST_TOTAL  <- nrow(datos)

# =======================================================================
# CAPAS ADICIONALES — riesgo por Fenómeno del Niño, quintiles de pobreza y
# población proyectada, para cruzar el IRSIP con otras variables.
#
# Año más reciente disponible en cada fuente (no hay dato más nuevo):
#  - Riesgo por Niño 2026: CENEPRED, escenarios de riesgo por inundación y
#    por movimiento de masas ante el Fenómeno del Niño 2026.
#  - Quintiles de pobreza: MIDIS/INEI, Mapa de Pobreza 2017.
#  - Población proyectada: INEI, proyección 2026.
# =======================================================================

# --- Riesgo por Fenómeno del Niño 2026 (CENEPRED) -----------------------
# Los dos shapefiles son distritales y su UBIGEO cruza al 100% con
# geo_distrital.rds (1 891 de 1 891), así que se leen SÓLO las tablas de
# atributos (.dbf) y se unen a la geometría que ya está en memoria.
NIVELES_RIESGO <- c("Bajo", "Medio", "Alto", "Muy alto")

# Los dos archivos no escriben igual la franja más alta ("Muy alto" en
# inundación, "Muy Alto" en movimiento de masas), así que se normaliza
# antes de construir el factor.
nivel_riesgo_factor <- function(x) {
  x <- trimws(as.character(x))
  x <- sub("^muy alto$", "Muy alto", x, ignore.case = TRUE)
  x <- sub("^alto$",     "Alto",     x, ignore.case = TRUE)
  x <- sub("^medio$",    "Medio",    x, ignore.case = TRUE)
  x <- sub("^bajo$",     "Bajo",     x, ignore.case = TRUE)
  factor(x, levels = NIVELES_RIESGO, ordered = TRUE)
}

inund_dbf <- foreign::read.dbf(
  "data/inundaciones/Riesgo_Inund_Distritos.dbf", as.is = TRUE
) |>
  transmute(
    ubigeo         = normalizar_ubigeo(UBIGEO),
    nivel_riesgo   = nivel_riesgo_factor(NRiesgo_In),
    nivel_vuln     = trimws(as.character(NVuln_In)),
    viviendas_exp  = as.numeric(viv_sinu),
    agricola_exp   = as.numeric(sagr_inu),
    vias_exp       = as.numeric(vias_sinu)
  ) |>
  filter(!is.na(ubigeo))

mmasa_dbf <- foreign::read.dbf(
  "data/inundaciones/Riesgo_MMasa_Distritos.dbf", as.is = TRUE
) |>
  transmute(
    ubigeo         = normalizar_ubigeo(UBIGEO),
    nivel_riesgo   = nivel_riesgo_factor(NRiesgo_MM),
    nivel_vuln     = trimws(as.character(Vuln_MM)),
    viviendas_exp  = as.numeric(vivsm_ma_a),
    agricola_exp   = as.numeric(sagr_smm),
    vias_exp       = as.numeric(vias_smm)
  ) |>
  filter(!is.na(ubigeo))

geo_inund <- geo_dist |> dplyr::inner_join(inund_dbf, by = "ubigeo")
geo_mmasa <- geo_dist |> dplyr::inner_join(mmasa_dbf, by = "ubigeo")

# --- Quintiles de pobreza (MIDIS/INEI, Mapa de Pobreza 2017) ------------
# El Excel trae encabezados con espacios y mayúsculas irregulares; se
# renombra por posición para no depender de ese formato. Las últimas
# filas del anexo son notas al pie sin ubigeo válido: normalizar_ubigeo()
# las devuelve como NA y se descartan aquí.
#
# El cruce con la geometría es un inner_join a propósito: el mapa de
# pobreza es de 2017 y no cubre los 17 distritos creados después de esa
# fecha (030612, 050413, 050512-050515, 080915-080918, 090724, 090725,
# 130112, 180107, 221006, 250306, 250307). Esos distritos simplemente no
# se pintan en esta capa; no se les inventa un quintil.
quintiles_raw <- readxl::read_excel(
  "data/Quintiles_polacion pobre.xlsx",
  sheet = "Anexo 01-IncidenciaPobreza"
)
names(quintiles_raw) <- c(
  "ubigeo", "departamento", "provincia", "distrito", "poblacion_2017",
  "pobreza_pct", "coef_var", "ic_inf", "ic_sup",
  "quintil_medio", "quintil_inferior", "quintil_superior"
)
NIVELES_QUINTIL <- c("1 (más pobre)", "2", "3", "4", "5 (menos pobre)")
quintiles <- quintiles_raw |>
  mutate(ubigeo = normalizar_ubigeo(ubigeo)) |>
  filter(!is.na(ubigeo)) |>
  transmute(
    ubigeo,
    quintil = factor(
      round(as.numeric(quintil_medio)), levels = 1:5, labels = NIVELES_QUINTIL
    ),
    pobreza_pct = round(as.numeric(pobreza_pct), 1)
  )

geo_dist_pobreza <- geo_dist |> dplyr::inner_join(quintiles, by = "ubigeo")

# --- Población proyectada (INEI, proyección 2026) -----------------------
# A diferencia del IRSIP, la población SÍ se agrega geográficamente sin
# problema —es un conteo, no un índice—, así que se calculan los totales
# provincial y departamental por prefijo de ubigeo. Los 196 códigos de
# provincia y los 25 de departamento cruzan al 100% con las geometrías.
poblacion_dist <- datos |> transmute(ubigeo, poblacion = as.numeric(poblacion))

poblacion_prov <- datos |>
  group_by(ubigeo_prov) |>
  summarise(poblacion = sum(poblacion), n_dist = n(), .groups = "drop")

poblacion_dep <- datos |>
  group_by(ubigeo_dep) |>
  summarise(poblacion = sum(poblacion), n_dist = n(), .groups = "drop")

geo_pob_dist <- geo_dist |> dplyr::inner_join(poblacion_dist, by = "ubigeo")
geo_pob_prov <- geo_prov |> dplyr::inner_join(poblacion_prov, by = "ubigeo_prov")
geo_pob_dep  <- geo_dep  |> dplyr::inner_join(poblacion_dep,  by = "ubigeo_dep")

RANGO_POBLACION <- range(geo_pob_dist$poblacion, na.rm = TRUE)
TOPE_SLIDER     <- ceiling(RANGO_POBLACION[2] / 1000) * 1000

# --- Paletas y etiquetas de las capas adicionales ------------------------
# Los niveles de riesgo van con dos familias de color distintas para que
# las dos capas se distingan entre sí y de la rampa roja del IRSIP.
pal_inund <- colorFactor(
  palette = c("#C6DBEF", "#6BAED6", "#2171B5", "#08306B"),
  domain = NULL, levels = NIVELES_RIESGO, ordered = TRUE, na.color = "#BDBDBD"
)
pal_mmasa <- colorFactor(
  palette = c("#F6E8C3", "#DFC27D", "#BF812D", "#8C510A"),
  domain = NULL, levels = NIVELES_RIESGO, ordered = TRUE, na.color = "#BDBDBD"
)
pal_pobreza <- colorFactor(
  palette = c("#7A0177", "#C51B8A", "#F768A1", "#FBB4B9", "#FEEBE2"),
  domain = NULL, levels = NIVELES_QUINTIL, na.color = "#BDBDBD"
)

# La población está muy sesgada (de 104 habitantes en el distrito más
# pequeño a 11.6 millones en Lima), así que una rampa continua dejaría
# casi todo en blanco. Se usan cortes fijos por nivel, con su propia
# escala en cada uno: los rangos distrital, provincial y departamental no
# son comparables entre sí.
CORTES_POB_DIST <- c(0, 1e3, 5e3, 1e4, 5e4, 1e5, Inf)
CORTES_POB_PROV <- c(0, 25e3, 50e3, 1e5, 25e4, 5e5, Inf)
CORTES_POB_DEP  <- c(0, 5e5, 1e6, 15e5, 25e5, Inf)

pal_pob_dist <- colorBin("Blues", domain = NULL, bins = CORTES_POB_DIST,
                         na.color = "#BDBDBD")
pal_pob_prov <- colorBin("Blues", domain = NULL, bins = CORTES_POB_PROV,
                         na.color = "#BDBDBD")
pal_pob_dep  <- colorBin("Blues", domain = NULL, bins = CORTES_POB_DEP,
                         na.color = "#BDBDBD")

etiqueta_riesgo <- function(f, titulo) {
  HTML(sprintf(
    "<div style='min-width:225px'>
       <strong>%s</strong><br/>%s, %s
       <hr style='margin:4px 0'/>
       %s: <strong>%s</strong><br/>
       Nivel de vulnerabilidad: <strong>%s</strong>
       <div style='font-size:11px; margin-top:4px'>
         Viviendas expuestas: %s<br/>
         Superficie agrícola expuesta: %s ha<br/>
         Vías expuestas: %s km
       </div>
     </div>",
    f$nombdist, f$nombprov, f$nombdep,
    titulo, as.character(f$nivel_riesgo), f$nivel_vuln,
    fmt(f$viviendas_exp, 0), fmt(f$agricola_exp, 1), fmt(f$vias_exp, 1)
  ))
}

etiqueta_pobreza <- function(f) {
  HTML(sprintf(
    "<div style='min-width:200px'>
       <strong>%s</strong><br/>%s, %s
       <hr style='margin:4px 0'/>
       Quintil de pobreza: <strong>%s</strong><br/>
       Incidencia de pobreza: <strong>%s%%</strong>
     </div>",
    f$nombdist, f$nombprov, f$nombdep, as.character(f$quintil),
    fmt(f$pobreza_pct, 1)
  ))
}

etiqueta_poblacion <- function(nombre, detalle, poblacion) {
  HTML(sprintf(
    "<div style='min-width:200px'>
       <strong>%s</strong>%s
       <hr style='margin:4px 0'/>
       Población proyectada %s: <strong>%s</strong> hab.
     </div>",
    nombre, detalle, ANIO_POBLACION, fmt(poblacion, 0)
  ))
}

# Las etiquetas de las capas distritales se precalculan una vez al
# arranque y luego se subconjuntan por índice, para no rehacer 1 891
# sprintf() en cada cambio de filtro.
etiquetas_inund <- lapply(seq_len(nrow(geo_inund)), function(i)
  etiqueta_riesgo(geo_inund[i, ], "Riesgo de inundación"))
etiquetas_mmasa <- lapply(seq_len(nrow(geo_mmasa)), function(i)
  etiqueta_riesgo(geo_mmasa[i, ], "Riesgo de movimiento de masas"))
etiquetas_pobreza <- lapply(seq_len(nrow(geo_dist_pobreza)), function(i)
  etiqueta_pobreza(geo_dist_pobreza[i, ]))

etiquetas_pob_dist <- lapply(seq_len(nrow(geo_pob_dist)), function(i)
  etiqueta_poblacion(
    geo_pob_dist$nombdist[i],
    sprintf("<br/>%s, %s", geo_pob_dist$nombprov[i], geo_pob_dist$nombdep[i]),
    geo_pob_dist$poblacion[i]
  ))
etiquetas_pob_prov <- lapply(seq_len(nrow(geo_pob_prov)), function(i)
  etiqueta_poblacion(
    geo_pob_prov$nombprov[i],
    sprintf("<br/>%s &middot; %d distritos", geo_pob_prov$nombdep[i],
            geo_pob_prov$n_dist[i]),
    geo_pob_prov$poblacion[i]
  ))
etiquetas_pob_dep <- lapply(seq_len(nrow(geo_pob_dep)), function(i)
  etiqueta_poblacion(
    geo_pob_dep$nombdep[i],
    sprintf("<br/>%d distritos", geo_pob_dep$n_dist[i]),
    geo_pob_dep$poblacion[i]
  ))

# --- UNIÓN A GEOMETRÍAS --------------------------------------------------
# Por ubigeo (código), nunca por nombre: los nombres de distrito no son
# únicos entre provincias/departamentos.
geo_dist <- geo_dist |> left_join(datos, by = "ubigeo")

# geo_distrital.rds ya traía sus propias columnas ubigeo_prov/ubigeo_dep;
# el left_join las chocó con las de `datos` y las renombró a .x/.y en vez
# de fusionarlas. Se recalculan explícitas a partir del propio ubigeo (que
# sí quedó limpio) para no depender de cuál de las dos versiones sea la
# "correcta".
geo_dist$ubigeo_prov <- substr(geo_dist$ubigeo, 1, 4)
geo_dist$ubigeo_dep  <- substr(geo_dist$ubigeo, 1, 2)

agg_prov <- datos |>
  group_by(ubigeo_prov) |>
  summarise(
    n_dist   = n(),
    n_alta   = sum(nivel_alerta == "Alta"),
    n_media  = sum(nivel_alerta == "Media"),
    n_baja   = sum(nivel_alerta == "Baja"),
    # La categoría la define la peor alerta presente en el territorio, no
    # un promedio ni una suma ponderada: basta UN distrito en alerta alta
    # para que la provincia/departamento se pinte de rojo.
    categoria_alerta = case_when(
      n_alta  >= 1 ~ "Alta",
      n_media >= 1 ~ "Media",
      TRUE         ~ "Baja"
    ),
    categoria_alerta = factor(categoria_alerta, levels = c("Baja", "Media", "Alta")),
    irsip_prom   = round(mean(irsip), 1),
    .groups = "drop"
  )

agg_dep <- datos |>
  group_by(ubigeo_dep) |>
  summarise(
    n_dist   = n(),
    n_alta   = sum(nivel_alerta == "Alta"),
    n_media  = sum(nivel_alerta == "Media"),
    n_baja   = sum(nivel_alerta == "Baja"),
    categoria_alerta = case_when(
      n_alta  >= 1 ~ "Alta",
      n_media >= 1 ~ "Media",
      TRUE         ~ "Baja"
    ),
    categoria_alerta = factor(categoria_alerta, levels = c("Baja", "Media", "Alta")),
    irsip_prom   = round(mean(irsip), 1),
    .groups = "drop"
  )

geo_prov <- geo_prov |> left_join(agg_prov, by = "ubigeo_prov")
geo_dep  <- geo_dep  |> left_join(agg_dep,  by = "ubigeo_dep")

# --- PALETA ---------------------------------------------------------------
# Un único semáforo de 3 categorías para los tres niveles (distrital,
# provincial, departamental): rojo si hay alerta alta, naranja si el peor
# es media, amarillo en el resto. No arranca en blanco/gris —"Baja" es un
# dato real, no un "sin dato"—; el gris queda reservado exclusivamente a
# na.color, para polígonos sin unión (no deberían existir en el universo
# actual, pero se deja como salvaguarda).
RAMPA <- c("#FFE08A", "#FDAE61", "#F46D43", "#D7301F", "#7F0000")

pal_alerta <- colorFactor(
  palette = c(RAMPA[1], RAMPA[3], RAMPA[4]),
  domain = NULL, levels = c("Baja", "Media", "Alta"), na.color = "#BDBDBD"
)

ZOOM_PROVINCIAL <- 7
ZOOM_DISTRITAL  <- 9

nivel_por_zoom <- function(z) {
  if (is.null(z) || is.na(z)) return("dep")
  if (z >= ZOOM_DISTRITAL)  return("dist")
  if (z >= ZOOM_PROVINCIAL) return("prov")
  "dep"
}

color_alerta <- function(nivel_alerta) {
  switch(as.character(nivel_alerta),
         "Alta" = RAMPA[4], "Media" = RAMPA[3], RAMPA[1])
}

# Etiqueta de hover a nivel distrital: el índice y el aporte de sus
# componentes (Sección 8.3 — un puntaje sin descomposición no es
# accionable).
etiqueta_distrito <- function(f) {
  HTML(sprintf(
    "<div style='min-width:235px'>
       <strong>%s</strong><br/>
       <span style='color:%s; font-weight:600'>%s</span> &middot; %s
       <hr style='margin:4px 0'/>
       IRSIP: <strong>%s</strong> / 100 &nbsp;(percentil %s)
       <div style='font-size:11px; margin-top:4px'>
         Piso del panel (ancla): %s pts<br/>
         Rezago de conflictos (61%%): <strong>%+.1f</strong> pts<br/>
         Ejecución de canon (&minus;17%%): <strong>%+.1f</strong> pts<br/>
         Canon per cápita (11%%): <strong>%+.1f</strong> pts<br/>
         Minería per cápita (11%%): <strong>%+.1f</strong> pts
       </div>
     </div>",
    f$nombdist, color_alerta(f$nivel_alerta), as.character(f$nivel), f$provincia,
    f$irsip, f$percentil,
    BASE_PTS, f$pts_rezago, f$pts_ejecucion, f$pts_canon, f$pts_mineria
  ))
}

# Etiqueta de hover a nivel departamental/provincial: la categoría que
# define el color (la peor alerta presente) más el desagregado de conteos
# —no el índice, porque el IRSIP no se agrega geográficamente—.
etiqueta_territorio <- function(nombre, f) {
  HTML(sprintf(
    "<div style='min-width:200px'>
       <strong>%s</strong><br/>
       %d distritos evaluados
       <hr style='margin:4px 0'/>
       Categoría de alerta: <strong>%s</strong>
       <div style='font-size:11px; margin-top:6px'>
         <span style='color:#D7301F'>&#9632;</span> Alerta alta: <strong>%d</strong><br/>
         <span style='color:#F46D43'>&#9632;</span> Alerta media: <strong>%d</strong><br/>
         <span style='color:#FFE08A'>&#9632;</span> Alerta baja: <strong>%d</strong>
       </div>
     </div>",
    nombre, f$n_dist, as.character(f$categoria_alerta), f$n_alta, f$n_media, f$n_baja
  ))
}

etiquetas_dep  <- lapply(seq_len(nrow(geo_dep)), function(i)
  etiqueta_territorio(geo_dep$nombdep[i], geo_dep[i, ]))
etiquetas_prov <- lapply(seq_len(nrow(geo_prov)), function(i)
  etiqueta_territorio(paste0(geo_prov$nombprov[i], ", ", geo_prov$nombdep[i]), geo_prov[i, ]))
etiquetas_dist <- lapply(seq_len(nrow(geo_dist)), function(i)
  etiqueta_distrito(geo_dist[i, ]))

# --- UI ----------------------------------------------------------------
ui <- page_sidebar(
  title = "ORSIP — Observatorio de Riesgo Social en la Inversión Pública",
  theme = theme_orsip,
  
  sidebar = sidebar(
    width = 320,
    markdown("**Filtros**"),
    selectInput("dep", "Departamento",
                choices = c("Todos", sort(unique(datos$departamento)))),
    selectInput("prov", "Provincia", choices = c("Todas")),
    selectInput("dist", "Distrito", choices = c("Todos")),
    selectInput("nivel", "Nivel de riesgo (IRSIP)",
                choices = c("Todos", "Alto", "Medio", "Bajo", "Muy bajo")),
    sliderInput("irsip_min", "IRSIP mínimo", min = 0, max = 100, value = 0),
    hr(),
    markdown("**Capas adicionales** (para cruzar con el IRSIP)"),
    checkboxInput("show_inund", "Riesgo de inundación (Niño 2026)",
                  value = FALSE),
    conditionalPanel(
      condition = "input.show_inund == true",
      checkboxGroupInput("filtro_inund", "Niveles a mostrar",
                         choices = NIVELES_RIESGO, selected = NIVELES_RIESGO)
    ),
    checkboxInput("show_mmasa", "Riesgo de movimiento de masas (Niño 2026)",
                  value = FALSE),
    conditionalPanel(
      condition = "input.show_mmasa == true",
      checkboxGroupInput("filtro_mmasa", "Niveles a mostrar",
                         choices = NIVELES_RIESGO, selected = NIVELES_RIESGO)
    ),
    checkboxInput("show_pobreza", "Quintiles de pobreza (MIDIS, 2017)",
                  value = FALSE),
    conditionalPanel(
      condition = "input.show_pobreza == true",
      selectInput("filtro_quintil", "Filtrar por quintil de pobreza",
                  choices = c("Todos", NIVELES_QUINTIL))
    ),
    checkboxInput("show_poblacion",
                  sprintf("Población proyectada (INEI, %s)", ANIO_POBLACION),
                  value = FALSE),
    conditionalPanel(
      condition = "input.show_poblacion == true",
      sliderInput("filtro_poblacion", "Rango de población distrital",
                  min = 0, max = TOPE_SLIDER,
                  value = c(0, TOPE_SLIDER), step = 1000),
      markdown("<span style='font-size:11px'>La capa de población sigue el
                zoom: departamental, provincial o distrital. El rango de
                arriba filtra sólo la vista distrital.</span>")
    ),
    hr(),
    markdown(sprintf(
      "El mapa cambia entre nivel departamental, provincial y distrital
      según el zoom.

      El color es el mismo **semáforo de alerta** en los tres niveles:
      rojo si hay al menos un distrito en alerta alta, naranja si el peor
      es media, amarillo en el resto. A nivel distrital es la categoría
      del propio distrito; a nivel provincial/departamental, la peor
      categoría entre sus distritos (el IRSIP no se agrega
      geográficamente, así que no se promedia).

      Si eliges un departamento, provincia o distrito en los filtros, el
      mapa resalta con su color real sólo esa unidad; el resto del
      territorio queda en gris tenue, como referencia de ubicación. La
      tabla de alertas y los indicadores se filtran de la misma forma.

      **Corte del índice: %s.** %d distritos evaluados (universo con
      exposición a canon)."
      , PERIODO_LABEL, N_DIST_TOTAL
    ))
  ),
  
  layout_columns(
    col_widths = c(4, 4, 4),
    row_heights = "110px",
    fill = FALSE,
    value_box(title = "Distritos evaluados", value = textOutput("n_univ"),
              theme = "primary"),
    value_box(title = "En alerta", value = textOutput("n_alto"),
              theme = "danger"),
    value_box(title = "IRSIP promedio", value = textOutput("score_prom"),
              theme = "secondary")
  ),
  
  navset_card_underline(
    nav_panel("Mapa de riesgo", leafletOutput("mapa", height = 550)),
    nav_panel("Alertas", DTOutput("tabla_alertas")),
    nav_panel("Descomposición", uiOutput("detalle")),
    nav_panel("Cómo leer esto", uiOutput("nota_metodologica"))
  )
)

# --- SERVER ------------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$dep, {
    provs <- if (input$dep == "Todos") {
      sort(unique(datos$provincia))
    } else {
      sort(unique(datos$provincia[datos$departamento == input$dep]))
    }
    updateSelectInput(session, "prov", choices = c("Todas", provs), selected = "Todas")
    
    dists <- if (input$dep == "Todos") {
      sort(unique(datos$distrito))
    } else {
      sort(unique(datos$distrito[datos$departamento == input$dep]))
    }
    updateSelectInput(session, "dist", choices = c("Todos", dists), selected = "Todos")
  })
  
  # La provincia también acota el distrito, tanto si el usuario la elige a
  # mano como si queda reiniciada en "Todas" por un cambio de departamento.
  observeEvent(input$prov, {
    base <- datos
    if (input$dep  != "Todos") base <- filter(base, departamento == input$dep)
    if (input$prov != "Todas") base <- filter(base, provincia == input$prov)
    dists <- sort(unique(base$distrito))
    updateSelectInput(session, "dist", choices = c("Todos", dists), selected = "Todos")
  }, ignoreInit = TRUE)
  
  filtrados <- reactive({
    d <- datos
    if (input$dep != "Todos")  d <- filter(d, departamento == input$dep)
    if (input$prov != "Todas") d <- filter(d, provincia == input$prov)
    if (!is.null(input$dist) && input$dist != "Todos") d <- filter(d, distrito == input$dist)
    if (input$nivel != "Todos") d <- filter(d, nivel == input$nivel)
    filter(d, irsip >= input$irsip_min)
  })
  
  output$n_univ    <- renderText({ nrow(filtrados()) })
  output$n_alto    <- renderText({ sum(filtrados()$nivel %in% c("Alto", "Medio")) })
  output$score_prom <- renderText({
    v <- mean(filtrados()$irsip)
    if (is.nan(v)) "—" else round(v, 1)
  })
  
  # El mapa base se dibuja una sola vez con las tres capas apiladas, y luego
  # se muestran u ocultan según el zoom, para que no salte al interactuar.
  output$mapa <- renderLeaflet({
    leaflet() |>
      addTiles() |>  # OpenStreetMap estándar — gratis, sin API key
      setView(lng = -75.5, lat = -9.5, zoom = 5.3) |>
      addPolygons(
        data = geo_dep, group = "Departamental", layerId = ~paste0("dep_", ubigeo_dep),
        fillColor = ~pal_alerta(categoria_alerta), fillOpacity = 0.75,
        color = "#FFFFFF", weight = 1,
        label = etiquetas_dep
      ) |>
      addPolygons(
        data = geo_prov, group = "Provincial", layerId = ~paste0("prov_", ubigeo_prov),
        fillColor = ~pal_alerta(categoria_alerta), fillOpacity = 0.75,
        color = "#FFFFFF", weight = 0.6,
        label = etiquetas_prov
      ) |>
      addPolygons(
        data = geo_dist, group = "Distrital", layerId = ~paste0("dist_", ubigeo),
        fillColor = ~pal_alerta(nivel_alerta), fillOpacity = 0.8,
        color = "#666666", weight = 0.3, opacity = 0.6,
        label = etiquetas_dist
      ) |>
      addLegend(
        position = "bottomright", pal = pal_alerta, values = c("Baja", "Media", "Alta"),
        title = "Categoría de alerta<br/>(según nivel del IRSIP)", layerId = "leyenda"
      ) |>
      hideGroup(c("Provincial", "Distrital"))
  })
  
  # Nivel territorial vigente según el zoom. Se guarda en un reactiveVal —y
  # no se deriva del zoom en cada observador— porque reactiveVal sólo
  # notifica cuando el valor CAMBIA: así, arrastrar el mapa o pasar de zoom
  # 10 a 11 no redibuja nada. Antes de la primera señal de zoom el mapa
  # arranca en vista departamental (setView con zoom 5.3).
  #
  # La leyenda ya no depende del nivel: los tres (distrito, provincia,
  # departamento) usan el mismo semáforo de 3 categorías, así que el
  # cambio de zoom sólo alterna qué capa se ve, sin tocar addLegend.
  nivel_mapa <- reactiveVal("dep")
  observeEvent(input$mapa_zoom, {
    nivel_mapa(nivel_por_zoom(input$mapa_zoom))
  })
  
  observeEvent(nivel_mapa(), {
    n <- nivel_mapa()
    proxy <- leafletProxy("mapa")
    if (n == "dist") {
      proxy |> showGroup("Distrital") |> hideGroup(c("Departamental", "Provincial"))
    } else if (n == "prov") {
      proxy |> showGroup("Provincial") |> hideGroup(c("Departamental", "Distrital"))
    } else {
      proxy |> showGroup("Departamental") |> hideGroup(c("Provincial", "Distrital"))
    }
  })
  
  # --- Resaltado de la unidad geográfica seleccionada en los filtros -----
  # Códigos (ubigeo) efectivamente elegidos: manda el filtro más específico
  # —si hay distrito, de él se derivan su provincia y su departamento; si
  # no, manda la provincia; si no, el departamento—. Sin ningún filtro
  # geográfico activo los tres quedan en NULL y el mapa se ve exactamente
  # igual que en el renderLeaflet inicial (panorama nacional completo).
  sel <- reactive({
    dep_code <- NULL; prov_code <- NULL; dist_code <- NULL
    
    if (!is.null(input$dist) && input$dist != "Todos") {
      cand <- datos
      if (input$dep  != "Todos") cand <- filter(cand, departamento == input$dep)
      if (input$prov != "Todas") cand <- filter(cand, provincia == input$prov)
      cand <- filter(cand, distrito == input$dist)
      if (nrow(cand) >= 1) {
        dist_code <- cand$ubigeo[1]
        prov_code <- cand$ubigeo_prov[1]
        dep_code  <- cand$ubigeo_dep[1]
      }
    } else if (input$prov != "Todas") {
      cand <- datos
      if (input$dep != "Todos") cand <- filter(cand, departamento == input$dep)
      cand <- filter(cand, provincia == input$prov)
      if (nrow(cand) >= 1) {
        prov_code <- cand$ubigeo_prov[1]
        dep_code  <- cand$ubigeo_dep[1]
      }
    } else if (input$dep != "Todos") {
      dep_code <- unique(datos$ubigeo_dep[datos$departamento == input$dep])[1]
    }
    
    list(dep = dep_code, prov = prov_code, dist = dist_code)
  })
  
  # ¿Cada polígono de la capa cae dentro de la selección vigente? Se usa
  # substr() sobre el propio ubigeo —no una columna aparte— porque geo_prov
  # y geo_dep no necesariamente traen el código del nivel padre, y el
  # ubigeo ya lo codifica: los 2 primeros dígitos son el departamento y los
  # 4 primeros la provincia.
  resaltado_dep <- function(s) {
    if (is.null(s$dep)) return(rep(TRUE, nrow(geo_dep)))
    geo_dep$ubigeo_dep == s$dep
  }
  resaltado_prov <- function(s) {
    if (is.null(s$dep)) return(rep(TRUE, nrow(geo_prov)))
    if (!is.null(s$prov)) return(geo_prov$ubigeo_prov == s$prov)
    substr(geo_prov$ubigeo_prov, 1, 2) == s$dep
  }
  resaltado_dist <- function(s) {
    if (is.null(s$dep)) return(rep(TRUE, nrow(geo_dist)))
    if (!is.null(s$dist)) return(geo_dist$ubigeo == s$dist)
    if (!is.null(s$prov)) return(geo_dist$ubigeo_prov == s$prov)
    geo_dist$ubigeo_dep == s$dep
  }
  
  # Sin selección, las tres capas se ven igual que en el renderLeaflet
  # inicial. Con selección, sólo la unidad elegida conserva su color real
  # (alerta o IRSIP); el resto del territorio queda en gris tenue, como
  # referencia de ubicación, no como dato.
  GRIS_FUERA_SELECCION <- "#D9D9D9"
  
  dibujar_capas_principales <- function() {
    s   <- sel()
    hd  <- resaltado_dep(s)
    hp  <- resaltado_prov(s)
    hdi <- resaltado_dist(s)
    
    leafletProxy("mapa") |>
      clearGroup("Departamental") |>
      addPolygons(
        data = geo_dep, group = "Departamental", layerId = ~paste0("dep_", ubigeo_dep),
        fillColor = ifelse(hd, pal_alerta(geo_dep$categoria_alerta), GRIS_FUERA_SELECCION),
        fillOpacity = ifelse(hd, 0.75, 0.25),
        color = "#FFFFFF", weight = 1, label = etiquetas_dep
      ) |>
      clearGroup("Provincial") |>
      addPolygons(
        data = geo_prov, group = "Provincial", layerId = ~paste0("prov_", ubigeo_prov),
        fillColor = ifelse(hp, pal_alerta(geo_prov$categoria_alerta), GRIS_FUERA_SELECCION),
        fillOpacity = ifelse(hp, 0.75, 0.25),
        color = "#FFFFFF", weight = 0.6, label = etiquetas_prov
      ) |>
      clearGroup("Distrital") |>
      addPolygons(
        data = geo_dist, group = "Distrital", layerId = ~paste0("dist_", ubigeo),
        fillColor = ifelse(hdi, pal_alerta(geo_dist$nivel_alerta), GRIS_FUERA_SELECCION),
        fillOpacity = ifelse(hdi, 0.8, 0.25),
        color = "#666666", weight = 0.3, opacity = 0.6, label = etiquetas_dist
      )
  }
  observeEvent(sel(), dibujar_capas_principales(), ignoreInit = TRUE)
  
  # Elegir un departamento, provincia o distrito en el sidebar también
  # centra el mapa en la unidad más específica de las tres que esté fijada.
  observeEvent(sel(), {
    s <- sel()
    geom <- if (!is.null(s$dist)) {
      geo_dist[geo_dist$ubigeo == s$dist, ]
    } else if (!is.null(s$prov)) {
      geo_prov[geo_prov$ubigeo_prov == s$prov, ]
    } else if (!is.null(s$dep)) {
      geo_dep[geo_dep$ubigeo_dep == s$dep, ]
    } else {
      NULL
    }
    if (!is.null(geom) && nrow(geom) > 0) {
      bb <- sf::st_bbox(geom)
      if (all(is.finite(bb))) {
        leafletProxy("mapa") |>
          flyToBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
      }
    }
  }, ignoreInit = TRUE)
  
  # --- Capas adicionales: riesgo, pobreza y población ---------------------
  # Las de riesgo y pobreza NO dependen del zoom: se agregan o quitan según
  # los checkboxes y conviven con la capa de IRSIP que esté visible. La de
  # población sí sigue el zoom (ver más abajo).
  #
  # Todas comparten la misma salvaguarda: si el filtro deja el subconjunto
  # vacío, se limpia el grupo y no se llama a addPolygons. Pasarle a leaflet
  # cero polígonos con `label = list()` revienta dentro de safeLabel(), que
  # hace sum() sobre una lista vacía y aborta la sesión.
  
  dibujar_capa_riesgo <- function(grupo, capa, etiquetas, niveles_sel,
                                  mostrar, pal, titulo_leyenda, id_leyenda) {
    proxy <- leafletProxy("mapa") |>
      clearGroup(grupo) |> removeControl(id_leyenda)
    if (!isTRUE(mostrar)) return(invisible(NULL))
    
    idx <- which(as.character(capa$nivel_riesgo) %in% niveles_sel)
    if (length(idx) == 0) return(invisible(NULL))
    
    proxy |>
      addPolygons(
        data = capa[idx, ], group = grupo,
        fillColor = ~pal(nivel_riesgo), fillOpacity = 0.65,
        color = "#666666", weight = 0.3,
        label = etiquetas[idx]
      ) |>
      addLegend(
        position = "bottomleft", pal = pal, values = NIVELES_RIESGO,
        title = titulo_leyenda, layerId = id_leyenda
      )
  }
  
  dibujar_inund <- function() {
    dibujar_capa_riesgo(
      "Inundacion", geo_inund, etiquetas_inund, input$filtro_inund,
      input$show_inund, pal_inund,
      "Riesgo de inundación<br/>(Niño 2026)", "leyenda_inund"
    )
  }
  observeEvent(input$show_inund,  dibujar_inund(), ignoreInit = TRUE)
  observeEvent(input$filtro_inund, dibujar_inund(), ignoreInit = TRUE,
               ignoreNULL = FALSE)
  
  dibujar_mmasa <- function() {
    dibujar_capa_riesgo(
      "MovMasas", geo_mmasa, etiquetas_mmasa, input$filtro_mmasa,
      input$show_mmasa, pal_mmasa,
      "Riesgo de movimiento<br/>de masas (Niño 2026)", "leyenda_mmasa"
    )
  }
  observeEvent(input$show_mmasa,  dibujar_mmasa(), ignoreInit = TRUE)
  observeEvent(input$filtro_mmasa, dibujar_mmasa(), ignoreInit = TRUE,
               ignoreNULL = FALSE)
  
  dibujar_pobreza <- function() {
    proxy <- leafletProxy("mapa") |>
      clearGroup("Pobreza") |> removeControl("leyenda_pobreza")
    if (!isTRUE(input$show_pobreza)) return(invisible(NULL))
    
    q <- input$filtro_quintil
    idx <- if (is.null(q) || q == "Todos") {
      seq_len(nrow(geo_dist_pobreza))
    } else {
      which(as.character(geo_dist_pobreza$quintil) == q)
    }
    if (length(idx) == 0) return(invisible(NULL))
    
    proxy |>
      addPolygons(
        data = geo_dist_pobreza[idx, ], group = "Pobreza",
        fillColor = ~pal_pobreza(quintil), fillOpacity = 0.7,
        color = "#666666", weight = 0.3,
        label = etiquetas_pobreza[idx]
      ) |>
      addLegend(
        position = "bottomleft", pal = pal_pobreza, values = NIVELES_QUINTIL,
        title = "Quintil de pobreza<br/>(MIDIS 2017)",
        layerId = "leyenda_pobreza"
      )
  }
  observeEvent(input$show_pobreza, dibujar_pobreza(), ignoreInit = TRUE)
  observeEvent(input$filtro_quintil, dibujar_pobreza(), ignoreInit = TRUE)
  
  # Población: a diferencia de las otras capas, ésta sí cambia de unidad
  # territorial con el zoom (departamento / provincia / distrito), igual que
  # la capa del IRSIP. La población es un conteo y sí se puede sumar
  # geográficamente, así que en los niveles agregados se muestra el total
  # real del territorio, no un promedio ni un conteo de distritos.
  #
  # Cada nivel tiene su propia escala de cortes: los rangos son
  # inconmensurables (104-1.3 M en distritos, 205 mil-11.6 M en
  # departamentos) y una escala común dejaría casi todo en un solo color.
  GRUPOS_POBLACION <- c("PoblacionDep", "PoblacionProv", "PoblacionDist")
  
  dibujar_poblacion <- function() {
    proxy <- leafletProxy("mapa")
    for (g in GRUPOS_POBLACION) proxy <- proxy |> clearGroup(g)
    proxy <- proxy |> removeControl("leyenda_poblacion")
    if (!isTRUE(input$show_poblacion)) return(invisible(NULL))
    
    n <- nivel_mapa()
    if (n == "dist") {
      rango <- input$filtro_poblacion
      if (is.null(rango)) rango <- c(0, TOPE_SLIDER)
      idx <- which(geo_pob_dist$poblacion >= rango[1] &
                     geo_pob_dist$poblacion <= rango[2])
      if (length(idx) == 0) return(invisible(NULL))
      capa   <- geo_pob_dist[idx, ]
      etiq   <- etiquetas_pob_dist[idx]
      pal    <- pal_pob_dist
      cortes <- CORTES_POB_DIST
      grupo  <- "PoblacionDist"
      titulo <- sprintf("Población distrital<br/>%s (INEI)", ANIO_POBLACION)
    } else if (n == "prov") {
      capa   <- geo_pob_prov
      etiq   <- etiquetas_pob_prov
      pal    <- pal_pob_prov
      cortes <- CORTES_POB_PROV
      grupo  <- "PoblacionProv"
      titulo <- sprintf("Población provincial<br/>%s (INEI)", ANIO_POBLACION)
    } else {
      capa   <- geo_pob_dep
      etiq   <- etiquetas_pob_dep
      pal    <- pal_pob_dep
      cortes <- CORTES_POB_DEP
      grupo  <- "PoblacionDep"
      titulo <- sprintf("Población departamental<br/>%s (INEI)", ANIO_POBLACION)
    }
    
    # Los cortes llegan hasta Inf; para la leyenda se usa el máximo real de
    # la capa, porque addLegend no sabe rotular un bin abierto.
    valores <- c(0, max(capa$poblacion, na.rm = TRUE))
    
    proxy |>
      addPolygons(
        data = capa, group = grupo,
        fillColor = ~pal(poblacion), fillOpacity = 0.7,
        color = "#666666", weight = if (n == "dist") 0.3 else 0.6,
        label = etiq
      ) |>
      addLegend(
        position = "bottomleft", pal = pal, values = valores,
        title = titulo, layerId = "leyenda_poblacion"
      )
  }
  observeEvent(input$show_poblacion, dibujar_poblacion(), ignoreInit = TRUE)
  observeEvent(input$filtro_poblacion, dibujar_poblacion(), ignoreInit = TRUE)
  observeEvent(nivel_mapa(), dibujar_poblacion(), ignoreInit = TRUE)
  
  output$tabla_alertas <- renderDT({
    filtrados() |>
      arrange(desc(irsip)) |>
      transmute(
        Departamento = departamento, Provincia = provincia, Distrito = distrito,
        IRSIP = irsip, Nivel = nivel, Percentil = percentil,
        `Rezago conflictos` = rezago_conflictos,
        `Ejecución canon (%)` = round(ejecucion_pct, 1),
        `Canon per cápita (S/)` = round(canon_pc, 0),
        `Minería per cápita (S/)` = round(mineria_pc, 0),
        Población = poblacion
      ) |>
      datatable(rownames = FALSE, options = list(pageLength = 15),
                class = "compact stripe") |>
      formatStyle(
        "Nivel",
        backgroundColor = styleEqual(
          c("Alto", "Medio", "Bajo", "Muy bajo"),
          c("#D7301F", "#F46D43", "#FDAE61", "#FFE08A")
        ),
        color = styleEqual(
          c("Alto", "Medio", "Bajo", "Muy bajo"),
          c("#FFFFFF", "#22303C", "#22303C", "#22303C")
        )
      )
  })
  
  seleccionada <- reactiveVal(NULL)
  observeEvent(input$mapa_shape_click, {
    id <- input$mapa_shape_click$id
    if (!is.null(id) && startsWith(id, "dist_")) {
      seleccionada(sub("^dist_", "", id))
    }
  })
  
  output$detalle <- renderUI({
    sel <- seleccionada()
    if (is.null(sel)) {
      return(markdown(
        "*Haz clic en un distrito del mapa —acerca el zoom hasta el nivel
        distrital— para ver la descomposición de su IRSIP.*"
      ))
    }
    f <- filter(datos, ubigeo == sel)
    if (nrow(f) == 0) return(NULL)
    
    tagList(
      h4(paste0(f$distrito, ", ", f$provincia, " — ", f$departamento)),
      p(class = "lead", sprintf(
        "IRSIP: %s / 100 (%s, percentil %s sobre %d distritos evaluados).",
        f$irsip, as.character(f$nivel), f$percentil, N_DIST_TOTAL
      )),
      hr(),
      h5("Aporte de cada componente"),
      p(em("El puntaje siempre va con su descomposición (Sección 8.3). Los
            pesos provienen de la validación empírica de Ponce y McClintock
            (2014), no de un criterio experto.")),
      tags$table(class = "table table-sm",
                 tags$thead(tags$tr(
                   tags$th("Componente"), tags$th("Peso"), tags$th("Valor del mes"),
                   tags$th("Aporte (pts IRSIP)")
                 )),
                 tags$tbody(
                   tags$tr(tags$td("Piso del panel (ancla)"), tags$td("—"), tags$td("—"),
                           tags$td(sprintf("%.1f", BASE_PTS))),
                   tags$tr(tags$td("Rezago de conflictos (t-1)"), tags$td("61.1 %"),
                           tags$td(sprintf("%d caso(s)", f$rezago_conflictos)),
                           tags$td(sprintf("%+.1f", f$pts_rezago))),
                   tags$tr(tags$td("Ejecución de canon"), tags$td("−16.7 %"),
                           tags$td(sprintf("%s%%", fmt(f$ejecucion_pct, 1))),
                           tags$td(sprintf("%+.1f", f$pts_ejecucion))),
                   tags$tr(tags$td("Canon per cápita (12m)"), tags$td("11.1 %"),
                           tags$td(paste0("S/ ", fmt(f$canon_pc, 0))),
                           tags$td(sprintf("%+.1f", f$pts_canon))),
                   tags$tr(tags$td("Minería per cápita (12m)"), tags$td("11.1 %"),
                           tags$td(paste0("S/ ", fmt(f$mineria_pc, 0))),
                           tags$td(sprintf("%+.1f", f$pts_mineria)))
                 )
      ),
      hr(),
      p(em(sprintf(
        "Distrito-mes, corte %s. Población proyectada: %s habitantes.",
        PERIODO_LABEL, fmt(f$poblacion, 0)
      ))),
      p(em("Las inversiones que se ejecuten en este distrito heredan este
            puntaje: el índice no puntúa proyectos individuales, porque el
            canon no se asigna a inversiones específicas de forma rastreable
            y los conflictos no se georreferencian a códigos de inversión
            (Sección 8.4)."))
    )
  })
  
  output$nota_metodologica <- renderUI({
    markdown(sprintf(
      "### Qué mide el IRSIP

      El Índice de Riesgo Social de la Inversión Pública estima, a nivel
      distrito-mes, la exposición a que se **inicie** un nuevo conflicto
      social vinculado a la inversión pública y la actividad extractiva,
      en una escala de 0 a 100.

      No mide conflictividad actual: un distrito con un conflicto en curso
      puede tener IRSIP bajo si no hay señales de que se abra uno nuevo.

      ### De dónde salen los pesos

      Son coeficientes tomados de la Especificación 3 de Ponce y McClintock
      (2014), normalizados por su estadístico *t* — no ponderaciones
      asignadas por juicio experto:

      - Rezago de conflictos (t-1): **61.1%%**, aumenta el riesgo
      - Ejecución de canon: **16.7%%**, factor protector
      - Canon per cápita: **11.1%%**, aumenta el riesgo
      - Producción minera per cápita: **11.1%%**, aumenta el riesgo

      ### Cómo leer el mapa

      El color es el mismo **semáforo de tres categorías en los tres
      niveles** (distrital, provincial y departamental): **rojo** si hay
      al menos un distrito en alerta alta, **naranja** si el peor es
      media, y **amarillo** en el resto.

      A nivel **distrital** la categoría es la del propio distrito (Alto
      → rojo; Medio → naranja; Bajo/Muy bajo → amarillo). A nivel
      **provincial y departamental** el IRSIP **no se agrega
      geográficamente** —promediarlo no tiene sustento en la
      metodología—, así que la categoría del territorio es la PEOR alerta
      que tenga algún distrito adentro. En ambos casos, el hover muestra
      el detalle detrás del color: el IRSIP exacto y su descomposición a
      nivel distrital, el desagregado de conteos a nivel prov/dep.

      Si filtras por departamento, provincia o distrito en el panel
      lateral, el mapa resalta con su color real sólo esa unidad; el resto
      del territorio queda en gris tenue, como referencia de ubicación.

      ### Capas adicionales

      Son variables de contexto para cruzar con el IRSIP; ninguna entra en
      su cálculo.

      - **Riesgo por Fenómeno del Niño 2026 (CENEPRED).** Dos escenarios
        distritales independientes, uno de inundación y otro de movimiento
        de masas, cada uno en cuatro franjas (bajo a muy alto). Reemplazan
        a la antigua capa de susceptibilidad a inundaciones (SIGRID 2003),
        que era un solo polígono de ámbito sin niveles.
      - **Quintiles de pobreza (MIDIS/INEI, Mapa de Pobreza 2017).** Cubre
        1 874 distritos: los 17 creados después de 2017 no tienen dato y
        quedan sin pintar en esa capa.
      - **Población proyectada (INEI, %s).** Sigue el zoom igual que el
        IRSIP: total departamental, provincial o distrital según el nivel
        en pantalla. A diferencia del índice, la población sí se suma
        geográficamente, así que los niveles agregados muestran el total
        real del territorio. Cada nivel usa su propia escala de color.

      ### Qué no hace

      - **No puntúa proyectos.** Las inversiones heredan el puntaje de su
        distrito.
      - **No es causal.** Los coeficientes provienen de un panel
        departamental y anual (2004-2010) trasladado a un panel distrital y
        mensual (2019-2026); no son una estimación propia.
      - **La ausencia de alerta no es ausencia de riesgo.** El registro de
        conflictos de la Defensoría depende de la presencia institucional
        territorial; sólo 253 de 1 892 distritos aparecen alguna vez en el
        registro bajo la definición usada.

      ### Estado

      Corte publicado: **%s**. Los puntajes de este tablero provienen del
      pipeline de datos abiertos descrito en la metodología (MEF, MINEM,
      Defensoría del Pueblo, INEI). Ver la
      [metodología completa](https://orsip.site/metodologia.html) y las
      limitaciones declaradas allí."
      , ANIO_POBLACION, PERIODO_LABEL
    ))
  })
}

shinyApp(ui, server)
