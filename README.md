# ORSIP — Observatorio de Riesgo Social en la Inversión Pública

Sistema de alerta temprana que estima, para cada **distrito-mes** del Perú, la exposición a que se inicie un nuevo conflicto social vinculado a la inversión pública y la actividad extractiva. El resultado es el **IRSIP**, un puntaje de 0 a 100 construido a partir de cuatro variables de registros administrativos: el rezago de conflictos, la **eficiencia con que se ejecuta el canon minero**, el canon per cápita y el valor de producción minera per cápita.

Retoma el marco de Ponce y McClintock (2014) y traslada su validación empírica —departamental y anual— a un panel distrital y mensual, poniendo a prueba la hipótesis que ese estudio dejó planteada: que el canon opera como detonante sobre todo donde la capacidad de ejecución es baja.

Desarrollado por el equipo **Perú5** para el **Desafío de Datos para la Democracia: 25 años de la Carta Democrática Interamericana** (OEA).

🌐 Sitio: <https://orsip.site/> · 📊 Tablero: Shiny (`app/`) · 📦 Datos abiertos: `data/outputs/`

> **Estado actual.** El pipeline completo está integrado y corre sobre datos reales: MEF (gasto y canon), Defensoría del Pueblo (conflictos), MINEM (producción minera) e INEI (población). El corte publicado es **febrero de 2026**, con **1 891 distritos** evaluados sobre un universo de 1 892.

## Los documentos que mandan

| Documento | Qué define |
|:-----------------------------------|:-----------------------------------|
| `docs_proyecto/metodologia_indice_riesgo_social.md` | Diseño del índice. Fuente de verdad ante cualquier discrepancia. |
| `site/metodologia.qmd` | Versión divulgativa de lo anterior, publicada en el sitio. |
| `DICCIONARIO_DATOS.md` | Diccionario y metadatos de los datasets publicados. |

## El índice en una pantalla

Los pesos no son de juicio experto: salen de los estadísticos *t* de la Especificación 3, Tabla 1 de Ponce y McClintock (2014), normalizados por Σ\|t\| = 18.

| Variable                            |       Peso | Dirección         |
|:------------------------------------|-----------:|:------------------|
| Rezago de conflictos (t−1)          | **+0.611** | Aumenta el riesgo |
| Ejecución de canon                  | **−0.167** | Protector         |
| Canon per cápita (12 m)             | **+0.111** | Aumenta el riesgo |
| Producción minera per cápita (12 m) | **+0.111** | Aumenta el riesgo |

Cada variable se normaliza a [0, 1], se pondera, se suman las cuatro contribuciones y el resultado se reescala a 0–100 con un **ancla fija del panel histórico** —no del mes publicado—, de modo que un IRSIP de 55 signifique lo mismo en enero que en diciembre.

## Estructura del repositorio

```         
orsip/
├── src/                  # Pipeline, en orden de ejecución
│   ├── 00_procesar_mapa.R       # Geometrías IGN/INEI 2023 -> .rds simplificados
│   ├── 01_procesar_conflictos.R # Panel caso-mes de la Defensoría
│   ├── 02_procesar_poblacion.R  # Proyecciones INEI por ubigeo-año
│   ├── 03_procesar_gasto.R      # PIM, devengado y canon recaudado (MEF)
│   ├── 04_procesar_mineria.R    # Valor de producción minera (MINEM)
│   ├── 05_irsip.R               # Cálculo del índice y datasets publicados
│   └── 06_diccionario.R         # Diccionario y metadatos de data/outputs/
├── data/
│   ├── raw/              # Descargas originales. NO versionado (ver .gitignore)
│   ├── interim/          # Insumos intermedios y tablas de referencia
│   ├── processed/        # Paneles listos para el índice (.rds)
│   └── outputs/          # DATOS ABIERTOS publicados
├── app/                  # Tablero interactivo (Shiny)
│   ├── app.R
│   ├── data/             # Copia autocontenida de lo que el tablero necesita
│   └── rsconnect/        # Metadatos de despliegue en shinyapps.io
├── site/                 # Sitio de divulgación (Quarto)
├── docs/                 # Salida renderizada, servida por GitHub Pages
├── docs_proyecto/        # Documento metodológico y material institucional
├── renv.lock             # Entorno de paquetes reproducible
└── orsip.Rproj
```

La numeración de `src/` es el orden de ejecución, y cada script deja su salida en `data/` para el siguiente. No hay estado compartido en memoria entre ellos: cada uno se puede correr solo mientras existan sus insumos.

## Datos

### Fuentes

Todas públicas, oficiales y de acceso abierto.

| Insumo | Fuente | Entidad |
|:-----------------------|:-----------------------|:-----------------------|
| Gasto devengado y PIM | [Datos abiertos MEF](https://datosabiertos.mef.gob.pe/dataset/presupuesto-y-ejecucion-de-gasto-devengado-mensual) | MEF |
| Canon recaudado | [Datos abiertos MEF](https://datosabiertos.mef.gob.pe/dataset/presupuesto-y-ejecucion-de-ingreso-recaudado-mensual) | MEF |
| Conflictos sociales | [Mapas de conflictos sociales](https://www.defensoria.gob.pe/mapas-de-conflictos-sociales/) | Defensoría del Pueblo |
| Producción minera | [Boletín Estadístico Minero](https://www.gob.pe/institucion/minem/colecciones/6-boletin-estadistico-minero) | MINEM |
| Población proyectada | [SIRTOD](https://sirtod.inei.gob.pe/) | INEI |
| Geometrías y ubigeos | [Geoportal](https://geosdot.servicios.gob.pe/geoportal/) | IGN / INEI |
| Riesgo por Fenómeno del Niño | [SIGRID](https://sigrid.cenepred.gob.pe/sigridv3/documento/22169) | CENEPRED |
| Quintiles de pobreza | [RedInforma](https://app.midis.gob.pe/RedInforma/ContenidoEstructurado/Index) | MIDIS |

### Qué se versiona y qué no

`data/raw/` **no se versiona** por peso: hay que colocar ahí las descargas originales antes de correr el pipeline. Los shapefiles del IGN van en `data/raw/mapa/`; `src/00_procesar_mapa.R` genera los `.rds` simplificados, que sí son livianos y sí se versionan.

`data/interim/`, `data/processed/` y `data/outputs/` sí se versionan: son la cadena auditable que va del insumo al dato publicado.

### Datos abiertos publicados

`data/outputs/` es el producto reutilizable del proyecto. Cada corte publica su propio juego de archivos, identificado por el sufijo `AAAAMM`; **los cortes anteriores no se sobrescriben**.

| Archivo | Contenido |
|:-----------------------------------|:-----------------------------------|
| `irsip_202602.csv` | Corte publicado: IRSIP por distrito con sus cuatro variables en escala original, normalizada y como aporte en puntos. |
| `irsip_serie_mensual_202602.csv` | Serie histórica distrito-mes desde enero 2019. Panel **no balanceado**. |
| `irsip_202602_limites.csv` | Límites de normalización vigentes. Hacen reproducibles las columnas `n_*`. |
| `irsip_202602_disponibilidad.csv` | Latencia de cada fuente y por qué el corte es el que es. |
| `irsip_202602_excluidos.csv` | Distritos del universo que quedaron fuera del corte, con el motivo. |
| `datapackage.json` | Descriptor [Frictionless Data](https://specs.frictionlessdata.io/) con el esquema, tipos, claves y hashes. |
| `diccionario_variables.csv` | El diccionario en formato tabular, una fila por campo. |

El diccionario legible está en `DICCIONARIO_DATOS.md` y se regenera con `src/06_diccionario.R`.

## Cómo correr el proyecto

Todo se ejecuta **desde la raíz del proyecto**: las rutas son relativas a ella.

``` r
# 1. Restaurar el entorno exacto de paquetes
renv::restore()

# 2. Pipeline, en orden. Los pasos 00-04 requieren data/raw/ poblado.
source("src/00_procesar_mapa.R")        # geometrías + tabla canónica de ubigeos
source("src/01_procesar_conflictos.R")
source("src/02_procesar_poblacion.R")
source("src/03_procesar_gasto.R")
source("src/04_procesar_mineria.R")

# 3. Cálculo del índice -> data/outputs/
source("src/05_irsip.R")
```

`05_irsip.R` imprime un diagnóstico al terminar: cobertura de cada variable, aporte medio de cada componente, top 20 de riesgo y la media mensual del IRSIP de los últimos 14 meses. **Esa última tabla es la que hay que mirar**: si la media se desplaza sistemáticamente dentro del año, la normalización de la ejecución dejó de neutralizar el calendario presupuestal.

``` r
# Tablero interactivo
shiny::runApp("app")
```

``` bash
# Sitio de divulgación, desde site/
quarto preview
```

## Despliegue

**Sitio (Quarto → GitHub Pages).** `quarto render` dentro de `site/` escribe en `docs/`; luego `git push`. El dominio `orsip.site` se resuelve con el `CNAME` que se copia a `docs/`.

**Tablero (shinyapps.io).**

``` r
rsconnect::deployApp("app")
```

`deployApp()` sube **sólo** la carpeta que se le pasa, así que `app/app.R` no puede leer nada con `"../"`. Por eso `app/data/` es una copia autocontenida de lo que el tablero necesita. Si regeneras las geometrías o publicas un corte nuevo del índice, actualiza esa copia antes de desplegar.

## El tablero

El mapa cambia de unidad territorial según el zoom: departamental, provincial y distrital. A nivel distrital el color es el IRSIP. A nivel provincial y departamental **no** se colorea un promedio del índice —agregarlo geográficamente no tiene sustento metodológico—, sino cuántos distritos del territorio están en alerta, ponderando alta = 2, media = 1, baja = 0.

Sobre esa base se pueden encender capas de contexto para cruzar con el índice:

| Capa | Fuente | Nivel |
|:-----------------------|:-----------------------|:-----------------------|
| Riesgo de inundación (Fenómeno del Niño 2026) | CENEPRED | Distrital, 4 franjas |
| Riesgo de movimiento de masas (Niño 2026) | CENEPRED | Distrital, 4 franjas |
| Quintiles de pobreza | MIDIS, Mapa de Pobreza 2017 | Distrital, 5 quintiles |
| Población proyectada | INEI, 2026 | Sigue el zoom, como el IRSIP |

La ficha de trazabilidad descompone el puntaje de cada distrito en el aporte de sus cuatro determinantes, de modo que se vea **por qué** un territorio recibió su puntaje y no sólo cuál fue.

## Decisiones de implementación que conviene conocer

**Las llaves son ubigeos, no nombres.** Los nombres de distrito no son únicos entre provincias, y el canon y el SIAF llegan codificados por ubigeo de pliego. Los nombres quedan sólo para mostrar. Corolario para quien consuma los CSV: `ubigeo` **debe leerse como texto** o los departamentos 01–09 pierden el cero inicial y dejan de cruzar.

**El mes de corte se detecta, no se asume.** El panel de gasto del MEF viene balanceado con ceros hasta el cierre del año fiscal, así que una verificación estándar de completitud daría diciembre como mes válido. La regla implementada compara la actividad de cada mes contra la mediana de los seis previos y descarta lo que cae por debajo del 85 %. Con los datos actuales el cuello de botella es la Defensoría: el índice se publica para febrero de 2026 aunque haya canon hasta junio.

**El ancla de escala se congela.** El reescalamiento a 0–100 usa el mínimo y el máximo del índice bruto de todo el panel histórico. Recalcularlos al extender el panel desplazaría retroactivamente toda la serie y rompería la comparabilidad temporal que el ancla existe para garantizar.

**La ejecución se normaliza contra el propio mes.** Es un acumulado del año fiscal: el promedio nacional pasa de 1.3 % en enero a 75.6 % en diciembre. Normalizarla contra el histórico convertía la variable en un indicador del punto del calendario presupuestal —la media del IRSIP caía de 30 en enero a 13 en diciembre por puro ciclo de gasto—. Con el percentil intra-mes ese desplazamiento desaparece.

**Ausencia de registro no es dato faltante.** La Defensoría sólo lista distritos con casos y el padrón minero sólo distritos con producción. En ambos, la ausencia se interpreta como ausencia del fenómeno y entra como cero, no como `NA`.

**Nada se pierde en silencio.** Los distritos que quedan fuera del corte por tener `NA` en alguna variable se publican en `irsip_*_excluidos.csv`, para que la diferencia entre el universo declarado y las filas del corte sea explícita y no un descuadre silencioso.

## Limitaciones

Son propias del diseño del índice y distintas de las limitaciones de las fuentes. Están desarrolladas en la metodología; el resumen operativo:

1.  **Subregistro de conflictos.** Sólo una fracción de los 1 892 distritos aparece alguna vez en el registro de la Defensoría, que depende de la presencia institucional territorial. La variable con 61 % del peso vale 0 para la gran mayoría del universo, de modo que **entre distritos sin historial de conflicto el ranking lo definen la ejecución, el canon y la minería**. Es una propiedad heredada de los pesos, no un error de cálculo, pero condiciona cómo leer las posiciones intermedias.
2.  **Coeficientes importados.** Vienen de un panel departamental y anual (2004–2010) aplicado a uno distrital y mensual (2019–2026). El índice **no es una estimación causal**.
3.  **Minería medida en valor.** El dataset del MINEM trae valor en soles, no volumen físico: un distrito puede subir de puntaje por un alza de precios internacionales sin cambio en su actividad extractiva.
4.  **Latencia heterogénea.** El mes de corte lo fija la fuente más rezagada, lo que determina la latencia real del sistema de alerta temprana.

**La ausencia de alerta no es ausencia de riesgo.**

## Siguientes pasos

Fase de refinamiento, en orden de prioridad:

1.  **Coeficientes propios.** Modelo binomial negativo estimado sobre el panel distrito-mes 2019–2026 del propio observatorio, tratando el exceso de ceros del conteo distrital. Sustituye pesos importados por pesos propios y permite reportar intervalos de confianza.
2.  **Nuevas variables.** Criterio de inclusión: mejora en capacidad predictiva fuera de muestra, no disponibilidad del dato.
3.  **Reportes ciudadanos.** Canal que mitigue el sesgo de subregistro, con anonimato por defecto, agregación previa a la publicación y un protocolo que impida usarlo para manipular el puntaje de un territorio. Aplica la Ley N.º 29733 de Protección de Datos Personales.
4.  **Menor latencia.** Explorar fuentes de conflictividad de mayor frecuencia: es la condición para que una alerta temprana sea operativamente útil.

## Referencia

Ponce, Aldo F. y Cynthia McClintock. 2014. "The Explosive Combination of Inefficient Local Bureaucracies and Mining Production: Evidence from Localized Societal Protests in Peru". *Latin American Politics and Society* 56(3): 118–140. [JSTOR](https://www.jstor.org/stable/43284916) · [Apéndices y datos](https://sites.google.com/site/aldofponceugolini/data)
