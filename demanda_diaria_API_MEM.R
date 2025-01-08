# ---- demanda diaria -----
#
# Autor: Ricardo Villalba
#


#---- auxiliar ----

`%||%` <- function (x, y) if (is.null(x)) y else x

.csvdefs <- local({
  
  demanda_diaria <- c(
    fecha = "POSIXct", dem = "integer",
    temp = "numeric", fecha_consulta = "Date"
  )
  
  demanda_diaria_Edesur.txt <- c(
    fecha = "POSIXct", dem = "integer",
    fecha_consulta = "Date"
  )
  
  demanda_diaria_GBA.txt <- c(
    fecha = "POSIXct", dem = "integer",
    temp = "numeric", fecha_consulta = "Date"
  )
  
  environment()
})

withRetries<- function(n, expr) {
  for (i in seq_len(n)) 
    withRestarts(
      retry = \(e) message(e, ". Intentos restantes: ", n - i),
      withCallingHandlers(
        error = \(e) invokeRestart("retry", e),
        return(eval.parent(substitute(expr)))))
  stop("Demasiados intentos fallidos")
}

make_API_URL <- function(.method, .domain = "demanda-svc/demanda/", ...) {
  paste0(
    "https://api.cammesa.com/", .domain, .method,
    "?", paste0(URLencode(...names()), "=", URLencode(c(...)), collapse = "&"))
}

normalizar_nombre <- function(nombre) {
  nombre |> 
    sub(pattern = "^demanda_diaria_\\s*(.*?)\\s?\\.txt$", replace="\\1") |> 
    gsub(pattern = "\\s+", replace = "_") |> 
    iconv(sub = "_", to = "ASCII//TRANSLIT") |> 
    sprintf(fmt = "demanda_diaria_%s.txt") 
}

# ---- apiCalls ----
## esta API utiliza fechas en formato simple sin tiempo
# tz <- "Etc/GMT"
# Normalmente devuelve {fecha, dem} o {fecha, dem, temp]
DemandaYTemperaturaRegionByFecha <- function(fecha = Sys.Date(), id_region) {
  withRetries(5, {
    message("Obteniendo fecha:", as.Date(fecha), ", id_region: ", id_region)
    url <-  make_API_URL(
      "ObtieneDemandaYTemperaturaRegionByFecha",
      fecha = strftime(fecha, "%Y-%m-%d"),
      id_region = id_region)
    data <- as.data.table(jsonlite::fromJSON(url))
    if (NROW(data) > 0) 
      data[, fecha := as.POSIXct(
        fecha, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC")][]
    else data.table()
  })
}

# ---- proceso ----

# regiones de demanda
obtenerRegion <- function() {
  url <- make_API_URL("RegionesDemanda")
  withRetries(5, {
    region <- jsonlite::fromJSON(url)
  })
  setDT(region)
  unique(region, by = "id", fromLast = T)
}

#consulta SOTR hacia atrás, todo lo disponible.
obtenerAcumulado <- function(fecha = Sys.Date(), id_region) {
  all_data <- NULL
  while ({
    data <- DemandaYTemperaturaRegionByFecha(fecha, id_region)
    nr <- NROW(data)
  }) {
    message("leídos ", nr, " registros ")
    data <- as.data.table(data)
    data[, fecha_consulta := as.Date(fecha)]
    all_data <- rbind(all_data, data)
    fecha <- fecha - 1
  }
  message("fin. leídos ", nr, " registros ")
  all_data
}

regenerarSOTR <- function(id_region, nombre, ask = TRUE) {
  if (interactive() && ask) {
    while ({
      resp <- readline(". Desea regenerarlo hasta lo máximo disponible? [y/n]:")
      !resp %in% c("y", "n")
    }) cat("especifique [y/n]\n")
    if (resp == "y") {
      data <- obtenerAcumulado(fecha, id_region = id_region)
      if (NROW(data)) {
        setDT(data)
        file_newname <- sprintf(
          "demanda_diaria_%s.txt", region[id == id_region, nombre[1]])
        message("Generar ", file_newname)
        saveRDS(data, file.path(folder, "data.RDS"))
        on.exit(message("Datos temporarios en", file.path(folder, "data.RDS")))
        fwrite(data, file.path(folder, file_newname), eol = "\n")
      } else {
        message("No se encontraron datos")
        FALSE
      }
    } else FALSE
  } else FALSE
}


completarSOTR <- function(regiones, fecha = Sys.Date()) {
  region <- obtenerRegion()
  for (id_region in region[, id]) {
    if (missing(regiones) || id_region %in% regiones) {
      nombre <- region[id == id_region, nombre[1]]
      fname <- normalizar_nombre(nombre)
      if (!file.exists(fname)) {
        stop("no se encontró ", fname)
      } else {
        old_data <- fread(
          file = fname,
          colClasses = .csvdefs[[fname]] %||% .csvdefs[["demanda_diaria"]]
        )
        setorder(old_data, fecha, fecha_consulta)
        desde <- old_data[.N, as.Date(fecha, tz = "America/Buenos_Aires")]
        hasta <- fecha
        if (hasta < desde) {
          stop(gettextf(
            "fecha futura en demanda_diaria [%s > %s, idreg=%d (%s)]",
            format(desde, "%Y-%m-%d"), format(hasta, "%Y-%m-%d"),
            id_region, nombre
          ))
        }
        fechas <- seq.Date(desde, hasta, by = "day")
        new_data <- rbindlist(
          lapply(fechas, DemandaYTemperaturaRegionByFecha, id_region = id_region)
        )
        match_cols = intersect(colnames(old_data), colnames(new_data))
        message("Combinando en ", paste(match_cols, collapse = ", "))
        new_data[, fecha_consulta := hasta]
        
        # si los datos coinciden exactamente, no se actualizan (notar 
        # que con en all_data se usa setkey, que por default es na.last == F
        new_data[old_data, on = match_cols, fecha_consulta := as.Date(NA)]
        
        # borra duplicados de la historia deja sólo última fecha_consulta
        all_data <- unique(
          by = c("fecha"), fromLast = TRUE, setkeyv(
            rbind(old_data, new_data, fill = TRUE), 
            c("fecha", "fecha_consulta"))
        )[,.SD, .SDcols = c(match_cols, "fecha_consulta")]

        # chequear completitud de la serie
        # tolerancia de 1 punto de dato de 15 (300 seg) minutos faltante
        stopifnot(as.numeric(max(diff.POSIXt(all_data[, fecha]))) <= 3300)

        file_newname <- fname
        message("Generar ", file_newname)
        fwrite(all_data, file_newname, eol = "\n")
      }
    }
  }
}

compilarSOTR <- function() {
  stop("en mantenimiento")
  #  folder <- "C:\\Users\\ar30592993\\OneDrive - Enel Spa\\cammesa\\demanda_diaria\\REST"
  region <- request.region()

  data <- lapply(region$id, \(id_region){
    nombre_region <- region[id == id_region, nombre]
    re <- paste0(
      "^demanda_diaria_", nombre_region,
      "_\\(", id_region, "\\)_", "(\\d{4}-\\d{2}-\\d{2})_(\\d{4}-\\d{2}-\\d{2})\\.txt\\.gz$"
    )
    files <- dir(folder, re)

    if (length(files)) {
      fsdesde <- as.Date(gsub(re, "\\1", files))
      fshasta <- as.Date(gsub(re, "\\2", files))
      file <- files[
        which.max(fshasta)
      ][
        which.max((fshasta - fsdesde)[which.max(fshasta)])
      ][1]
      message("Cargando ", nombre_region)
      df <- fread(file.path(folder, file))
      df[, id_region := id_region]
      df
    } else {
      message("no se encuentra ", nombre_region)
      NULL
    }
  })

  message("Generar ", "demanda_diaria_all.txt.gz")
  fwrite(
    rbindlist(Map(data, f = \(df) df[, c("fecha", "dem", "fecha_consulta", "id_region")])),
    file.path(folder, "demanda_diaria_all.txt.gz"),
    eol = "\n"
  )

  message("Generar ", "demanda_diaria_all_id_region.txt.gz")
  fwrite(
    region,
    file.path(folder, "demanda_diaria_all_id_region.txt.gz"),
    eol = "\n"
  )

  message("Generar ", "demanda_diaria_all_temperatura.txt.gz")
  fwrite(
    rbindlist(
      data |>
        Filter(f = \(df) "temp" %in% names(df)) |>
        Map(f = \(df) df[, c("fecha", "temp", "fecha_consulta", "id_region")])
    ),
    file.path(folder, "demanda_diaria_all_temperatura.txt.gz"),
    eol="\n"
  )
}

obtenerDemandasGBA <- function(ndias = 15L, completar = TRUE) {
  if (completar) completarSOTR(c(426, 1078))
  fname <- "demanda_diaria_GBA.txt"
  fname.eds <- "demanda_diaria_Edesur.txt"
  dem <- fread(
    file = fname,
    colClasses = .csvdefs[[fname]] %||% .csvdefs[["demanda_diaria"]])
  dem.eds <- fread(
    file = fname.eds,
    colClasses = .csvdefs[[fname.eds]] %||% .csvdefs[["demanda_diaria"]])
  dem[dem.eds, on = "fecha", eds := i.dem]
  # es buena idea esto?
  setattr(dem$fecha, "tzone", "America/Buenos_Aires")
  data <- dem[
    fecha_consulta >= Sys.Date() - ndias,
    .(fecha, dem, eds,
      temp = (nafill(temp, "locf") + 10L) * 250L)]
  structure(list(data = data, ndias = ndias), class="DemandasGBA")
}

plot.DemandasGBA <- function(x) {
  message("graficar")
  data <- x$data
  ndias <- x$ndias
  
  cdate <- Sys.Date()
  
  # máxima últ 12 hs
  max_24 <- data[
    fecha > fecha[.N] - 12L * 3600L &
      eds == max(eds[fecha > fecha[.N] - 12L * 3600L], na.rm = T)][1,]

  plt <- ggplot(data) +
    geom_line(aes(fecha, dem, color = "GBA")) +
    geom_line(aes(fecha, eds, color = "Edesur")) +
    geom_line(aes(fecha, temp, color = "Temperatura"),
                       linetype = "dashed") +
    scale_color_manual(values = c(
      Edesur = "blue", Temperatura = "red", GBA = "black"
    )) +
    guides(color = guide_legend("Ref.")) +
    scale_x_datetime(
      breaks = seq(as.POSIXct(cdate - ndias), data[, max(fecha)], "6 hour"),
      minor_breaks = seq(as.POSIXct(cdate - ndias), data[, max(fecha)], "3 hour"),
      date_labels = "%d %b %Hh"
    ) +
    scale_y_continuous(
      breaks = scales::extended_breaks(n = 10),
      name = "Demanda GBA y Edesur [MWh]",
      sec.axis = sec_axis(\(x) x / 250 - 10, name = "Temperatura \u00b0C")
    ) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "top", axis.title.x = element_blank()
    ) +
    labs(title = "Demanda GBA/Edesur vs Temperatura") +
    
    annotate("label",
      hjust = "right",
      x = max_24[, fecha] + c(0, 0, 0, 0),
      y = max_24[, unlist(.(
        eds + 500, dem + 500, eds + 1000,
        (temp)
      ))],
      color = c("black", "black", "black", "red"),
      fill = "white", 
      alpha = 0.75,
      label = max_24[, unlist(
        .(
          sprintf("%d MW", eds),
          sprintf("%d MW", dem),
          format(fecha),
          sprintf("%1.0f\u00b0C (máx. 12hs)", temp / 250L - 10L)
        )
      )]
    )
  
  # corte
  if (!is.null(data$cortes)) {
    plt <- cowplot::plot_grid(
      nrow = 2, ncol = 1,  align = "v", rel_heights = 2:1,
      (plt),
      (
        ggplot(data, aes(fecha, cortes)) 
        + geom_line(color="darkorange",linewidth=1)
        + scale_x_datetime (
          breaks = seq(as.POSIXct(cdate - ndias), data[, max(fecha)], "6 hour"),
          minor_breaks = seq(as.POSIXct(cdate - ndias), data[, max(fecha)], "3 hour"),
          date_labels = "%d %b %Hh") 
        + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                axis.ticks.x = element_blank())))
  }
  plt
}

cortes_ENRE <- function() {
  df.default <- data.frame(
    hora = character(0), EDN = integer(0), EDS = integer(0), 
    algo = character(0), clima = character(0), temp = integer(0), 
    UTCtime = as.POSIXct(numeric(0), tz= "UTC"))
  
  if (file.exists("cortes.csv")) {
    df0 <- read.csv("cortes.csv", sep=",") |> 
      transform(UTCtime = as.POSIXct(UTCtime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) 
  } else {
    df0 <- df.default
  }
  df0  
}

# ---- Init ----

{
  
  library(ggplot2)
  library(data.table)
  
  Sys.setenv(TZ = "America/Buenos_Aires")
  demandasGBA <- obtenerDemandasGBA(completar = TRUE)
  cortes <- cortes_ENRE()
  cortes <- as.data.table(cortes)[,- "temp"]
  demandasGBA$data[
    , cortes := cortes[demandasGBA$data, on = c("UTCtime" = "fecha"), roll = +Inf, EDS]]
  
  png("demanda_diaria.png", width = 1280, height = 720)
  print(plot(demandasGBA))
  dev.off()
  invisible()
}


# ---- Otros (aux) ----
# if (F) {
#   url <- "https://api.cammesa.com/demanda-svc/demanda/Agentes"
# 
#   withRetries(5, agente.data <- httr::content(httr::GET(url)))
# 
#   setDT(agente)
# 
#   distrib <- agente[tagNemo == "DI"]
# }
# 
# 
# # ---- completar demandas sotr ----
# # todos las regiones
# if (F) {
#   completarSOTR()
# }
# 
# # ---- compilar demanda SOTR ----
# if (F) {
#   compilarSOTR()
# }
