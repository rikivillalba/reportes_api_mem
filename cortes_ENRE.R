# ==== Actualizar tabla de cortes. ====
# Autor: Rikivillalba

# ---- parsing ----

tokenize <- function (text) {
  stopifnot(is.character(text) && length(text) == 1L)  
  pattern <- paste0(collapse = "|", c(
    "(\"([^\"]|(\\\\\"))*\")", "(\'([^\']|(\\\\\'))*\')",
    "([a-zA-Z0-9_$?-]+)", "([^'\"[:space:]])"))    # "([])}[({,;])",
  m <- gregexpr(pattern, text)
  tokens <- regmatches(text, m)[[1]]
  spaces <- regmatches(text, m, invert = TRUE)[[1]]
  if (any(p <- grep("[^[:space:]]", spaces))) {
    stop(sprintf(
      "no se esperaba \"%s\": ...%s...)",
      regmatches(spaces[p[1]], r <- regexpr("[^[:space:]]", spaces[p[1]])),
      substring(text, m[[1L]][p[1L] - 1L] - 5L,  m[[1L]][p[1L] - 1L] + r[[1]])))
  }
  tokens
}

token_gen_factory <- function(tokens) {
  pattern_id <- "^[+-]?((\\.\\d+)|(\\d+(\\.\\d*)?)([eE]-?\\d+)?)$"
  i_parser <<- 0L
  function(ahead = 0L) {
    i <- i_parser + ahead + 1L
    if (i <= length(tokens)) { 
      if (missing(ahead)) i_parser <<- i_parser + 1L   # consume 1 token
      t <- tokens[i] 
      structure(t, class = {
        if (t %in% c("{", "}", "[", "]", "(", ")", ";", ","))  t 
        else if (startsWith(t, "'") || startsWith(t, '"') || grepl(pattern_id, t)) "id"
        else stop(sprintf("token no válido %s", t))
      })
    } else { 
      structure("eof", class = "$")
    }
  }
}

llparse <- function(tokens) {
  next_token <- token_gen_factory(tokens)
  consume_one_after <- function(e) { e; next_token(); e}
  stopf <- function(...) stop(sprintf(...), call. = FALSE)
  
  if (!exists("Tailcall", envir = baseenv())) 
    Tailcall <- function(f, ...) f(...)

  nt_value <- function() {
    switch(
      class(next_token(0)),
      "[" =  { Tailcall(nt_list) },
      "id" = { consume_one_after(next_token(0)) },
      stopf("se esperaba '[' o valor, pero no '%s'", next_token(0)))
  }
  nt_list <- function() {
    next_token()  # consume [
    switch(
      class(next_token(0)),
     "]" =  { next_token(); list() },
     "[" =, "id"= { consume_one_after(nt_elements()) },
     stopf("no se esperaba '%s' después de [ en lista", next_token(0)))
  }
  nt_elements <- function(prev = NULL) {
    v <- nt_value()
    e0 <- c(prev, list(v))
    e <- switch(
      class(next_token(0)),
      "]" = { e0 },
      "," = { next_token(); Tailcall(nt_elements, e0) },
      stopf("no se esperaba '%s' después de <%s> en lista", next_token(0), class(v))
    )
    structure(e, class = "elements")
  }
  if (class(next_token(0)) == "$") 
    e <- NULL
  else {
    e <- nt_value()
    if (class(next_token(0)) != "$") 
      stopf("no se esperaba %s después de <%s>", next_token(0), class(e))
  }
  e
}

# ---- auxiliar ----

# "Cambio de horario 2007/2008" 
# tz_arg <- "America/Buenos_Aires"
# fmt <- "%Y-%m-%d %H:%M:%S%z"
# 
# "No existe 2007-12-30 00:00"
# as.POSIXct("2007-12-30 00:00:00-0200",fmt,tz=tz_arg)  # 2007-12-29 23:00:00 -03
# as.POSIXct("2007-12-30 00:00:00-0300",fmt,tz=tz_arg)  # 2007-12-30 01:00:00 -02
# 
# "Existe dos veces 2008-03-15 23:00:00"
# as.POSIXct("2008-03-15 22:00:00-0300",fmt,tz=tz_arg)  # 2008-03-15 23:00:00 -02
# as.POSIXct("2008-03-15 23:00:00-0300",fmt,tz=tz_arg)  # 2008-03-15 23:00:00 -03
# 
# "Error:"
# as.POSIXct("2007-12-30 00:00:00", tz = "America/Buenos_Aires") # Error
# 
# "En esta fecha hay 2 horas de diferencia"
# ( as.POSIXct("2008-03-15 23:00:00", tz = "America/Buenos_Aires")  
#   - as.POSIXct("2008-03-16 00:00:00", tz = "America/Buenos_Aires"))


hora_to_datetime <- function(hora, ctime, ordered = FALSE, tz = "") {
  ctime <- as.POSIXct(ctime, tz = tz)
  if (tz == "Etc/GMT+3") {
    ctime <- as.numeric(ctime)   # Y2K38 aware!
    hsec <-  as.numeric(sapply(hora, substring, c(1L,4L,7L), c(2L,5L,8L)) |>
      apply(1:2, as.integer) |> crossprod(c(3600L,60L,1L)))
    cday <- (ctime - 3L * 3600L) %/% 86400L * 86400L + 3L * 3600L
    if (ordered && length(hsec) > 1 && !length(ctime) == 1 ) {
      reslt <- numeric(length(hsec))
      for(i in rev(seq_along(hsec))) {
        reslt[i] <- ctime - 86400L + (hsec[i] - (ctime - cday) - 1L) %% 86400L + 1L
        ctime <- reslt[i]
        cday <- (ctime - 3L * 3600L) %/% 86400L * 86400L + 3L * 3600L
      }
    } else {
      reslt <- ctime - 86400L + (hsec - (ctime - cday) - 1L) %% 86400L + 1L
    } 
    as.POSIXct(reslt, tz = tz, origin = 0)
  } else {
    stime <- format(ctime, "%Y-%m-%dT%H:%M:%S", tz = tz)
    if (ordered && length(hora) > 1 && length(ctime) == 1) {
      reslt <- numeric(length(hsec))
      for(i in rev(seq_along(hsec))) {
        reslt[i] <- as.POSIXct(
          sprintf("%sT%s", substr(stime, 1L, 10L), hora),
          "%Y-%m-%dT%H:%M:%S", tz = tz) -
          86400L * as.integer(substr(ctime, 12L, 19L) <= hora)
        stime <- format(reslt[i], "%Y-%m-%dT%H:%M:%S", tz = tz)
      }
      as.POSIXct(reslt, tz = tz, origin = 0)
    } else {
      stime <- format(ctime, "%Y-%m-%dT%H:%M:%S", tz = tz)
      as.POSIXct(
        sprintf("%sT%s", substr(stime, 1L, 10L), hora),
        "%Y-%m-%dT%H:%M:%S", tz = tz) -
        86400L * as.integer(substr(ctime, 12L, 19L) <= hora)
    }
  }
}

# ---- init ----

df.default <- data.frame(
  hora = character(0), EDN = integer(0), EDS = integer(0), 
  algo = character(0), clima = character(0), temp = integer(0), 
  UTCtime = as.POSIXct(numeric(0), tz= "UTC"))

validate <- function(df) {
  stopifnot(
    identical(df[0,], df.default),
    all(diff(df$UTCtime) > 0))
  df
}

data <- readLines(
  url("https://www.enre.gov.ar/Graficos/UFS/data/Datos_UFS.js?1"), 
  warn = FALSE)

data.proc <- data |> 
  paste(collapse="")  |> strsplit(";") |> _[[1]] |>
  lapply(\(i) regmatches(i, regexec("^ *(\\S*) *[=:] *(.*)$", i))[[1]]) |> 
  lapply(\(i) (setNames(list(llparse(tokenize(gsub("'", "\"", i[3])))),i[2]))) |>
  unlist(recursive=FALSE) |> 
  c(list(run_time = Sys.time()))
stopifnot(grepl("^\"(.*)\"$", data.proc$Hora_actual))

tz <- "Etc/GMT+3"   # cambiar x America/Buenos_Aires si hay cambios de horario
ctime <- Sys.time()

hora_act <- hora_to_datetime(
  paste0(gsub("^\"(.*)\"$", "\\1", data.proc$Hora_actual), ":00"), ctime, tz = tz)

df <- data.proc$UFS |> lapply(unlist) |> do.call(what = rbind) |> 
  as.data.frame()  |> 
  setNames(c("hora", "EDN", "EDS", "algo", "clima", "temp"))

if (!all(grepl("^\"? *(.*)\"?$", df$hora))) stop("hora mal formada")

df <- df |>
  transform(hora = gsub("^\" *(.*)\"$", "\\1", hora)) |>
  transform(
    algo = gsub("^\" *(.*)\"$", "\\1", algo),
    clima = trimws(gsub("^\" *(.*)\"$", "\\1", clima)),
    EDS = as.integer(EDS), 
    EDN = as.integer(EDN), 
    temp = as.integer(gsub("^\"(.*)\"$", "\\1", temp)),
    UTCtime = as.POSIXct(hora_to_datetime(
      paste0(hora, ":00"), hora_act, ordered = TRUE, tz = tz), tz = "UTC")) |>
  validate()

if (file.exists("cortes.csv")) {
  df0 <- read.csv("cortes.csv", sep=",") |> 
    transform(UTCtime = as.POSIXct(UTCtime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC")) |>
    validate()
} else {
  df0 <- df.default
}

df1 <- rbind(df0, df) |> 
  (\(df) subset(df, !duplicated(UTCtime, fromLast = TRUE)))() |>
  validate() 

df1 |> 
  transform(UTCtime = format(UTCtime, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")) |>
  write.csv(file = "cortes.csv", row.names = FALSE, quote = FALSE)

# ---- para recuperar hora del commit y recalcular UTCtime ----
# hora_act <- as.POSIXct("20250107T202559Z", "%Y%m%dT%H%M%OS", tz = "UTC") + 20L
# df0 <- read.csv("cortes.csv", sep=",")
# if (!all(grepl("^\"? *(.*)\"?$", df0$hora))) stop("hora mal formada")
# df0 <- df0 |>
#   transform(hora = sub("^\"? *(.*)\"?$", "\\1", hora)) |>
#   transform(
#     UTCtime = as.POSIXct(hora_to_datetime(
#        paste0(hora,":00"), hora_act, ordered=TRUE, tz=tz), tz = "UTC"))
# stopifnot(
#   identical(df0[0,], df.default),
#   all(diff(df0$UTCtime) > 0))
# df0 |>
#   transform(UTCtime = format(UTCtime, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")) |>
#   write.csv(file = "cortes.csv", row.names = FALSE, quote = FALSE)
