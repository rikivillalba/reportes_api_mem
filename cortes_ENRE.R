# Actualizar tabla de cortes.


# ---- Parsing ----

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

hora_to_datetime <- function(hora, hora_act, tz = "") {
  hora_act <- as.POSIXct(hora_act, tz = tz)
  hora <- gsub("^\" *(.*)\"$", "\\1", hora) 
  as.POSIXct(sprintf(
    "%s %s:00", format(hora_act, "%Y-%m-%d", tz = tz), hora), tz = tz) -
    86400L * as.integer(format(hora_act, "%H:%M", tz = tz) < hora)
}


# ---- init ----

df.default <- data.frame(
  hora = character(0), EDN = integer(0), EDS = integer(0), 
  algo = character(0), clima = character(0), temp = integer(0), 
  UTCtime = as.POSIXct(numeric(0), tz= "UTC"))


data <- readLines(
  url("https://www.enre.gov.ar/Graficos/UFS/data/Datos_UFS.js?1"), 
  warn = FALSE)

data.proc <- data |> 
  paste(collapse="")  |> strsplit(";") |> _[[1]] |>
  lapply(\(i) regmatches(i, regexec("^ *(\\S*) *[=:] *(.*)$", i))[[1]]) |> 
  lapply(\(i) (setNames(list(llparse(tokenize(gsub("'", "\"", i[3])))),i[2]))) |>
  unlist(recursive=FALSE) |> 
  c(list(run_time = Sys.time()))

tz <- "Etc/GMT+3"   # cambiar x America/Buenos_Aires si hay cambios de horario
ctime <- Sys.time()

hora_act <- hora_to_datetime(data.proc$Hora_actual, ctime, tz = tz)

df <- data.proc$UFS |> lapply(unlist) |> do.call(what = rbind) |> 
  as.data.frame()  |> 
  setNames(c("hora", "EDN", "EDS", "algo", "clima", "temp")) |>
  transform(
    UTCtime = as.POSIXct(hora_to_datetime(hora, hora_act), tz = "UTC"),
    hora = gsub("^\"(.*)\"$", "\\1", hora),
    algo = gsub("^\"(.*)\"$", "\\1", algo),
    clima = trimws(gsub("^\" *(.*)\"$", "\\1", clima)),
    EDS = as.integer(EDS), 
    EDN = as.integer(EDN), 
    temp = as.integer(gsub("^\"(.*)\"$", "\\1", temp)))

stopifnot(identical(df[0,], df.default)) 

if (file.exists("cortes.csv")) {
  df0 <- read.csv("cortes.csv", sep=",") |> 
    transform(UTCtime = as.POSIXct(UTCtime, "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  stopifnot(identical(df0[0,], df.default)) 
} else {
  df0 <- df.default
}

df1 <- rbind(df0, df)

# para escribir
df1[!duplicated(df1$UTCtime, fromLast = TRUE),] |> 
  transform(UTCtime = format(UTCtime, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")) |>
  write.csv(file = "cortes.csv", row.names = FALSE, quote = FALSE)

