################################################################################
# FUNCIONES GENERALES
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################
# LOAD_ALL()--------------------------------------------------------------------
load_all <- function(path = "data", ext = ".csv") {
  if (ext == ".csv") {
    f <- data.table::fread
  } else {
    f <- readLines
  }
  files <- list.files(path = path, full.names = TRUE, pattern = ext)
  names <- gsub(paste0(path, "/"), "", files)
  names <- gsub(ext, "", names)
  l <- lapply(files, f)
  names(l) <- names
  list2env(l, envir = .GlobalEnv)
  rm(l)
}

# ROUND_OFF()-------------------------------------------------------------------
round_off <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- trunc(abs(x) * 10^(digits + 1)) / 10
  z <- floor(z * posneg + 0.5) / 10^digits
  return(z)
}

# CREATE_LINK()-----------------------------------------------------------------
create_link <- function(val) {
  sprintf('<a href="https://www.comprasestatales.gub.uy/consultas/detalle/mostrar-llamado/1/id/%s" target="_blank" class="btn btn-primary">Link</a>', val)
}

# FORMAT_NUMBER_ES -------------------------------------------------------------
format_number_es <- function(serie) {
  htmlwidgets::JS(
    stringr::str_glue(
      "function(params) {return Intl.NumberFormat('es-ES', { style: 'decimal'}).format(params.value[{{serie}}]);}",
      .open = "{{",
      .close = "}}"
    )
  )
}

# FORMAT_MILES()----------------------------------------------------------------
format_miles <- function(serie) {
  htmlwidgets::JS(
    "function(params) {
       var value = params.value[1].toFixed(2).replace('.', ',');
       return value.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, '$1.');
     }"
  )
}
