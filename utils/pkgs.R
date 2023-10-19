################################################################################
# PAQUETES
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

pkgs <- c(
  "shiny",
  "dqshiny",
  "faq",
  "shinyWidgets",
  "shinycssloaders",
  "shinydashboard",
  "htmlwidgets",
  "htmltools",
  "shinyglide",
  "shinyjs",
  "shinyBS",
  "sysfonts",
  "showtextdb",
  "showtext",
  # data
  "dplyr",
  "tidyr",
  "stringr",
  "forcats",
  "lubridate",
  "DT",
  "data.table",
  # plots
  "plotly",
  "ggplot2",
  "ggbump",
  "echarts4r"
)

invisible(lapply(pkgs, library, character.only = TRUE, quietly = TRUE))
