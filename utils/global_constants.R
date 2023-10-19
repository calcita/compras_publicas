################################################################################
# CONSTANTES GLOBALES
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

pesos_miles <- "Montos expresados en miles de pesos uruguayos constantes de 2010"
pesos_millones <- "Montos expresados en millones de pesos uruguayos constantes de 2010"

# Global constants --------------------------------------------------------
# number of rows in the HTML data table showing all packages
ROWS_PER_PAGE <- 10

DT_options_pt <- list(
  pageLength = ROWS_PER_PAGE, searching = FALSE,
  responsive = TRUE, dom = "ftip", info = FALSE,
  paging = TRUE
)

DT_options_pf <- list(
  pageLength = ROWS_PER_PAGE, searching = FALSE,
  responsive = TRUE, dom = "ftip", info = FALSE,
  paging = FALSE
)

DT_caption_millon <- htmltools::tags$caption(
  style = "caption-side: bottom; text-align: left; margin: 8px 0; font-size: 75%",
  "Montos expresados en millones de pesos uruguayos constantes de 2010"
)

# colores
color_bl <- "#6082B6"
color_ac <- "#CCCCFF"
color_am <- "#B6D0E2"
celeste <- "#B6D0E2"

paleta_azul <- c("#78A2CC", "#88AED0", "#96B9D0", "#A4C3D2", "#AECBD6", "#BFD4DB")


style_verbatim <- "color: white; width: 90%; font-size: 90%; text-align: left; margin-top: 10px; position:absolute; top:20px; font-family: ubuntu;"
style_checkbox <-  "margin-top:23px; accent-color:#86acac !important; font-size:12px; font-family:ubuntu;"