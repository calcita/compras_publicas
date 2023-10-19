################################################################################
# GLOBAL
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

# PAQUETES Y CONSTANTES GLOBALES------------------------------------------------
files <- list.files(path = "utils", full.names = TRUE)
invisible(sapply(files, source))

# DATOS Y CODIGUERAS------------------------------------------------------------
load("tables/compras_all.RData")
load_all(path = "tables", ext = ".csv")
organismos_lista <- readLines("tables/organismos_lista.txt")
anios <- anios_codiguera %>% select(anio) %>% pull() %>% sort()

# DATOS MODULO CARRUSEL --------------------------------------------------------
load_all(path = "data", ext = ".csv")
load_all(path = "data", ext = ".txt")

# MODULOS ----------------------------------------------------------------------
files <- list.files(path = "modules", full.names = TRUE)
invisible(sapply(files, source))

