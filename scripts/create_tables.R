################################################################################
# TABLAS
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################
library(dplyr)

# DATOS BRUTOS
load("raw/compras.Rdata")

# TABLA ARTICULOS---------------------------------------------------------------
articulos <- compras %>%
  filter(!is.na(articulo)) %>%
  filter(duplicated(articulo) == F) %>%
  select(articulo, iditem, cod_familia, cod_sub_familia)

max(nchar(articulos$articulo))
write.csv(articulos, "tables/articulos.csv", row.names = FALSE, na = "")

# TABLA CODIGUERA FAMILIA ------------------------------------------------------
familia_codiguera <- compras %>%
  count(familia, cod_familia) %>%
  filter(!is.na(cod_familia)) %>%
  select(familia, cod_familia)
write.csv(familia_codiguera, "tables/familia_codiguera.csv", row.names = FALSE, na = "")

# TABLA CODIGUERA SUB-FAMILIA --------------------------------------------------
sub_familia_codiguera <- compras %>%
  count(familia, cod_familia, sub_familia, cod_sub_familia) %>%
  select(-n) %>%
  filter(!is.na(cod_sub_familia))
write.csv(sub_familia_codiguera, "tables/sub_familia_codiguera.csv", row.names = FALSE, na = "")

# TABLA ARTICULOS DETALLE ------------------------------------------------------
articulos_detalle <- articulos %>%
  left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia"))
write.csv(articulos_detalle, "tables/articulos_detalle.csv", row.names = FALSE, na = "")

# TABLA ARTICULOS DETALLE SIN OBRAS --------------------------------------------
articulos_so <- articulos %>%
  filter(cod_familia != 6 & !duplicated(iditem))
write.csv(articulos_so, "tables/articulos_so.csv", row.names = FALSE, na = "")

# TABLA PROVEEDORES ------------------------------------------------------------
proveedores <- compras %>%
  filter(!is.na(codigo_fiscal)) %>%
  filter(!duplicated(codigo_fiscal)) %>%
  select(codigo_fiscal, denominacion_social) %>%
  mutate(denominacion_social = stringr::str_trim(denominacion_social, side = "both"))
write.csv(proveedores, "tables/proveedores.csv", row.names = FALSE, na = "")

# TABLA CODIGUERA TIPO COMPRA --------------------------------------------------
tipo_compra_codiguera <- compras %>%
  group_by(tipo_compra) %>%
  count(tipo_compra_codigo) %>%
  select(-n)
write.csv(tipo_compra_codiguera, "tables/tipo_compra_codiguera.csv", row.names = FALSE, na = "")

# TABLA compras x anio----------------------------------------------------------
for (i in unique(compras$anio)) {
  assign(paste0("compras_", i), compras %>%
    filter(anio == i) %>%
    select(
      ocid, fecha, iditem, monto_item, cantidad, unidad,
      monto_item_total, monto_compra, cod_familia,
      cod_sub_familia, codigo_fiscal, organismo_codigo,
      categoria_codigo, tipo_compra_codigo,
      monto_estandar, cantidad_estandar, unidad_estandar
    ))
}

compras_all <- list(
  compras_2002 = compras_2002,
  compras_2003 = compras_2003,
  compras_2004 = compras_2004,
  compras_2005 = compras_2005,
  compras_2006 = compras_2006,
  compras_2007 = compras_2007,
  compras_2008 = compras_2008,
  compras_2009 = compras_2009,
  compras_2010 = compras_2010,
  compras_2011 = compras_2011,
  compras_2012 = compras_2012,
  compras_2013 = compras_2013,
  compras_2014 = compras_2014,
  compras_2015 = compras_2015,
  compras_2016 = compras_2016,
  compras_2017 = compras_2017,
  compras_2018 = compras_2018,
  compras_2019 = compras_2019,
  compras_2020 = compras_2020,
  compras_2021 = compras_2021
)

save(compras_all, file = "tables/compras_all.RData")

# TABLA COMPRAS RESUMEN --------------------------------------------------------
compras_resumen <- compras %>%
  filter(duplicated(ocid) == F) %>%
  select(ocid, fecha, monto_compra, organismo_codigo, tipo_compra_codigo, categoria_codigo, anio)
write.csv(compras_resumen, "tables/compras_resumen.csv", row.names = FALSE, na = "")

# TABLA COMPRAS RESUMEN SIN OBRAS ----------------------------------------------
compras_resumen_so <- compras %>%
  filter(cod_familia != 6 | is.na(cod_familia)) %>%
  filter(!is.na(monto_item_total)) %>%
  group_by(ocid) %>%
  mutate(monto_compra = sum(monto_item_total)) %>%
  ungroup() %>%
  filter(duplicated(ocid) == F) %>%
  select(ocid, fecha, monto_compra, organismo_codigo, tipo_compra_codigo, categoria_codigo, anio)
write.csv(compras_resumen_so, "tables/compras_resumen_so.csv", row.names = FALSE, na = "")

# TABLA PRECIOS UNITARIOS ------------------------------------------------------
precios_unit <- compras %>%
  filter(!is.na(monto_estandar)) %>%
  select(ocid, fecha, anio, iditem, monto_estandar, cantidad_estandar, unidad_estandar)
write.csv(precios_unit, "tables/precios_unit.csv", row.names = FALSE, na = "")

# VECTOR ORGANISMOS
organismos_lista <- organismos %>%
  select(organismo_sigla) %>% pull() %>% sort()
writeLines(organismos_lista, "tables/organismos_lista.txt")

# TABLA DICCIONARIO ------------------------------------------------------------
vars <- c(
  "articulo", "iditem", "cod_fam", "cod_subfam", "familia",
  "sub_familia", "cod_subfam", "codigo_fiscal", "denominacion_social",
  "fecha", "ocid", "monto_item", "cantidad", "unidad", "monto_compra",
  "organismo_sigla", "cod_compra", "url" # ,
  #' monto_estandar', 'cantidad_estandar', 'unidad estandar'
)

desc <- c(
  "nombre del articulo", "identificador del articulo",
  "codigo familia", "codigo sub-familia", "descripcion familia",
  "descripcion sub-familia", "codigo sub-familia", "RUT proveedor",
  "denominacion social de proveedor", "fecha de la compra",
  "identificador de la compra", "monto unitario del articulo en pesos constantes de 2010",
  "cantidad comprada del articulo", "unidad de presentación del artículo",
  "monto total de la compra",
  "organismo comprador", "codigo del tipo de compra",
  "link a la compra en la web de ARCE" # , "precio unitario del articulo estandarizado en pesos constantes de 2010",
  # "cantidad comprada del articulo estandarizada", 'unidad de presentación del artículo estandarizada'
)

diccionario <- data.frame(
  variable = vars,
  descripcion = desc
)
write.csv(diccionario, "tables/diccionario.csv", row.names = FALSE, na = "")
