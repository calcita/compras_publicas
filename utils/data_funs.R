################################################################################
# FUNCIONES DATOS
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

# BAR_DATA ---------------------------------------------------------------------
bar_data <- function(data = compras_resumen_so,
                     year = 2017) {
  d <- data %>%
    dplyr::filter(duplicated(ocid) == F) %>%
    dplyr::filter(anio == year) %>%
    dplyr::group_by(organismo_codigo) %>%
    dplyr::summarise(monto_total = sum(monto_compra), .groups = 'drop') %>%
    dplyr::top_n(15, monto_total) %>%
    dplyr::arrange(monto_total) %>%
    dplyr::mutate(monto_total = round(monto_total / 1e6, 0)) %>%
    dplyr::left_join(., organismos, by = "organismo_codigo")

  return(d)
}

# BARTIME_DATA()----------------------------------------------------------------
bartime_data <- function(data = compras_resumen) {
  d <- data %>%
    dplyr::filter(!duplicated(ocid) & monto_compra > 0) %>%
    dplyr::left_join(., tipo_compra_codiguera, by = "tipo_compra_codigo") %>%
    dplyr::group_by(anio, tipo_compra) %>%
    dplyr::summarise(
      n = n(),
      monto_total = sum(monto_compra),
      monto_total = round(monto_total / 1e6, 1),
      .groups = "drop"
    ) %>%
    dplyr::arrange(monto_total) %>%
    dplyr::ungroup()
  
  return(d)
}


# SANKEY_DATA ------------------------------------------------------------------
sankey_data <- function(data = NULL,
                        organismo = "Presidencia",
                        year = NA_integer_,
                        top = "5",
                        unidad = "M") {
  if (!is.na(year)) {
    n <- anios_codiguera %>%
      dplyr::filter(anio == year) %>%
      dplyr::select(cod_anio) %>%
      dplyr::pull()
    data <- rbindlist(compras_all[n]) %>%
      dplyr::filter(organismo_codigo == as.character(organismos[organismo_sigla == {{ organismo }}, "organismo_codigo"])) %>%
      dplyr::left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
      dplyr::left_join(., proveedores, by = "codigo_fiscal") %>%
      dplyr::mutate(
        organismo_sigla = {organismo},
        anio = year(fecha)
        )
  }
  top <- as.numeric(top)

  d <- data %>%
    dplyr::group_by(organismo_sigla, denominacion_social) %>%
    dplyr::summarise(
      monto_total = sum(monto_item_total),
      q_compras = n(),
      .groups = 'drop'
    ) %>%  
    dplyr::ungroup()

  if (unidad == "M") {
    d_top <- d %>%
      dplyr::top_n(monto_total, n = top) %>%
      dplyr::arrange(monto_total) %>%
      dplyr::select(denominacion_social)
    
    d <- d %>%
      dplyr::mutate(
        empresa = case_when(
          denominacion_social %in% d_top$denominacion_social ~ denominacion_social,
          TRUE ~ "Otros"
        ),
        empresa = fct_relevel(as_factor(empresa), "Otros", after = Inf)
      ) %>%
      dplyr::group_by(organismo_sigla, empresa) %>%
      dplyr::mutate(
        total = sum(monto_total, na.rm = T),
        total = round(total / 1e3, 1)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!duplicated(empresa)) %>%
      dplyr::select(organismo_sigla, denominacion_social, total, empresa) %>%
      dplyr::mutate(
        prop = round(total / sum(total, na.rm = T) * 100, 2),
        denominacion_social = paste(empresa, paste0(prop, "%"), sep = "\n")
      )
  } else {
    d_top <- d %>%
      dplyr::top_n(q_compras, n = top) %>%
      dplyr::arrange(q_compras) %>%
      dplyr::select(denominacion_social)
    
    d <- d %>%
      dplyr::mutate(
        empresa = case_when(
          denominacion_social %in% d_top$denominacion_social ~ denominacion_social,
          TRUE ~ "Otros"
        ),
        empresa = fct_relevel(as_factor(empresa), "Otros", after = Inf)
      ) %>%
      dplyr::group_by(organismo_sigla, empresa) %>%
      dplyr::mutate(q = sum(q_compras, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!duplicated(empresa)) %>%
      dplyr::select(organismo_sigla, denominacion_social, q, empresa) %>%
      dplyr::mutate(
        prop = round(q / sum(q, na.rm = T) * 100, 2),
        denominacion_social = paste(empresa, paste0(prop, "%"), sep = "\n")
      )
  }
  return(d)
}

# SANKEY_DATA_PROV()------------------------------------------------------------
sankey_data_prov <- function(data = NULL,
                             year = NA_integer_,
                             top = 5,
                             unidad = "M") {
  top <- as.numeric(top)

  if (!is.na(year)) {
    n <- anios_codiguera %>%
      dplyr::filter(anio %in% year) %>%
      dplyr::select(cod_anio) %>%
      dplyr::pull()
    data <- rbindlist(compras_all[n])
    data <- data %>%
      dplyr::left_join(., proveedores, by = "codigo_fiscal") %>%
      dplyr::left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
      dplyr::mutate(
        anio = year(fecha),
        mes = lubridate::month(fecha, abbr = FALSE, label = TRUE)
      )
  }

  if (unidad == "M") {
    d <- data %>%
      dplyr::group_by(denominacion_social, organismo_sigla) %>%
      dplyr::summarise(monto_total = sum(monto_item_total), .groups = 'drop') %>%
      dplyr::ungroup()

    d_top <- d %>%
      dplyr::group_by(denominacion_social) %>%
      dplyr::summarise(monto_total = sum(monto_total), .groups = 'drop') %>%
      dplyr::ungroup() %>%
      dplyr::top_n(monto_total, n = top) %>%
      dplyr::arrange(monto_total) %>%
      dplyr::select(denominacion_social)

    d <- d %>%
      dplyr::mutate(empresa = case_when(
        denominacion_social %in% d_top$denominacion_social ~ denominacion_social,
        TRUE ~ "Otros"
      )) %>%
      dplyr::group_by(denominacion_social, organismo_sigla) %>%
      dplyr::summarise(total = sum(monto_total, na.rm = T), .groups = "drop") %>%
      dplyr::arrange(desc(total)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        prop = round(total / sum(total) * 100, 2),
        organismo_sigla = paste(organismo_sigla, paste0(prop, "%"), sep = "\n")
      )
  } else {
    d <- data %>%
      dplyr::group_by(denominacion_social, organismo_sigla) %>%
      dplyr::summarise(cantidad = n(), .groups = 'drop') %>%
      dplyr::ungroup()

    d_top <- d %>%
      dplyr::group_by(denominacion_social) %>%
      dplyr::summarise(cantidad = n(), .groups = 'drop') %>%
      dplyr::ungroup() %>%
      dplyr::top_n(cantidad, n = top) %>%
      dplyr::arrange(cantidad) %>%
      dplyr::select(denominacion_social)

    d <- d %>%
      dplyr::mutate(
        empresa = case_when(
          denominacion_social %in% d_top$denominacion_social ~ denominacion_social,
          TRUE ~ "Otros"
        )
      ) %>%
      dplyr::group_by(denominacion_social, organismo_sigla) %>%
      dplyr::summarise(q = sum(cantidad, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::arrange(desc(q)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        prop = round(q / sum(q) * 100, 2),
        organismo_sigla = paste(organismo_sigla, paste0(prop, "%"), sep = "\n")
      )
  }
  return(d)
}

# DATA_LINES()------------------------------------------------------------------
data_lines <- function(data) {
  d <- data %>%
    dplyr::group_by(fecha) %>%
    dplyr::mutate(cantidad = n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(fecha) %>%
    tidyr::complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
    dplyr::mutate(
      monto_item_total = replace_na(monto_item_total, 0),
      monto = monto_item_total / 1e3,
      cantidad = replace_na(cantidad, 0)
    ) %>%
    dplyr::group_by(fecha) %>%
    dplyr::summarise(
      cantidad = max(cantidad),
      monto = round_off(sum(monto), 1), .groups = 'drop'
    ) 
  
  return(d)
}


# TREEMAP_DATA_ARTS()--------------------------------------------------------------
treemap_data_arts <- function(year = 2017,
                           org = NULL) {
  
  data <- compras_all[[paste0("compras_", year)]] %>%
    dplyr::filter(!is.na(iditem)) %>%
    dplyr::filter(cod_familia != 6) %>%
    dplyr::left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
    dplyr::left_join(., proveedores, by = "codigo_fiscal") %>%
    dplyr::left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
    dplyr::left_join(., articulos %>% select(articulo, iditem), by = "iditem") %>%
    dplyr::filter(!is.na(articulo))
  
  if (!is.null(org)) {
    data <- data %>%
      dplyr::filter(organismo_sigla == {{ org }})
  }
  
  cc <- data %>%
    dplyr::group_by(articulo) %>%
    dplyr::summarise(monto_total = sum(monto_item_total, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(monto_total = round(monto_total / 1e6, 1), trim = TRUE) %>%
    dplyr::top_n(monto_total, n=10) %>%
    dplyr::arrange(desc(monto_total)) %>%
    dplyr::select(articulo, Total = monto_total) %>%
    dplyr::mutate(articulo = stringr::str_to_sentence(articulo, locale = "es"))
  
  d <- data %>%
    dplyr::filter(articulo %in% toupper(cc$articulo)) %>%
    dplyr::group_by(articulo, organismo_sigla) %>%
    dplyr::summarise(monto_total = sum(monto_item, na.rm = T), .groups = 'drop') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(monto_total = round(monto_total / 1e6, 1)) %>%
    dplyr::select(articulo, Total = monto_total, organismo_sigla)
  
  return(d)
}

# TREEMAP_DATA()----------------------------------------------------------------
treemap_data <- function(data = NULL,
                              organism = "Presidencia",
                              #year = 2021,
                              var1 = "familia", 
                              var2 = "sub_familia",
                              counts = "monto_item_total") {
  
  #data <- tree_data_arts(year = year) 
  d <- data %>%
    dplyr::mutate(
      ids = ifelse(.data[[var1]] == "", .data[[var2]],
        paste0(.data[[var2]], "-", .data[[var1]])
      )
    ) %>%
    dplyr::select(ids, all_of(var1), all_of(var2), all_of(counts))

  par_info <- d %>%
    dplyr::group_by(!!!syms(var1)) %>% # group by parent
    dplyr::summarise(!!counts := sum(.data[[counts]]), .groups = 'drop') %>% # parent total
    dplyr::ungroup() %>% 
    dplyr::rename(!!var2 := all_of(var1)) %>% # parent labels for the item field
    dplyr::mutate(!!var1 := "", ids = .data[[var2]]) %>% # add missing fields for my_data
    dplyr::select(names(d))

  data_treemap_arts <- rbind(d, par_info)

  return(data_treemap_arts)
}

# VIOLIN_DATA()-----------------------------------------------------------------
violin_data <- function(data = precios_unit,
                        year_from = 2017,
                        year_to = 2021,
                        item = NA_character_) {
  articulo <- articulos %>%
    dplyr::filter(articulo == {
      item
    }) %>%
    dplyr::select(iditem) %>%
    dplyr::pull()

  d <- data %>%
    dplyr::filter(anio %in% year_from:year_to & iditem == articulo)

  # sin outliers
  Q <- quantile(d$monto_estandar, probs = c(.05, .95), na.rm = FALSE)

  d <- d %>%
    dplyr::filter(monto_estandar > Q[1] & monto_estandar < Q[2]) %>%
    dplyr::mutate(
      monto_estandar = round_off(monto_estandar),
      anio = as.factor(anio)
    ) %>%
    dplyr::select(-ocid, -fecha)

  return(d)
}

# SET_DT()----------------------------------------------------------------------
set_DT <- function(t,
                    info = TRUE,
                    paging = TRUE,
                    searching = FALSE,
                    pages = 6,
                    font.size = '0.8',
                    buttons = TRUE,
                    dom = "Bfrtip",
                    responsive = FALSE,
                    selection = list(mode = "single"),
                    class = "display compact",
                    caption = "",
                    rownames = FALSE,
                    colnames = NULL,
                    width = "100%") {

  if(isTRUE(buttons)){
    extensions = "Buttons"
  } else {
    extensions = list()
  }
  
   options = list(
      pageLength = pages, responsive = responsive, dom = dom, #"Bfrtip", #
      searching = searching, info = info, paging = paging,
      buttons = list(
        list(extend = "csv", text = "csv"),
        list(extend = "excel", text = "excel"),
        list(extend = "pdf", text = "pdf"),
        list(extend = "print", text = "imprimir")
      ),
       headerCallback = DT::JS(
          "function(thead) {",
          paste0("  $(thead).css('font-size', '", font.size, "em');"),
          "}"
        ),
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "em'});"),
        "}"
      ),
      language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json")
     )

   DT::datatable(t,
                extensions = extensions,
                options = options, 
                selection = selection,
                width = width, 
                rownames = rownames, 
                caption = caption, 
                colnames = colnames
  ) %>% 
    DT::formatStyle(columns = 1:length(colnames), fontFamily = "ubuntu")
  
  #return(dt)
}

calendar_filter <- function(category = "Administración Central") {
  d <- compras_resumen %>%
    left_join(., organismos %>% select(organismo_sigla, organismo_codigo, categoria), by = "organismo_codigo") %>%
    filter(!duplicated(ocid) & anio %in% 2017:2021 & categoria == {{ category }}) %>%
    mutate(trimestre = lubridate::quarter(fecha, type = "year.quarter"),
           anio = year(fecha)) %>% 
    group_by(organismo_sigla, trimestre) %>%
    summarise(monto_total = sum(monto_compra), .groups = 'drop') %>%
    ungroup() %>% 
    mutate(monto_total = round(monto_total / 1e6, 0),
           anio = as.integer(substr(trimestre, 1, 4)),
           tri = as.integer(substr(trimestre, 6, 6)), 
           Trimestre = case_when(
              anio == 2017 ~ tri,
              anio == 2018 ~ tri+4,
              anio == 2019 ~ tri+8,
              anio == 2020 ~ tri+12,
              anio == 2021 ~ tri+16,
              TRUE ~ NA_integer_
            )
      )

  return(d)
}

calendar_data <- function( # data = NULL,
  year = NA_integer,
  category = "Administración Central") {
  
  data <- calendar_filter(category = category)
  
  cuartiles <- data %>%
      group_by(anio) %>%
      summarise(cuartiles = quantile(monto_total, probs = seq(0, 1, by = 0.25)))
    
    d <- data %>%
      filter(anio == year) %>%
      mutate(
        monto_cuartil = cut(monto_total,
                            breaks = as.numeric(unlist(cuartiles[cuartiles$anio == year, "cuartiles"])),
                            include.lowest = TRUE,
                            labels = c(paste("Cuartil", 1:4))
        ),
        cuartil = as.integer(substr(monto_cuartil, 9,9)), 
        monto_cuartil = factor(as.character(monto_cuartil),
                               levels = rev(levels(monto_cuartil))
        )
      )
  
  return(d)
}
