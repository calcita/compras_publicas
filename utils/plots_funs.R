################################################################################
# FUNCIONES GRAFICOS
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################
# PLOTS ECHARTS4R --------------------------------------------------------------
e_common(font_family = "Ubuntu", theme = NULL)

# ECHARTS_BAR()-----------------------------------------------------------------
echarts_bar <- function(data = NULL,
                        vary = NA_character_,
                        varx = NA_character_,
                        l = "30%", 
                        text = NA_character_,
                        subtext = NA_character_,
                        color = NA_character_) {
  p <- data %>%
    echarts4r::e_charts_(x = vary, reorder = FALSE) %>%
    echarts4r::e_bar_(varx, legend = FALSE, name = "Compras") %>%
    echarts4r::e_labels(position = "right", color = "#515656") %>%
    echarts4r::e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "ES", digits = 0)) %>%
    # echarts4r::e_format_x_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "ES", digits = 0)) %>%
    echarts4r::e_title(text, subtext, textStyle = list(color = "#515656")) %>%
    echarts4r::e_flip_coords() %>%
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_x_axis(show = FALSE) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Descargar") %>%
    echarts4r::e_grid(left = l) %>%
    echarts4r::e_grid(right = "1%") %>%
    echarts4r::e_grid(width = "100%") %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_tooltip(backgroundColor = "rgba(0,0,0,0)") %>%
    echarts4r::e_color(color = c("#96B9D0")) %>%
    echarts4r::e_locale(locale = "ES")
  return(p)
}

# ECHARTS_BARTIME---------------------------------------------------------------
echarts_bartime <- function(data = NULL,
                            vary = "tipo_compra",
                            varx = "monto_total",
                            maxim = 25000,
                            text = "Monto total comprado por tipo de compra (en millones de $)",
                            subtext = "") {
  data %>%
    dplyr::group_by(anio) %>%
    echarts4r::e_charts_(vary, timeline = TRUE) %>%
    echarts4r::e_bar_(varx, x_index = 0, y_index = 0) %>%
    echarts4r::e_format_y_axis(suffix = "", prefix = "", formatter = echarts4r::e_axis_formatter(locale = "ES", digits = 0)) %>%
    echarts4r::e_y_axis(min = 0, max = maxim) %>%
    echarts4r::e_tooltip(trigger = "axis") %>%
    echarts4r::e_flip_coords() %>%
    echarts4r::e_title(text, subtext, textStyle = list(color = "#515656")) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Descargar") %>%
    echarts4r::e_grid(left = "15%") %>%
    # e_theme("wonderland") %>%
    echarts4r::e_legend(show = FALSE) %>%
    e_color(color = c("#96B9D0")) %>%
    e_timeline_opts(
      autoPlay = TRUE, symbol = "diamond", symbolSize = 13,
      lineStyle = list(color = "#515656", width = 2), label = list(color = "#515656"),
      itemStyle = list(color = "#515656", shadowColor = "#515656"),
      controlStyle = list(color = "#515656"),
      # iconStyle = list(color = '#515656'),
      checkpointStyle = list(symbol = "diamond", color = "#577399", symbolSize = 6)
    )
}

# ECHARTS_LINES()---------------------------------------------------------------
echarts_lines <- function(data = NULL,
                          varx = "fecha",
                          vary = NA_character_,
                          value = "M",
                          text = "") {
  if (value == "M" & is.na(vary)) {
    vary <- "monto"
  } else if (value == "C" & is.na(vary)) {
    vary <- "cantidad"
  } else {
    vary <- vary
  }

  data %>%
    echarts4r::e_charts_(varx) %>%
    echarts4r::e_line_(vary) %>% # , right=r, top=t, bottom=b
    echarts4r::e_x_axis_(type = "category") %>%
    echarts4r::e_y_axis_(offset = -10) %>%
    echarts4r::e_format_y_axis(suffix = "", prefix = "", formatter = e_axis_formatter(locale = "ES", digits = 0)) %>%
    echarts4r::e_legend(show = FALSE) %>%
    echarts4r::e_tooltip(formatter = format_number_es(1)) %>%
    echarts4r::e_title(text, textStyle = list(color = "#515656")) %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Descargar") %>%
    echarts4r::e_grid(left = "10%") %>%
    echarts4r::e_grid(right = "1%") %>%
    echarts4r::e_grid(width = "100%") %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_color(color = c("#B6D0E2")) %>%
    echarts4r::e_datazoom(type = "slider", toolbox = FALSE, bottom = -5)
}

# ECHARTS_SANKEY()--------------------------------------------------------------
echarts_sankey <- function(data = NULL,
                           source,
                           target,
                           value = "M", 
                           count = FALSE,
                           r = "0%",
                           t = "25%",
                           b = "0%",
                           text = NA_character_,
                           subtext = NA_character_) {
  
  if (isTRUE(count)) {
    data <- data %>%
      dplyr::group_by(!!!syms(source), !!!syms(target)) %>%
      dplyr::summarise(monto_total = sum(!!!syms(value)), .groups = 'drop') %>%
      dplyr::ungroup()
  }

  if (value == "M") {
    value <- "total"
  } else {
    value <- "q"
  }

  p <- data %>%
    echarts4r::e_charts_(reorder = FALSE) %>%
    echarts4r::e_sankey_(source, target, value, right = r, top = t, bottom = b) %>% 
    echarts4r::e_title(text, subtext, textStyle = list(color = "#515656")) %>%
    echarts4r::e_tooltip(layoutIterations = 0, draggable = TRUE) %>%
    echarts4r::e_labels(position = "right", color = "white") %>%
    echarts4r::e_toolbox_feature(feature = "saveAsImage", title = "Descargar")
  
  return(p)
}

# FUNCIONES PLOTLY -------------------------------------------------------------
# SET_CONFIG -------------------------------------------------------------------
set_config <- function(p,
                       modeBarButtons = list(list("toImage",
                                                  "zoomIn2d",
                                                  "zoomOut2d")),
                       toImageButtonOptions = list(format = "png",
                                                   filename = "",
                                                   width = 700,
                                                   height = 900),
                       ...) {
  plotly::config(p,
                 displaylogo = FALSE,
                 locale = "es",
                 modeBarButtons = modeBarButtons,
                 toImageButtonOptions = toImageButtonOptions,
                 ...
  )
}

# SET_LAYOUT -------------------------------------------------------------------
set_layout <- function(p,
                       title = list(text = "", 
                                    y = 1, 
                                    x = 0, 
                                    xanchor = "center",
                                    yanchor = "bottom"),
                       font = list(family = "ubuntu",
                                   size = 13,
                                   color = "#515656"),
                       show_lines = TRUE,
                       showlegend = FALSE,
                       margin = list(t = 60, pad = 20),
                       plot_bgcolor = "rgba(0,0,0,0)",
                       paper_bgcolor = "rgba(0,0,0,0)",
                       modebar = list(bgcolor = "transparent", 
                                      color = "gray", 
                                      activecolor = "#c2dae1"),
                       ...) {
  if (isTRUE(show_lines)) {
    xaxis <- list(showgrid = FALSE, title = "", zeroline = TRUE, 
                  showline = TRUE, tickfont = list(color = "#515656"),
                  linecolor = "#515656"
    )
    yaxis <- list(showgrid = FALSE, title = "", tickformat = "digits", 
                  showline = TRUE, tickfont = list(color = "#515656"),
                  linecolor = "#515656"
    )
  } else {
    xaxis <- list(showgrid = FALSE, title = "", zeroline = FALSE, 
                  showline = FALSE, tickfont = list(color = "#515656"),
                  linecolor = "#515656"
    )
    yaxis <- list(showgrid = FALSE, title = "", tickformat = "digits",
                  showline = FALSE, tickfont = list(color = "#515656"),
                  linecolor = "#515656"
    )
  }
  plotly::layout(p,
                 title = title,
                 font = font,
                 xaxis = xaxis,
                 yaxis = yaxis,
                 margin = margin,
                 showlegend = showlegend,
                 plot_bgcolor = plot_bgcolor,
                 paper_bgcolor = paper_bgcolor,
                 modebar = modebar,
                 # layout.yaxis.tickformat = layout.yaxis.tickformat,
                 # layout.separators = layout.separators,
                 ...
  )
}

# PLOTLY_TREEMAP()--------------------------------------------------------------
plotly_treemap <- function(data,
                           cantidad = FALSE,
                           varx = sub_familia,
                           vary = familia,
                           width = NULL,
                           height = NULL,
                           text = NA_character_,
                           xalign = NA_real_,
                           yalign = NA_real_) {
  
  varx <- eval(substitute(varx), data)
  vary <- eval(substitute(vary), data)

  if (isFALSE(cantidad)) {
    p <- data %>%
      plotly::plot_ly(
        branchvalues = "total", type = "treemap", labels = varx,
        parents = vary, values = ~ .data$Total, ids = ~ .data$ids,
        opacity = 0.7,
        hovertext = formatC("%{value}", digits = 2, big.mark = ".",
                            decimal.mark = ",", format = "d"
                            ),
        hovertemplate = "%{label}<br>Total: %{value}<extra></extra>",
        width = width,
        height = height
      )
  }
  if (isTRUE(cantidad)) {
    p <- data %>%
      plotly::plot_ly(
        branchvalues = "total", type = "treemap", labels = varx,
        parents = vary, values = ~ .data$Cantidad, ids = ~ .data$ids,
        opacity = 0.7,
        hovertemplate = "%{label}<br>Total: %{value}<extra></extra>",
        width = width, height = height
      )
  }
  p <- p %>% 
    set_layout(
      title = list(
        text = paste0("<b>", text, "</b>"),
        y = yalign, x = xalign, xanchor = "center", yanchor = "bottom"
      ),
      margin = list(t = 45, l = 0, r = 5, pad = 1)
    ) %>% 
    set_config() 
  return(p)
}


#
# plotly_scatter <- function(data = NULL,
#                            varx,
#                            #vary,
#                            namey){
#   
#   varx <- eval(substitute(varx), data)
#   #vary <- eval(substitute(vary), data)
#   namey <- eval(substitute(namey), data)
#   
#   data %>% 
#     plotly::plot_ly(
#       x = ~varx, y = ~ data[[namey]], type = "scatter",
#       mode = "none", stackgroup = "one", fillcolor = color_am,
#       name = namey
#     ) %>%
#     set_config() %>%
#     set_layout(title = "Monto total vendido por año (miles de pesos)", 
#                showlegend = TRUE,
#                show_lines = FALSE
#     )
# }
# PLOTLY_VIOLIN() --------------------------------------------------------------
plotly_violin <- function(data = NULL,
                          varx = anio,
                          vary = monto_estandar,
                          text = NA_character_,
                          xalign = NA_real_){
  
  varx <- eval(substitute(varx), data)
  vary <- eval(substitute(vary), data)
  
  p <- 
    data %>% 
    plotly::plot_ly(
    x = ~varx, y = ~vary, split = ~varx, type = "violin",
    box = list(visible = T), meanline = list(visible = F), spanmode = "hard",
    text = ~vary, hoverinfo = "y"
    ) %>%
    set_layout(
      font = list(color = "#515656"),
      title = list(
        text = paste0('<b>', text, '</b>'),
        y = 0.90, x = xalign, xanchor = "center", yanchor = "bottom"
      ),
      margin = list(t = 65, l = 0, r = 5, pad = 1),
      show_lines = FALSE
    ) %>%
    set_config()
  return(p)
}

# PLOTLY_CALENDAR()-------------------------------------------------------------
plotly_calendar <- function(data = NULL){
  data %>% 
    plotly::plot_ly(text = text) %>%
    plotly::add_heatmap(x = ~as.factor(trimestre), 
                        y = ~organismo_sigla,
                        z = ~cuartil,
                        xgap = 0.5,
                        ygap = 0.5,
                        colors = rev(paleta_azul),
                        text = ~paste0("Organismo: ", organismo_sigla, "<br>",
                                       "Monto: ", monto_total, "<br>",
                                       monto_cuartil),
                        hoverinfo ="text"
    ) %>% 
    hide_colorbar() %>% 
    set_layout(
      title = list(
        text = paste0("<b>", "Distribución por cuartiles trimestrales de montos según organismo", "</b>"),
        y = 0.90, x = 0.407, xanchor = "center", yanchor = "bottom"
      ),
      show_lines = FALSE
    ) %>%
    set_config()
}
