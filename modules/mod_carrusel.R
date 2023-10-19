################################################################################
# MODULO CARRUSEL
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

# UI ---------------------------------------------------------------------------
carrusel_ui <- function(id) {
  ns <- NS(id)

  htmlTemplate(
    filename = "www/modules/carrusel/index.html",
    glide_carrusel = glide(
      next_label = icon("chevron-right", lib = "glyphicon"),
      previous_label = icon("chevron-left", lib = "glyphicon"),
      loading_label = icon("hourglass", lib = "glyphicon"),
      height = "685px",
      screen(
        br(), br(), ### CUANTO ####
        p("QUIÉNES COMPRAN"),
        p(""),
        p(quien_compra),
        tabsetPanel(
          tabPanel("2017", br(), shinycssloaders::withSpinner(
            echarts4rOutput(ns("plot_bar_q1")), color = "white"
            )
          ),
          tabPanel("2018", br(), echarts4rOutput(ns("plot_bar_q2"))),
          tabPanel("2019", br(), echarts4rOutput(ns("plot_bar_q3"))),
          tabPanel("2020", br(), echarts4rOutput(ns("plot_bar_q4"))),
          tabPanel("2021", br(), echarts4rOutput(ns("plot_bar_q5")))
        )
      ),
      screen(
        br(), br(), ### CUANDO ####
        p("CUÁNDO COMPRA"), #
        p(""),
        p(cuando_compra), 
        tabsetPanel(
          tabPanel(
            "Administración Central",
            column(8, plotlyOutput(ns("plot_calen_1"))),
            column(4, br(), DTOutput(ns("tab_calen_1")))
          ),
          tabPanel(
            "Gobiernos Departamentales",
            column(8, plotlyOutput(ns("plot_calen_2"))),
            column(4, br(), DTOutput(ns("tab_calen_2")))
          ),
          tabPanel(
            "Entes autónomos",
            column(8, plotlyOutput(ns("plot_calen_3"))),
            column(4, br(), DTOutput(ns("tab_calen_3")))
          ),
          tabPanel(
            "Servicios descentralizados",
            column(8, plotlyOutput(ns("plot_calen_4"))),
            column(4, br(), DTOutput(ns("tab_calen_4")))
          ),
          tabPanel(
            "Otros",
            column(8, plotlyOutput(ns("plot_calen_5"))),
            column(4, br(), DTOutput(ns("tab_calen_5")))
          )
        )
      ),
      screen(
        br(), br(), ### COMO ####
        p("CÓMO COMPRA"),
        p(""),
        p(como_compra),
        tabsetPanel(
          tabPanel("Monto", br(), echarts4rOutput(ns("plot_bar_c1"))),
          tabPanel("Cantidad", br(), echarts4rOutput(ns("plot_bar_c2")))
        )
      ),
      screen(
        br(), br(), ### A QUIENES COMPRA ####
        p("A QUIÉNES COMPRA"),
        p(""),
        p(a_quien_compra),
        tabsetPanel(
          tabPanel("2017", br(), echarts4rOutput(ns("plot_sankey_1"))),
          tabPanel("2018", br(), echarts4rOutput(ns("plot_sankey_2"))),
          tabPanel("2019", br(), echarts4rOutput(ns("plot_sankey_3"))),
          tabPanel("2020", br(), echarts4rOutput(ns("plot_sankey_4"))),
          tabPanel("2021", br(), echarts4rOutput(ns("plot_sankey_5")))
        )
      ),
      screen(
        br(), br(), ### A QUIENES VENDE ####
        p("A QUIÉNES VENDE"),
        p(""),
        p(a_quienes_vende),
        tabsetPanel(
          tabPanel("2017", br(), echarts4rOutput(ns("plot_sankey_v1"))),
          tabPanel("2018", br(), echarts4rOutput(ns("plot_sankey_v2"))),
          tabPanel("2019", br(), echarts4rOutput(ns("plot_sankey_v3"))),
          tabPanel("2020", br(), echarts4rOutput(ns("plot_sankey_v4"))),
          tabPanel("2021", br(), echarts4rOutput(ns("plot_sankey_v5"))),
        )
      ),
      screen(
        br(), br(), ### QUE COMPRA ####
        p("QUÉ COMPRA"),
        p(""),
        p(que_compran),
        tabsetPanel(
          tabPanel("2017", plotlyOutput(ns("plot_tree_1"))),
          tabPanel("2018", plotlyOutput(ns("plot_tree_2"))),
          tabPanel("2019", plotlyOutput(ns("plot_tree_3"))),
          tabPanel("2020", plotlyOutput(ns("plot_tree_4"))),
          tabPanel("2021", plotlyOutput(ns("plot_tree_5")))
        )
      ),
      screen(
        br(), br(), ### A CUANTO ####
        p("A CUÁNTO COMPRA"),
        p(""),
        p(a_cuanto_compra),
        tabsetPanel(
          tabPanel("Notebook", plotlyOutput(ns("plot_violin_1"))),
          tabPanel("Tapaboca", plotlyOutput(ns("plot_violin_2"))),
          tabPanel("Agua mineral", plotlyOutput(ns("plot_violin_3"))),
          tabPanel("Café", plotlyOutput(ns("plot_violin_4"))),
          tabPanel("Tóner para fotocopiadora", plotlyOutput(ns("plot_violin_5")))
        )
      )
    )
  )
}

# SERVER -----------------------------------------------------------------------
carrusel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ### CUANTO ####

    output$plot_bar_q1 <- renderEcharts4r({
      # echarts_bar(data_bar_1)
      data_bar_1 %>%
        echarts_bar(
          vary = "organismo_sigla", varx = "monto_total", l = "20%",
          text = "Monto comprado por organismo (en millones de $)",
          color = "#96B9D0"
        )
    })

    output$plot_bar_q2 <- renderEcharts4r({
      data_bar_2 %>%
        echarts_bar(
          vary = "organismo_sigla", varx = "monto_total", l = "20%",
          text = "Monto comprado por organismo (en millones de $)",
          color = "#96B9D0"
        )
    })

    output$plot_bar_q3 <- renderEcharts4r({
      data_bar_3 %>%
        echarts_bar(
          vary = "organismo_sigla", varx = "monto_total", l = "20%",
          text = "Monto comprado por organismo (en millones de $)",
          color = "#96B9D0"
        )
    })

    output$plot_bar_q4 <- renderEcharts4r({
      data_bar_4 %>%
        echarts_bar(
          vary = "organismo_sigla", varx = "monto_total", l = "20%",
          text = "Monto comprado por organismo (en millones de $)",
          color = "#96B9D0"
        )
    })

    output$plot_bar_q5 <- renderEcharts4r({
      data_bar_5 %>%
        echarts_bar(
          vary = "organismo_sigla", varx = "monto_total", l = "20%",
          text = "Monto comprado por organismo (en millones de $)",
          color = "#96B9D0"
        )
    })

    ### CUANDO ####
    output$tab_calen_1 <- renderDT({
      data_heatmap_1 %>%
        group_by(organismo_sigla) %>%
        summarise(monto_total = sum(monto_total)) %>%
        set_DT(colnames = c("Organismo", "Monto total"), buttons = FALSE, dom = 'ftip') %>%
        formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$plot_calen_1 <- renderPlotly({
      data_heatmap_1 %>% plotly_calendar()
    })

    output$tab_calen_2 <- renderDT({
      data_heatmap_2 %>%
        group_by(organismo_sigla) %>%
        summarise(monto_total = sum(monto_total)) %>%
        set_DT(colnames = c("Organismo", "Monto total"), buttons = FALSE, dom = 'ftip') %>%
        formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$plot_calen_2 <- renderPlotly({
      data_heatmap_2 %>% plotly_calendar()
    })

    output$tab_calen_3 <- renderDT({
      data_heatmap_3 %>%
        group_by(organismo_sigla) %>%
        summarise(monto_total = sum(monto_total)) %>%
        set_DT(colnames = c("Organismo", "Monto total"), buttons = FALSE, dom = 'ftip') %>%
        formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$plot_calen_3 <- renderPlotly({
      data_heatmap_3 %>% plotly_calendar()
    })

    output$tab_calen_4 <- renderDT({
      data_heatmap_4 %>%
        group_by(organismo_sigla) %>%
        summarise(monto_total = sum(monto_total)) %>%
        set_DT(colnames = c("Organismo", "Monto total"), buttons = FALSE, dom = 'ftip') %>%
        formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$plot_calen_4 <- renderPlotly({
      data_heatmap_4 %>% plotly_calendar()
    })

    output$tab_calen_5 <- renderDT({
      data_heatmap_5 %>%
        mutate(organismo_sigla = ifelse(organismo_sigla == "Instituciones sin fines de lucro públicas", "ISFL Públicas", organismo_sigla)) %>%
        group_by(organismo_sigla) %>%
        summarise(monto_total = sum(monto_total)) %>%
        set_DT(colnames = c("Organismo", "Monto total"), buttons = FALSE, dom = 'ftip') %>%
        formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$plot_calen_5 <- renderPlotly({
      data_heatmap_5 %>% plotly_calendar()
    })
    ### A QUIEN ####

    output$plot_sankey_1 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_1, source = "organismo_sigla", target = "empresa",
        value = "M", text = "Principales proveedores de Presidencia"
      )
    })

    output$plot_sankey_2 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_2, source = "organismo_sigla", target = "empresa", 
        value = "M", text = "Principales proveedores de Presidencia"
      )
    })

    output$plot_sankey_3 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_3, source = "organismo_sigla", target = "empresa",
        value = "M", text = "Principales proveedores de Presidencia"
      )
    })

    output$plot_sankey_4 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_4, source = "organismo_sigla", target = "empresa",
        value = "M", text = "Principales proveedores de Presidencia"
      )
    })

    output$plot_sankey_5 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_5, source = "organismo_sigla", target = "empresa",
        value = "M", text = "Principales proveedores de Presidencia"
      )
    })

#  A QUIENES VENDE ####
    output$plot_sankey_v1 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_v1, "denominacion_social", "organismo_sigla",
        value = "M", text = "Principales proveedores del Estado"
      )
    })

    output$plot_sankey_v2 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_v2, "denominacion_social", "organismo_sigla",
        value = "M", text = "Principales proveedores del Estado"
      )
    })

    output$plot_sankey_v3 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_v3, "denominacion_social", "organismo_sigla",
        value = "M", text = "Principales proveedores del Estado"
      )
    })

    output$plot_sankey_v4 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_v4, "denominacion_social", "organismo_sigla",
        value = "M", text = "Principales proveedores del Estado"
      )
    })

    output$plot_sankey_v5 <- renderEcharts4r({
      echarts_sankey(
        data_sankey_v5, "denominacion_social", "organismo_sigla",
        value = "M", text = "Principales proveedores del Estado"
      )
    })

# COMO COMPRA ####
    output$plot_bar_c1 <- renderEcharts4r({
      m <- max(data_bartime$monto_total)
      echarts_bartime(
        data = data_bartime, varx = "monto_total", maxim = m,
        text = "Monto total comprado por tipo de compra (en millones de $)"
      )
    })

    output$plot_bar_c2 <- renderEcharts4r({
      m <- max(data_bartime$n)
      echarts_bartime(
        data = data_bartime, varx = "n", maxim = m,
        text = "Cantidad de compras por tipo de compra"
      )
    })

# QUE COMPRA ####
    output$plot_tree_1 <- renderPlotly({
     data_treemap_1 %>%
        plotly_treemap(
          varx = organismo_sigla, vary = articulo, yalign = 0.9, xalign = 0.2,
          text = "Artículos más comprados en 2017 según organismo"
          )
    })

  output$plot_tree_2 <- renderPlotly({
      data_treemap_2 %>%
        plotly_treemap(
          varx = organismo_sigla, vary = articulo, yalign = 0.9, xalign = 0.2,
          text = "Artículos más comprados en 2018 según organismo"
        )
    })

  output$plot_tree_3 <- renderPlotly({
      data_treemap_3 %>%
        plotly_treemap(
          varx = organismo_sigla, vary = articulo, yalign = 0.9, xalign = 0.2,
          text = "Artículos más comprados en 2019 según organismo"
        )
    })

    output$plot_tree_4 <- renderPlotly({
      data_treemap_4 %>%
        plotly_treemap(
          varx = organismo_sigla, vary = articulo, yalign = 0.9, xalign = 0.2,
          text = "Artículos más comprados en 2020 según organismo"
        )
    })

    output$plot_tree_5 <- renderPlotly({
      data_treemap_5 %>%
        plotly_treemap(
          varx = organismo_sigla, vary = articulo, yalign = 0.9, xalign = 0.2,
          text = "Artículos más comprados en 2021 según organismo"
        )
    })

# A QUE PRECIO
    output$plot_violin_1 <- renderPlotly({
      data_violin_1 %>%
        plotly_violin(text = "Distribución del precio pagado por una notebook",
                      xalign = 0.195
                     )
    })

    output$plot_violin_2 <- renderPlotly({
      data_violin_2 %>%
        plotly_violin(text = "Distribución del precio pagado por una caja de tapabocas",
                      xalign = 0.195
        )
    })

    output$plot_violin_3 <- renderPlotly({
      data_violin_3 %>%
        plotly_violin(text = "Distribución del precio pagado por un litro de agua mineral",
                      xalign = 0.22
        )
    })

    output$plot_violin_4 <- renderPlotly({
      data_violin_4 %>%
        plotly_violin(text = "Distribución del precio pagado por un kilo de café",
                      xalign = 0.195
          )
    })

    output$plot_violin_5 <- renderPlotly({
      data_violin_5 %>%
        plotly_violin(text = "Distribución del precio pagado por un tóner de fotocopiadora",
                      xalign = 0.24
          )
    })
    ## --------- the end ----------##
  })
}
