################################################################################
# MODULO EXPLORAR
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

# UI----------------------------------------------------------------------------

explorar_ui <- function(id) { #
  ns <- NS(id)

  htmlTemplate(
    filename = "www/modules/interactivo/index.html",
    panel_interactivo = fluidPage(
      tags$head(includeHTML("google-analytics.html")),
      includeCSS("www/css/details.css"),
      fluidRow(
        useShinyjs(),
        column(
          width = 3
        ),
        column(
          width = 6,
          prettyRadioButtons("nivel", "",
            c("ORGANISMOS" = "org", "PROVEEDORES" = "prov", "PRODUCTOS" = "arts"),
            status = "default",
            inline = TRUE # ,
            # selected="org"
          )
        ),
        column(
          width = 3
        )
      ),

      #### --------------- ORGANISMOS ---------------- ################
      conditionalPanel(
        condition = "input.nivel == 'org'",
        fluidRow(
          column(
            width = 4,
            selectInput(ns("orga"), label = "", multiple = F, choices = organismos_lista, selected = "MEF") # , width="90%"
          ),
          column(
            width = 4,
            selectInput(ns("anio.o1"), label = "", multiple = TRUE, choices = anios, selected = 2021)
          ),
          column(
            width = 4,
            radioGroupButtons(ns("tiempo"),
              choices = c("ACUMULADO" = "o.acumulado", "ANUAL" = "o.anual"),
              selected = "o.anual", size = "sm", justified = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 4, # offset =0 , style='padding-right: 0;padding-left: 0;',
            radioGroupButtons(
              "orga_qcc",
              choices = c("CUÁNTO" = "o.cuanto", "CUÁNDO" = "o.cuando", "A QUIÉN" = "o.aquien", "CÓMO" = "o.como"),
              selected = "o.cuanto", size = "sm", justified = TRUE, checkIcon = list(inline = TRUE)
            ),
          ),
          column(
            width = 4,
            radioGroupButtons(ns("mc.o1"),
              choices = c("MONTO" = "M", "CANTIDAD" = "C"),
              selected = "M", size = "sm", justified = TRUE # , checkIcon = list(yes = icon("ok", lib = "glyphicon"), inline = TRUE)
            )
          ),
          column(
            width = 4, style = style_checkbox,
            checkboxInput(ns("obras"), label = "EXCLUIR OBRAS", value = FALSE, width = "400px")
          )
        ),
        conditionalPanel(
          condition = "input.orga_qcc=='o.cuanto'",
          fluidRow(
            column(12, uiOutput(ns("tabs_cuanto_o")))
          )
        ),
        conditionalPanel(
          condition = "input.orga_qcc=='o.cuando'",
          fluidRow(
            column(
              width = 12,
              prettyRadioButtons(
                inputId = ns("plot_type"), label = "FRECUENCIA", status = "default",
                choices = c("DIARIO", "MENSUAL"), inline = TRUE
              ),
              uiOutput(ns("tabs_cuando_o"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.orga_qcc=='o.aquien'",
          fluidRow(
            column(
              width = 12,
              prettyRadioButtons(ns("top5_o"),
                label = "", choices = c("TOP 5" = "5", "TOP 10" = "10"),
                selected = "5", inline = TRUE, status = "default"
              ),
              bsTooltip("top5_o", "Selecciona los 5 o 10 principales proveedores",
                "right",
                options = list(container = "body")
              ),
              uiOutput(ns("tabs_aquien_o"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.orga_qcc=='o.como'",
          fluidRow(
            column(
              width = 12, uiOutput(ns("tabs_como_o"))
            )
          )
        )
      ),
      ##### ---------PROVEEDORES-------######
      conditionalPanel(
        condition = "input.nivel == 'prov'",
        fluidRow(
          useShinyjs(),
          column(
            width = 4
          ),
          column(
            width = 4,
            prettyRadioButtons(ns("radio.p1"),
              label = "", width = "90%", status = "default",
              choices = list("Por nombre" = "empresa", "Por RUT" = "rut"), selected = "empresa", inline = TRUE
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              id = ns("combo.e.p1"),
              column(
                width = 4, 
                dqshiny::autocomplete_input(ns("nombre.p1"),
                  label = "",
                  options = sort(proveedores$denominacion_social),
                  value = "FUNDACION A GANAR",
                  placeholder = "Ingresa nombre de empresa", width = "100%"
                )
              ),
              column(
                width = 4, span(verbatimTextOutput(ns("nxr1.p1")),
                style = style_verbatim
              ))
            ),
            div(
              id = ns("combo.r.p1"),
              column(4, dqshiny::autocomplete_input(ns("rut.p1"),
                label = "",
                options = sort(proveedores$codigo_fiscal),
                value = "215748230018",
                placeholder = "Ingresa RUT de la empresa",
                max_options = 50,
                width = "100%"
              )),
              column(4, span(verbatimTextOutput(ns("nxr2.p1")),
                style = style_verbatim
              ))
            ),
            column(4, selectInput(ns("anio.p1"), "", multiple = T, choices = anios, selected = 2021))
          )
        ),
        fluidRow(
            column(
              width = 4,
              radioGroupButtons(
                inputId = "prov_qcc",
                label = "",
                choices = c("CUÁNTO" = "p.cuanto", "CUÁNDO" = "p.cuando", "A QUIÉN" = "p.aquien", "CÓMO" = "p.como"),
                justified = TRUE, size = "sm",
                checkIcon = list(inline = TRUE)
              )
            ),
          column(
            width = 4,
            radioGroupButtons(ns("mc.p1"),
              choices = c("MONTO" = "M", "CANTIDAD" = "C"),
              selected = "M", size = "sm", justified = TRUE,
              checkIcon = list(inline = TRUE)
            )
          ),
          column(
            width = 4,
            radioGroupButtons(ns("tiempo.p"),
              choices = c("ACUMULADO" = "p.acumulado", "ANUAL" = "p.anual"),
              selected = "p.anual", size = "sm", justified = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.prov_qcc == 'p.cuanto'",
          fluidRow(
            column(
              width = 12, 
              uiOutput(ns("tabs_cuanto_p"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.prov_qcc=='p.cuando'",
          fluidRow(
            column(
              width = 12,
              prettyRadioButtons(inputId = ns("plot_type_p"), status = "default", label = "Frecuencia", choices = c("DIARIO", "MENSUAL"), inline = TRUE),
              uiOutput(ns("tabs_cuando_p"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.prov_qcc == 'p.aquien'",
          fluidRow(
            column(
              width = 12,
              prettyRadioButtons(ns("top5_p"),
                label = "", choices = c("Top 5" = "5", "Top 10" = "10"),
                selected = "5", inline = TRUE, status = "default"
              ),
              bsTooltip("top5_p", "Selecciona los 5 o 10 principales compradores",
                "right",
                options = list(container = "body")
              ),
              uiOutput(ns("tabs_aquien_p"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.prov_qcc=='p.como'",
          fluidRow(
            column(
              width = 12,
              uiOutput(ns("tabs_como_p"))
            )
          )
        )
      ),
      ##### ------------ARTICULOS-----------########
      conditionalPanel(
        condition = "input.nivel == 'arts'",
        fluidRow(
          useShinyjs(),
          tags$style(HTML("
                  .box-header {
                    padding: 0 10px 0 0;
                  }
                  .box-header h3 {
                    width: 100%;
                    padding: 10px;
                  }")),
          tags$style(
            "input[type='radio']:checked+span{
             color: black;
             }
    input[type='radio']+span{
        color: #757d75;
      }",
            "input[type='checkbox']:checked+span{
             color: black;
             }
    input[type='checkbox']+span{
        color: #757d75;
      }"
          ),
          column(4),
          column(
            width = 4,
            prettyRadioButtons(ns("radio.a1"),
              label = "Seleccionar:",
              choices = list("Por nombre" = "articulo", "Por familia" = "familia"), selected = "articulo", inline = TRUE, status = "default"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            dqshiny::autocomplete_input(ns("nombre.a1"), "", sort(articulos$articulo),
              placeholder = "Ingresa nombre del artículo", value = "CAFE", max_options = 50
            ), #
            div(
              id = ns("combo.a1"),
              column(4, selectInput(ns("familia.a1"), "Familia", multiple = F, choices = sort(familia_codiguera$familia), selected = "MATERIALES Y SUMINISTROS")),
              column(4, selectInput(ns("subfam.a1"), "SubFamilia", multiple = F, choices = NULL, selected = "ALIMENTOS Y PRODUCTOS AGROPECUARIOS, FORESTALES Y MARITIMOS")),
              column(4, selectInput(ns("articulo.a1"), "Articulo", multiple = F, choices = NULL, selected = "CAFE"))
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            radioGroupButtons(
              inputId = "art_qcc",
              label = "",
              choices = c("CUÁNTO" = "a.cuanto", "CUÁNDO" = "a.cuando", "A QUIÉN" = "a.aquien", "CÓMO" = "a.como"),
              size = "sm",
              checkIcon = list(inline = TRUE)
            )
          ),
          column(width = 4, selectInput(ns("anio.a1"), "", multiple = T, choices = anios, selected = 2021, width = "300px")),
          column(
            width = 4,
            radioGroupButtons(ns("mc.a1"),
              choices = c("MONTO" = "M", "CANTIDAD" = "C"),
              selected = "M", size = "sm", justified = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = "input.art_qcc=='a.cuanto'",
          fluidRow(
            column(
              width = 12,
              uiOutput(ns("tabs_cuanto_a"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.art_qcc=='a.cuando'",
          fluidRow(
            column(
              width = 12,
              fluidRow(prettyRadioButtons(inputId = ns("plot_type_a"), label = "FRECUENCIA", choices = c("DIARIO", "MENSUAL"), inline = TRUE, status = "default")),
              uiOutput(ns("tabs_cuando_a"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.art_qcc=='a.aquien'",
          fluidRow(
            column(
              width = 12,
              uiOutput(ns("tabs_aquien_a"))
            )
          )
        ),
        conditionalPanel(
          condition = "input.art_qcc=='a.como'",
          fluidRow(
            column(
              width = 12,
              uiOutput(ns("tabs_como_a")),
            )
          )
        )
      )
      #--------------THE END-----------#
    )
  )
}

### SERVER ---------------------------------------

explorar_server <- function(id) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # observer events
    toListen <- reactive({
      list(input$anio, input$orga_qcc)
    })

    observeEvent(toListen(), {
      if (length(input$anio) > 1) {
        shinyjs::show("tiempo")
      } else {
        shinyjs::hide("tiempo")
      }
    })

    observe({
      print(input$locInput)
    })


    observeEvent(input$radio.p1, {
      if (input$radio.p1 == "empresa") {
        shinyjs::show("combo.e.p1")
      } else {
        shinyjs::hide("combo.e.p1")
      }
    })
    #
    observeEvent(input$radio.p1, {
      if (input$radio.p1 == "rut") {
        shinyjs::show("combo.r.p1")
      } else {
        shinyjs::hide("combo.r.p1")
      }
    })

    observeEvent(input$anio.p1, {
      if (length(input$anio.p1) > 1) {
        shinyjs::show("tiempo.p")
      } else {
        shinyjs::hide("tiempo.p")
      }
    })

    observeEvent(input$calc, {
      if (input$calc == TRUE) {
        shinyjs::show("combo_calc")
      } else {
        shinyjs::hide("combo_calc")
      }
    })

    observeEvent(input$radio.a1, {
      if (input$radio.a1 == "articulo") {
        shinyjs::show("nombre.a1")
      } else {
        shinyjs::hide("nombre.a1")
      }
    })

    observeEvent(input$radio.a1, {
      if (input$radio.a1 == "familia") {
        shinyjs::show("combo.a1")
      } else {
        shinyjs::hide("combo.a1")
      }
    })

    observeEvent(input$familia.a1, {
      updateSelectInput(session, "subfam.a1",
        choices = unique(sub_familia_codiguera$sub_familia[sub_familia_codiguera$familia == input$familia.a1])
      )
    })
    observeEvent(input$subfam.a1, {
      updateSelectInput(session, "articulo.a1",
        choices = unique(articulos_detalle$articulo[articulos_detalle$sub_familia == input$subfam.a1])
      )
    })

    ### -------- ORGANISMOS -------------####

    #### ** CUANTO ** ####

    data_oo <- reactive({
      n <- anios_codiguera %>%
        filter(anio %in% input$anio.o1) %>%
        select(cod_anio) %>%
        pull()
      compras_periodo <- rbindlist(compras_all[n]) %>%
        filter(organismo_codigo == as.character(organismos[organismo_sigla == input$orga, "organismo_codigo"])) %>%
        left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
        left_join(., proveedores, by = "codigo_fiscal") %>%
        mutate(organismo_sigla = input$orga) %>%
        mutate(anio = year(fecha))
    }) # %>% bindCache(input$orga, input$anio.o1)

    data_o <- reactive({
      if (input$obras == T) {
        data_oo() %>%
          filter(cod_familia != 6)
      } else {
        data_oo()
      }
    })

    data_cum_o <- reactive({
      data_o() %>%
        # filter(!duplicated(ocid)) %>%
        group_by(familia, sub_familia, anio) %>%
        summarise(
          Total = round(sum(monto_item_total), 0),
          Cantidad = n()
        ) %>%
        ungroup()
    }) # %>% bindCache(input$orga, input$anio.o1)

    output$tabs_cuanto_o <- renderUI({
      if (input$tiempo == "o.anual") {
        nTabs <- length(input$anio.o1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.o1[i],
            plotly::plotlyOutput(ns(paste0("p", input$anio.o1[i])),
              height = "100%", width = "100%"
            ),
            br(),
            DT::DTOutput(ns(input$anio.o1[i])),
            bsTooltip(id = ns(input$anio.o1[i]), title = pesos_miles)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo == "o.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          shinycssloaders::withSpinner(plotlyOutput(ns("plot_cum_cuanto_o")), color = "white"),
          br(),
          DT::DTOutput(ns("tab_cum_cuanto_o")),
          bsTooltip(id = ns("tab_cum_cuanto_o"), title = pesos_miles)
        )
        tabsetPanel(myTabs)
      }
    })

    #
    observe(
      lapply(seq_len(length(input$anio.o1)), function(i) {
        output[[input$anio.o1[i]]] <- DT::renderDT({
          data_o() %>%
            # filter(!duplicated(ocid)) %>%
            filter(anio == input$anio.o1[i]) %>%
            group_by(familia) %>%
            summarise(
              Total = round(sum(monto_item_total) / 1e3, 0),
              Cantidad = n()
            ) %>%
            mutate(
              "% Monto" = round_off(Total / sum(Total) * 100, digits = 1),
              "% Cantidad" = round_off(Cantidad / sum(Cantidad) * 100, digits = 1),
              Organismo = input$orga
            ) %>%
            select(Organismo, Familia = familia, Monto = Total, "% Monto", Cantidad, "% Cantidad") %>%
            arrange(-Monto) %>%
            set_DT(
              colnames = c("Organismo", "Familia", "Monto", "% Monto", "Cantidad", "% Cantidad"),
              responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE
            ) %>% # DT_caption_mil
            DT::formatStyle(columns = 1:6, fontFamily = "ubuntu") %>%
            formatCurrency(columns = c(3), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",") %>%
            formatCurrency(columns = c(4, 6), currency = "", digits = 1, mark = ".", dec.mark = ",")
        })
        output[[paste0("p", input$anio.o1[i])]] <- plotly::renderPlotly({
          if (input$mc.o1 == "M") {
            dd <- data_cum_o() %>%
              filter(anio == input$anio.o1[i]) %>%
              treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Total") # %>%

            validate(need(nrow(dd) > 0, ""))
            dd %>%
              plotly_treemap(
                yalign = 0.95, xalign = 0.25,
                text = paste0("Monto comprado según familia y subfamilia de productos", " (", input$anio.o1[i], ")")
              )
          } else if (input$mc.o1 == "C") {
            dd <- data_cum_o() %>%
              filter(anio == input$anio.o1[i]) %>%
              treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Cantidad") # %>%

            validate(need(nrow(dd) > 0, ""))
            dd %>%
              plotly_treemap(
                cantidad = TRUE, yalign = 0.95, xalign = 0.3,
                text =  paste0("Cantidad de compras según familia y subfamilia de productos", " (", input$anio.o1[i], ")")
              )
          }
        })
      })
    )

    output$tab_cum_cuanto_o <- renderDT({
      data_o() %>%
        # filter(!duplicated(ocid)) %>%
        group_by(familia) %>%
        summarise(
          Total = round(sum(monto_item_total) / 1e3, 0),
          Cantidad = n()
        ) %>%
        mutate(
          "% Monto" = round_off(Total / sum(Total) * 100, digits = 1),
          "% Cantidad" = round_off(Cantidad / sum(Cantidad) * 100, digits = 1),
          Organismo = input$orga
        ) %>%
        select(Organismo, Familia = familia, Monto = Total, "% Monto", Cantidad, "% Cantidad") %>%
        arrange(-Monto) %>%
        set_DT(
          colnames = c("Organismo", "Familia", "Monto", "% Monto", "Cantidad", "% Cantidad") ,
          responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE
        ) %>% # DT_caption_mil
         formatCurrency(
          columns = c(3), currency = "$", digits = 1,
          interval = 3, mark = ".", dec.mark = ","
        ) %>%
        formatCurrency(
          columns = c(4, 6), currency = "", digits = 1,
          mark = ".", dec.mark = ","
        )
    })

    output$plot_cum_cuanto_o <- renderPlotly({
      if (input$mc.o1 == "M") {
        dd <- data_cum_o() %>%
          treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Total") # %>%

        validate(need(nrow(dd) > 0, "No hay datos para generar el gráfico"))
        dd %>%
          plotly_treemap(
            yalign = 0.95, xalign = 0.3,
            text = paste0("Monto comprado según familia y subfamilia de productos", " (", input$anio.o1[i], ")")
          )
      } else if (input$mc.o1 == "C") {
        dd <- data_cum_o() %>%
          treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Cantidad") # %>%

        validate(need(nrow(dd) > 0, ""))
        dd %>%
          plotly_treemap(
            cantidad = TRUE, yalign = 0.95, xalign = 0.3,
            text = paste0("Cantidad de compras según familia y subfamilia de productos")
          )
      }
    })

    ############### ** CUANDO ** ######################################
    d_tiempo <- reactive({
      if (input$obras == F) {
        cr <- compras_resumen
      } else {
        cr <- compras_resumen_so
      }
      cr %>%
        filter(organismo_codigo == as.character(organismos[organismo_sigla == input$orga, "organismo_codigo"])) %>%
        filter(anio %in% input$anio.o1) %>%
        mutate(mes = lubridate::month(fecha, label = TRUE, abbr = FALSE)) %>%
        group_by(fecha) %>%
        mutate(cantidad = n()) %>%
        ungroup()
    }) # %>% bindCache(input$orga, input$anio.o1)

    #########
    output$tabs_cuando_o <- renderUI({
      if (input$tiempo == "o.anual") {
        nTabs <- length(input$anio.o1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.o1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.o1[i], "_cuando_o"))), 
            br(),
            DT::DTOutput(ns(paste0("t_", input$anio.o1[i], "_cuando_o"))),
            bsTooltip(id = ns(paste0("t_", input$anio.o1[i], "_cuando_o")), title = pesos_miles)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo == "o.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_cuando_o")),
          br(),
          DT::DTOutput(ns("t_cum_cuando_o")),
          bsTooltip(id = ns("t_cum_cuando_o"), title = pesos_miles)
        )
        tabsetPanel(myTabs)
      }
    })

    #
    observe(
      lapply(seq_len(length(input$anio.o1)), function(i) {
        output[[paste0("t_", input$anio.o1[i], "_cuando_o")]] <- DT::renderDT(
          {
            if (input$plot_type == "DIARIO") {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                arrange(fecha) %>%
                complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
                mutate(
                  monto_compra = replace_na(monto_compra, 0),
                  cantidad = replace_na(cantidad, 0)
                ) %>%
                group_by(fecha) %>%
                summarise(
                  cantidad = max(cantidad),
                  monto_total = round_off(sum(monto_compra), 1)
                ) %>%
                mutate(organismo = input$orga) %>%
                select(fecha, monto_total, cantidad, organismo) %>%
                set_DT(colnames = c("Fecha", "Monto", "Cantidad", "Organismo"),
                        searching = TRUE, responsive = TRUE) %>%
                DT::formatStyle(columns = 1:4, fontFamily = "ubuntu") %>%
                DT::formatCurrency(columns = c(2), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
            } else {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                group_by(mes, anio) %>%
                summarise(
                  monto_total = round_off(sum(monto_compra), 0),
                  cantidad = n()
                ) %>%
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                mutate(monto_total = round_off(monto_total / 1e6, 1)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                mutate(organismo = input$orga) %>%
                select(organismo, mes, anio, monto_total, cantidad) %>%
                set_DT(colnames = c("Organismo", "Mes", "Año", "Monto", "Cantidad"),
                       searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
                DT::formatCurrency(columns = c(4), currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
            }
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.o1[i], "_cuando_o")]] <- renderEcharts4r({
          aa <- d_tiempo()
          validate(need(nrow(aa) > 0, ""))

          if (input$plot_type == "DIARIO") {
            if (input$mc.o1 == "M") {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                arrange(fecha) %>%
                complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
                mutate(monto_compra = replace_na(monto_compra, 0)) %>%
                mutate(monto_compra = monto_compra / 1e3) %>%
                group_by(fecha) %>%
                summarise(
                  cantidad = max(cantidad),
                  monto_total = round_off(sum(monto_compra), 1)
                ) %>%
                mutate(fecha = as.character(fecha)) %>%
                echarts_lines(
                  varx = "fecha", vary = "monto_total",
                  text = "Compras diarias (en miles de pesos)"
                )
            } else if (input$mc.o1 == "C") {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                group_by(fecha) %>%
                mutate(cantidad = n()) %>%
                ungroup() %>%
                arrange(fecha) %>%
                complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
                mutate(
                  monto_compra = replace_na(monto_compra, 0),
                  cantidad = replace_na(cantidad, 0)
                ) %>%
                mutate(fecha = as.character(fecha)) %>%
                echarts_lines(
                  varx = "fecha", vary = "cantidad",
                  text = "Cantidad de compras diarias"
                )
            }
          } else if (input$plot_type == "MENSUAL") {
            if (input$mc.o1 == "M") {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                group_by(mes) %>% #
                summarise(monto_total = sum(monto_compra)) %>%
                # summarise(n = n())
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                mutate(monto_total = round_off(monto_total / 1e6, 1)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "monto_total", vary = "mes", color = "#B6D0E2",
                  text = "Compras mensuales (en millones de pesos)", l = "10%"
                )
            } else {
              d_tiempo() %>%
                filter(anio == input$anio.o1[i]) %>%
                group_by(mes) %>% #
                summarise(cantidad = n()) %>%
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "cantidad", vary = "mes", color = "#B6D0E2",
                  text = "Cantidad de compras mensuales", l = "10%"
                )
            }
          }
        })
        output$p_cum_cuando_o <- renderEcharts4r({
          # if(input$mc.o1 =="M"){
          #
          #   d_tiempo() %>%
          #     mutate(fecha = as.Date(as.character(fecha), "%Y-%m-%d"),
          #            monto_compra = monto_compra/1e3) %>%
          #     group_by(anio) %>%
          #     e_charts(fecha) %>%
          #     e_calendar(range = input$anio[i],#top="40",
          #                dayLabel = list(
          #                  firstDay=1,
          #                  nameMap = c('Do', 'Lu', 'Ma', 'Mi', 'Ju', 'Vi', 'Sá')),
          #                monthLabel = list(
          #                  nameMap = lubridate::month(1:12, label = TRUE, abbr = FALSE))
          #     ) %>%
          #     e_heatmap(monto_compra, coord_system = "calendar") %>%
          #     e_visual_map(max = 30, inRange = list(color = c('#78A2CC', '#88AED0', '#96B9D0', '#A4C3D2', '#AECBD6', '#BFD4DB'))) %>%
          #     e_title("Calendario de compras")%>%
          #     e_tooltip("item")
          # }
          # else if(input$mc.o1 == "C"){
          #
          # }
        })
      })
    )

    output$t_cum_cuando_o <- renderDT({

    })



    # ###** A QUIEN **###############
    output$tabs_aquien_o <- renderUI({
      if (input$tiempo == "o.anual") {
        nTabs <- length(input$anio.o1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.o1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.o1[i], "_aquien_o"))),
            br(), br(), br(), br(), br(),
            DT::DTOutput(ns(paste0("t_", input$anio.o1[i], "_aquien_o"))),
            bsTooltip(id = ns(paste0("t_", input$anio.o1[i], "_aquien_o")), title = pesos_miles)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo == "o.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_aquien_o")),
          br(), br(), br(), br(), br(),
          DT::DTOutput(ns("t_cum_aquien_o")),
          bsTooltip(id = ns("t_cum_aquien_o"), title = pesos_miles)
        )
        tabsetPanel(myTabs)
      }
    })

    observe(
      lapply(seq_len(length(input$anio.o1)), function(i) {
        output[[paste0("t_", input$anio.o1[i], "_aquien_o")]] <- DT::renderDT(
          {
            data_o() %>%
              filter(anio == input$anio.o1[i]) %>%
              group_by(organismo_sigla, denominacion_social) %>%
              summarise(
                monto_total = round_off(sum(monto_item_total) / 1e3, 1),
                q_compras = n(), .groups = "drop"
              ) %>% # compra
              ungroup() %>%
              arrange(-monto_total) %>%
              select(denominacion_social, monto_total, q_compras) %>%
              set_DT(colnames = c("Denominación social", "Total", "Cantidad"),
                      searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # , caption = DT_caption_mil
              formatCurrency("monto_total", currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.o1[i], "_aquien_o")]] <- renderEcharts4r({
          aa <- data_o() %>% filter(anio == input$anio.o1[i])
          validate(need(nrow(aa) > 0, "No hay datos para realizar el gráfico"))

          data_o() %>%
            filter(anio == input$anio.o1[i]) %>%
            sankey_data(organismo = input$orga, top = input$top5_o, unidad = input$mc.o1) %>%
            echarts_sankey(
              source = "organismo_sigla", target = "denominacion_social", value = input$mc.o1,
              text = paste("Principales proveedores de", input$orga), t = "8%"
            ) %>% # ¿A quiénes compra?
            e_dims(height = "500", width = "1000")
        })
      })
    )

    output$t_cum_aquien_o <- renderDT(
      {
        data_o() %>%
          group_by(organismo_sigla, denominacion_social) %>%
          summarise(
            monto_total = round_off(sum(monto_item_total) / 1e3, 1),
            q_compras = n(), .groups = "drop"
          ) %>% # compra
          ungroup() %>%
          arrange(-monto_total) %>%
          select(denominacion_social, monto_total, q_compras) %>%
          set_DT(colnames = c("Denominación social", "Total", "Cantidad"),
                  searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # , caption = DT_caption_mil
          #DT::formatStyle(columns = c("denominacion_social", "monto_total", "q_compras")) %>%
          formatCurrency("monto_total", currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
      },
      server = FALSE
    )

    output$p_cum_aquien_o <- renderEcharts4r({
      data_o() %>%
        sankey_data(organismo = input$orga, top = input$top5_o, unidad = input$mc.o1) %>%
        echarts_sankey(
          source = "organismo_sigla", target = "empresa", value = input$mc.o1,
          text = paste("Principales proveedores de", input$orga), t = "8%"
        ) %>%
        e_dims(height = "500", width = "1000")
    })

    ######** COMO **#######
    org_como <- reactive({
      compras_resumen %>%
        filter(organismo_codigo == as.character(organismos[organismo_sigla == input$orga, "organismo_codigo"]) & anio %in% input$anio.o1) %>%
        left_join(., tipo_compra_codiguera, by = "tipo_compra_codigo") %>%
        group_by(tipo_compra, anio) %>%
        summarise(
          Monto = sum(monto_compra),
          Cantidad = n()
        ) %>%
        mutate(Monto = round_off(Monto / 1e6, 1)) %>%
        ungroup() %>%
        arrange(Monto)
    }) # %>% bindCache(input$orga, input$anio.o1)

    ###
    output$tabs_como_o <- renderUI({
      if (input$tiempo == "o.anual") {
        nTabs <- length(input$anio.o1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.o1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.o1[i], "_como_o"))),
            br(),
            DT::DTOutput(ns(paste0("t_", input$anio.o1[i], "_como_o"))),
            bsTooltip(id = ns(paste0("t_", input$anio.o1[i], "_como_o")), title = pesos_millones)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo == "o.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_como_o")),
          br(),
          DT::DTOutput(ns("t_cum_como_o")),
          bsTooltip(id = ns("t_cum_como_o"), title = pesos_millones)
        )
        tabsetPanel(myTabs)
      }
    })

    observe(
      lapply(seq_len(length(input$anio.o1)), function(i) {
        output[[paste0("t_", input$anio.o1[i], "_como_o")]] <- DT::renderDT(
          {
            org_como() %>%
              filter(anio == input$anio.o1[i]) %>%
              arrange(-Monto) %>%
              select(tipo_compra, anio, Monto, Cantidad) %>%
              set_DT(colnames = c("Tipo de compra", "Año", "Monto", "Cantidad"),
                     responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE) %>% # , caption = DT_caption_mil
              formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.o1[i], "_como_o")]] <- renderEcharts4r({
          aa <- org_como() %>%
            filter(anio == input$anio.o1[i])
          validate(need(nrow(aa) > 0, ""))

          if (input$mc.o1 == "C") {
            t <- "Tipo de compra seǵun cantidad"
            x <- "Cantidad"
          } else {
            t <- "Tipo de compra según monto (en millones de $)"
            x <- "Monto"
          }
          org_como() %>%
            filter(anio == input$anio.o1[i]) %>%
            echarts_bar(
              vary = "tipo_compra", varx = x, l = "15%",
              text = t, subtext = "", color = "#B6D0E2"
            )
        })
      })
    )

    output$t_cum_como_o <- renderDT({
      org_como() %>%
        group_by(tipo_compra) %>%
        summarise(
          total = sum(Monto, na.rm = T),
          Cantidad = sum(Cantidad, na.rm = T)
        ) %>%
        arrange(-total) %>%
        select(tipo_compra, total, Cantidad) %>%
        set_DT(colnames = c("Tipo de compra", "Monto", "Cantidad"),
               responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE
               ) %>% # , caption = DT_caption_mil
        formatCurrency(columns = 2, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$p_cum_como_o <- renderEcharts4r({
      if (input$mc.o1 == "C") {
        t <- "Tipo de compra seǵun cantidad"
        x <- "Cantidad"
      } else {
        t <- "Tipo de compra según monto (en millones de $)"
        x <- "Monto"
      }
      org_como() %>%
        echarts_bar(
          vary = "tipo_compra", varx = x, l = "15%",
          text = t, subtext = "", color = "#B6D0E2"
        )
    })
    ###

    ##### ---------------- PROVEEDORES ------------------################
    data_p <- reactive({
      n <- anios_codiguera %>%
        filter(anio %in% input$anio.p1) %>%
        select(cod_anio) %>%
        pull()
      compras_periodo2 <- rbindlist(compras_all[n])
    })

    d_prov <- reactive({
      if (input$radio.p1 == "empresa") {
        data_p() %>%
          filter(codigo_fiscal == as.character(proveedores[denominacion_social == input$nombre.p1, "codigo_fiscal"])) %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          mutate(
            anio = year(fecha),
            mes = lubridate::month(fecha, abbr = FALSE, label = TRUE)
          )
      } else if (input$radio.p1 == "rut") {
        data_p() %>%
          filter(codigo_fiscal == input$rut.p1) %>%
          left_join(., proveedores %>% filter(codigo_fiscal == input$rut.p1), by = "codigo_fiscal") %>%
          left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          mutate(
            anio = year(fecha),
            mes = lubridate::month(fecha, abbr = FALSE, label = TRUE)
          )
      } else {}
    })

    nombre <- reactive(d_prov()$denominacion_social[1])

    output$nxr1.p1 <- renderText({
      paste("RUT:", as.character(proveedores[denominacion_social == input$nombre.p1, "codigo_fiscal"]))
    })

    output$nxr2.p1 <- renderText({
      paste("Empresa:", as.character(proveedores[codigo_fiscal == input$rut.p1, "denominacion_social"]))
    })

    ### ** CUANTO #####
    output$tabs_cuanto_p <- renderUI({
      if (input$tiempo.p == "p.anual") {
        nTabs <- length(input$anio.p1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.p1[i],
            plotly::plotlyOutput(ns(paste0("p_", input$anio.p1[i], "_cuanto_p")),
              height = "100%", width = "100%"
            ),
            br(),
            DT::DTOutput(ns(paste0("t_", input$anio.p1[i], "_cuanto_p"))),
            bsTooltip(id = ns(paste0("t_", input$anio.p1[i], "_cuanto_p")), title = "")
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo.p == "p.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          plotlyOutput(ns("p_cum_cuanto_p")),
          br(),
          DT::DTOutput(ns("t_cum_cuanto_p")),
          bsTooltip(id = ns("t_cum_cuanto_p"), title = "")
        )
        tabsetPanel(myTabs)
      }
    })

    observe(
      lapply(seq_len(length(input$anio.p1)), function(i) {
        output[[paste0("t_", input$anio.p1[i], "_cuanto_p")]] <- DT::renderDT(
          {
            d_prov() %>%
              filter(anio == input$anio.p1[i]) %>%
              group_by(organismo_sigla, familia, sub_familia) %>%
              # summarise(Total = round_off(sum(monto_item)/1e3,1)) %>%
              summarise(Total = sum(monto_item_total)) %>%
              # mutate(monto_total = round_off(monto_total / 1e6, 1), trim = TRUE) %>%
              ungroup() %>%
              mutate(Empresa = nombre()) %>%
              select(Empresa, familia, sub_familia, Total) %>%
              filter(!familia == "") %>%
              arrange(-Total) %>%
              set_DT(colnames = c("Empresa", "Familia", "SubFamilia", "Monto"),
                     searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # , caption = DT_caption
              formatCurrency(columns = 4, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.p1[i], "_cuanto_p")]] <- plotly::renderPlotly({
          if (input$mc.p1 == "M") {
            aa <- d_prov() %>%
              filter(anio == input$anio.p1[i]) # %>%
            validate(need(nrow(aa) > 0, ""))
            d_prov() %>%
              filter(anio == input$anio.p1[i]) %>%
              group_by(organismo_sigla, familia, sub_familia) %>%
              # summarise(Total = round_off(sum(monto_item)/1e3,1)) %>%
              summarise(Total = sum(monto_item_total)) %>%
              # mutate(monto_total = round_off(monto_total / 1e6, 1), trim = TRUE) %>%
              ungroup() %>%
              treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Total") %>%
              plotly_treemap(
                yalign = 0.95, xalign = 0.211,
                text = paste0("Monto total vendido según familia de productos", " (", input$anio.p1[i], ")")
              )
          } else if (input$mc.p1 == "C") {
            aa <- d_prov() %>%
              filter(anio == input$anio.p1[i]) # %>%
            validate(need(nrow(aa) > 0, ""))
            d_prov() %>%
              filter(anio == input$anio.p1[i]) %>%
              group_by(organismo_sigla, familia, sub_familia) %>%
              summarise(Cantidad = n()) %>%
              ungroup() %>%
              treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Cantidad") %>%
              plotly_treemap(
                cantidad = TRUE, yalign = 0.95, xalign = 0.3,
                text = paste0("<b>", "Total de ventas según familia de productos", " (", input$anio.p1[i], ")</b>")
              )
          }
        })
      })
    )

    output$t_cum_cuanto_p <- renderDT({
      d_prov() %>%
        group_by(organismo_sigla, familia, sub_familia) %>%
        summarise(Total = sum(monto_item_total)) %>%
        ungroup() %>%
        mutate(Empresa = nombre()) %>%
        select(Empresa, familia, sub_familia, Total) %>%
        filter(!familia == "") %>%
        arrange(-Total) %>%
        set_DT(colnames = c("Empresa", "Familia", "SubFamilia", "Total"),
               searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # , caption = DT_caption
        formatCurrency(columns = 4, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$p_cum_cuanto_p <- renderPlotly({
      if (input$mc.p1 == "M") {
        aa <- d_prov() %>%
          group_by(organismo_sigla, familia, sub_familia) # %>%
        validate(need(nrow(aa) > 0, ""))

        d_prov() %>%
          summarise(Total = sum(monto_item_total)) %>%
          ungroup() %>%
          treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Total") %>%
          plotly_treemap(
            yalign = 0.95, xalign = 0.3,
            text =  "Monto total vendido según familia de productos"
          )
      } else if (input$mc.p1 == "C") {
        aa <- d_prov() # %>%
        validate(need(nrow(aa) > 0, ""))
        d_prov() %>%
          group_by(organismo_sigla, familia, sub_familia) %>%
          summarise(Cantidad = n()) %>%
          ungroup() %>%
          treemap_data(var1 = "familia", var2 = "sub_familia", counts = "Cantidad") %>%
          plotly_treemap(
            cantidad = TRUE, yalign = 0.95, xalign = 0.25,
            text =  "Total de ventas según familia de productos"
          )
      }
    })
    ###

    ### ** CUANDO ----------####
    output$tabs_cuando_p <- renderUI({
      if (input$tiempo.p == "p.anual") {
        nTabs <- length(input$anio.p1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.p1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.p1[i], "_cuando_p"))),
            br(),
            DT::DTOutput(ns(paste0("t_", input$anio.p1[i], "_cuando_p"))),
            bsTooltip(id = ns(paste0("t_", input$anio.p1[i], "_cuando_p")), title = pesos_miles)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo.p == "p.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_cuando_p")),
          br(),
          DT::DTOutput(ns("t_cum_cuando_p")),
          bsTooltip(id = ns("t_cum_cuando_p"), title = pesos_miles)
        )
        tabsetPanel(myTabs)
      }
    })

    observe(
      lapply(seq_len(length(input$anio.p1)), function(i) {
        output[[paste0("t_", input$anio.p1[i], "_cuando_p")]] <- DT::renderDT(
          {
            if (input$plot_type_p == "DIARIO") {
              d_prov() %>%
                filter(anio == input$anio.p1[i]) %>%
                group_by(fecha) %>%
                mutate(cantidad = n()) %>%
                ungroup() %>%
                arrange(fecha) %>%
                complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
                mutate(monto_item_total = replace_na(monto_item_total, 0)) %>%
                mutate(cantidad = replace_na(cantidad, 0)) %>%
                group_by(fecha) %>%
                summarise(
                  cantidad = max(cantidad),
                  monto_total = round_off(sum(monto_item_total), 1)
                ) %>%
                mutate(Empresa = nombre()) %>%
                select(Empresa, fecha, monto_total, cantidad) %>%
                set_DT(colnames = c("Empresa", "Fecha", "Monto", "Cantidad"),
                       searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
                DT::formatStyle(columns = 1:4, fontFamily = "ubuntu")
            } else {
              d_prov() %>%
                filter(anio == input$anio.p1[i]) %>%
                # mutate(mes = month(fecha)) %>%
                group_by(anio, mes) %>%
                summarise(
                  monto_total = round_off(sum(monto_item_total), 0),
                  cantidad = n()
                ) %>%
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                mutate(monto_total = round_off(monto_total / 1e3, 0)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                mutate(Empresa = nombre()) %>%
                select(Empresa, anio, mes, monto_total, cantidad) %>%
                set_DT(colnames = c("Empresa", "Año", "Mes", "Monto", "Cantidad"),
                       searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
                DT::formatStyle(columns = 1:5, fontFamily = "ubuntu")
            }
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.p1[i], "_cuando_p")]] <- renderEcharts4r({
          aa <- d_prov() %>%
            filter(anio == input$anio.p1[i]) # %>%
          validate(need(nrow(aa) > 0, ""))

          if (input$plot_type_p == "DIARIO") {
            d_prov() %>%
              filter(anio == input$anio.p1[i]) %>%
              data_lines() %>%
              mutate(fecha = as.character(fecha)) %>%
              echarts_lines(
                varx = "fecha", value = input$mc.p1,
                text = "Ventas diarias (en miles de pesos)"
              )
          } else if (input$plot_type_p == "MENSUAL") {
            if (input$mc.p1 == "M") {
              d_prov() %>%
                filter(anio == input$anio.p1[i]) %>%
                group_by(mes) %>% #
                summarise(monto_total = sum(monto_item_total)) %>%
                # summarise(n = n())
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                mutate(monto_total = round_off(monto_total / 1e3, 1)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "monto_total", vary = "mes", color = "#B6D0E2",
                  text = "Ventas mensuales (en millones de pesos)", l = "10%"
                )
            } else if (input$mc.p1 == "C") {
              d_prov() %>%
                filter(anio == input$anio.p1[i]) %>%
                group_by(mes) %>% #
                summarise(cantidad = n()) %>%
                mungroup() %>%
                filter(!duplicated(mes)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "cantidad", vary = "mes", color = "#B6D0E2",
                  text = "Cantidad de ventas mensuales", l = "10%"
                )
            } else {}
          }
        })
      })
    )

    output$t_cum_cuando_p <- renderDT({

    })

    output$p_cum_cuando_p <- renderPlotly({

    })

    ####** COMO #####
    prov_como <- reactive({
      d_prov() %>%
        left_join(., tipo_compra_codiguera, by = "tipo_compra_codigo") %>%
        group_by(tipo_compra, anio) %>%
        summarise(
          monto_total = sum(monto_item_total),
          cantidad = n()
        ) %>%
        mutate(monto_total = round_off(monto_total / 1e3, 1)) %>%
        ungroup() %>%
        arrange(monto_total)
    })

    output$tabs_como_p <- renderUI({
      if (input$tiempo.p == "p.anual") {
        nTabs <- length(input$anio.p1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.p1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.p1[i], "_como_p"))),
            br(),
            DT::DTOutput(ns(paste0("t_", input$anio.p1[i], "_como_p"))),
            bsTooltip(id = ns(paste0("t_", input$anio.p1[i], "_como_p")), title = pesos_millones)
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo.p == "p.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_como_p")),
          br(),
          DT::DTOutput(ns("t_cum_como_p")),
          bsTooltip(id = ns("t_cum_como_p"), title = pesos_millones)
        )
        tabsetPanel(myTabs)
      }
    })
    ##

    observe(
      lapply(seq_len(length(input$anio.p1)), function(i) {
        output[[paste0("t_", input$anio.p1[i], "_como_p")]] <- DT::renderDT(
          {
            prov_como() %>%
              filter(anio == input$anio.p1[i]) %>%
              mutate(Empresa = nombre()) %>%
              arrange(-monto_total) %>%
              select(Empresa, tipo_compra, anio, monto_total, cantidad) %>%
              set_DT(colnames = c("Empresa", "Tipo de compra", "Año", "Monto", "Cantidad"),
                     responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE
                     ) %>% # , caption = DT_caption_mil
              formatCurrency(columns = 4, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.p1[i], "_como_p")]] <- renderEcharts4r({
          aa <- prov_como() %>%
            filter(anio == input$anio.p1[i])
          validate(need(nrow(aa) > 0, ""))

          if (input$mc.p1 == "C") {
            t <- "Tipo de compra seǵun cantidad"
            x <- "cantidad"
          } else {
            t <- "Tipo de compra según monto (en millones de $)"
            x <- "monto_total"
          }
          prov_como() %>%
            filter(anio == input$anio.p1[i]) %>%
            echarts_bar(
              vary = "tipo_compra", varx = x, l = "15%",
              text = t, subtext = "", color = "#B6D0E2"
            )
        })
      })
    )

    output$t_cum_como_o <- renderDT({
      prov_como() %>%
        group_by(tipo_compra) %>%
        summarise(total = sum(monto_total, na.rm = T)) %>%
        arrange(-total) %>%
        mutate(Empresa = nombre()) %>%
        select(Empresa, tipo_compra, total, cantidad) %>%
        set_DT(colnames = c("Empresa", "Tipo de compra", "Monto", "Cantidad"),
               responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE
               ) %>% # , caption = DT_caption_mil
        formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
    })

    output$p_cum_como_p <- renderEcharts4r({
      if (input$mc.p1 == "C") {
        t <- "Cantidad de compras por tipo"
        x <- "cantidad"
      } else {
        t <- "Compras por tipo (en millones de $)"
        x <- "monto_total"
      }
      prov_como() %>%
        echarts_bar(
          vary = "tipo_compra", varx = x, l = "15%",
          text = t, subtext = "", color = "#B6D0E2"
        )
    })

    ##### ** A QUIEN ** #####

    output$tabs_aquien_p <- renderUI({
      if (input$tiempo.p == "p.anual") {
        nTabs <- length(input$anio.p1)

        myTabs <- lapply(seq_len(nTabs), function(i) {
          tabPanel(
            input$anio.p1[i],
            echarts4rOutput(ns(paste0("p_", input$anio.p1[i], "_aquien_p"))),
            br(), br(), br(), br(), br(),
            DT::DTOutput(ns(paste0("t_", input$anio.p1[i], "_aquien_p"))),
            bsTooltip(id = ns(paste0("t_", input$anio.p1[i], "_aquien_p")), title = "")
          )
        })
        do.call(tabsetPanel, myTabs)
      } else if (input$tiempo.p == "p.acumulado") {
        myTabs <- tabPanel(
          "Acumulado",
          echarts4rOutput(ns("p_cum_aquien_p")),
          br(), br(), br(), br(), br(),
          DT::DTOutput(ns("t_cum_aquien_p")),
          bsTooltip(id = ns("t_cum_aquien_p"), title = "")
        )
        tabsetPanel(myTabs)
      }
    })

    observe(
      lapply(seq_len(length(input$anio.p1)), function(i) {
        output[[paste0("t_", input$anio.p1[i], "_aquien_p")]] <- DT::renderDT(
          {
            d_prov() %>%
              filter(anio == input$anio.p1[i]) %>%
              group_by(organismo_sigla) %>%
              summarise(
                Total = sum(monto_item_total),
                q = n()
              ) %>%
              mutate(Empresa = nombre()) %>%
              arrange(-Total) %>%
              select(Empresa, organismo_sigla, Total, q) %>%
              set_DT(colnames = c("Empresa", "Organismo", "Monto", "Cantidad"),
                     searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # ,caption = DT_caption
              DT::formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio[i], "_aquien_p")]] <- renderEcharts4r({
          aa <- d_prov() %>% filter(anio == input$anio.p1[i])
          validate(need(nrow(aa) > 0, "No hay datos para realizar el gráfico"))

          if (input$mc.p1 == "C") {
            tit <- "Principales compradores según cantidad"
          } else {
            tit <- "Principales compradores según monto"
          }

          d_prov() %>%
            filter(anio == input$anio.p1[i]) %>%
            sankey_data_prov(top = input$top5_p, unidad = input$mc.p1) %>%
            echarts_sankey("denominacion_social", "organismo_sigla",
              value = input$mc.p1,
              text = tit, t = "8%"
            ) %>%
            e_dims(height = "500", width = "1000")
        })
      })
    )

    output$t_cum_aquien_p <- renderDT(
      {
        d_prov() %>%
          group_by(organismo_sigla) %>%
          summarise(
            Total = sum(monto_item_total),
            q = n()
          ) %>%
          mutate(Empresa = nombre()) %>%
          arrange(-Total) %>%
          select(Empresa, organismo_sigla, Total, q) %>%
          set_DT(colnames = c("Empresa", "Organismo", "Monto", "Cantidad"),
                 searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>% # ,caption = DT_caption
          DT::formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
      },
      server = FALSE
    )


    output$p_cum_aquien_p <- renderEcharts4r({
      d_prov() %>%
        sankey_data_prov(top = input$top5_p, unidad = input$mc.p1) %>%
        echarts_sankey("denominacion_social", "organismo_sigla",
          value = input$mc.p1,
          text = "Principales compradores", t = "8%"
        ) %>%
        e_dims(height = "500", width = "1000")
    })

    #### --------------ARTICULOS--------------------##############
    compras_periodo3 <- reactive({
      n <- anios_codiguera %>%
        filter(anio %in% input$anio.a1) %>%
        select(cod_anio) %>%
        pull()
      compras_periodo3 <- rbindlist(compras_all[n])
    })

    d_art <- reactive({
      if (input$radio.a1 == "familia") {
        compras_periodo3() %>%
          filter(cod_familia != 3) %>%
          filter(iditem == as.character(articulos[articulo == input$articulo.a1, "iditem"])) %>%
          left_join(., articulos %>% select(articulo, iditem), by = "iditem") %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          mutate(
            anio = year(fecha),
            mes = lubridate::month(fecha, abbr = F, label = T)
          )
      } else if (input$radio.a1 == "articulo") {
        compras_periodo3() %>%
          filter(cod_familia != 3) %>%
          filter(iditem == as.character(articulos[articulo == input$nombre.a1, "iditem"])) %>%
          left_join(., articulos %>% select(articulo, iditem), by = "iditem") %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          mutate(
            anio = year(fecha),
            mes = lubridate::month(fecha, abbr = F, label = T)
          )
      } else {}
    })

    artic <- reactive(d_art()$articulo[1])
    # miny <- reactive(min(d_art()$anio))
    # maxy <- reactive(max(d_art()$anio))
    #
    # d_artp <- reactive({
    #   d_art() %>% filter(!is.na(monto_estandar)) %>%
    #     violin_data(year_from = miny(),
    #                 year_to = maxy(), item = artic())
    # })
    #
    ######
    output$tabs_cuanto_a <- renderUI({
      nTabs <- length(input$anio.a1)

      myTabs <- lapply(seq_len(nTabs), function(i) {
        tabPanel(
          input$anio.a1[i],
          plotly::plotlyOutput(ns(paste0("p_", input$anio.a1[i], "_cuanto_a")),
            height = "100%", width = "100%"
          ),
          br(),
          DT::DTOutput(ns(paste0("t_", input$anio.a1[i], "_cuanto_a"))),
          bsTooltip(id = ns(paste0("t_", input$anio.a1[i], "_cuanto_a")), title = pesos_miles)
        )
      })
      do.call(tabsetPanel, myTabs)
      # } else if(input$tiempo.a == 'a.acumulado'){
      #   myTabs = tabPanel("Acumulado",
      #                     plotlyOutput(ns("p_cum_cuanto_a")),
      #                     br(),
      #                     DT::DTOutput(ns("t_cum_cuanto_a")),
      #                     bsTooltip(id= ns('t_cum_cuanto_a'), title = pesos_miles)
      #   )
      #   tabsetPanel(myTabs)
      # }
    })

    observe(
      lapply(seq_len(length(input$anio.a1)), function(i) {
        output[[paste0("t_", input$anio.a1[i], "_cuanto_a")]] <- DT::renderDT(
          {
            d_art() %>%
              filter(anio == input$anio.a1[i]) %>%
              group_by(organismo_sigla, articulo) %>%
              summarise(monto_total = round_off(sum(monto_item_total) / 1e3, 1)) %>%
              ungroup() %>%
              arrange(-monto_total) %>%
              set_DT(colnames = c("Organismo", "Artículo", "Monto"),
                     searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
              formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.a1[i], "_cuanto_a")]] <- plotly::renderPlotly({
          top5_prod <- d_art() %>%
            filter(anio == input$anio.a1[i]) %>%
            filter(!is.na(monto_estandar)) %>%
            group_by(organismo_sigla) %>%
            summarise(monto_total = round_off(sum(monto_item_total) / 1e3, 1)) %>%
            ungroup() %>%
            slice_max(monto_total, n = 5) %>%
            select(organismo_sigla) %>%
            pull()

          d_art2 <- d_art() %>%
            filter(anio == input$anio.a1[i]) %>%
            filter(!is.na(monto_estandar)) %>%
            filter(organismo_sigla %in% top5_prod) %>%
            mutate(monto_estandar = round_off(monto_estandar, 1)) %>%
            select(organismo_sigla, monto_estandar)

          Q <- quantile(d_art2$monto_estandar, probs = c(.05, .95), na.rm = FALSE)

          validate(need(nrow(d_art2) > 0, ""))
          d_art2 %>%
            filter(monto_estandar > Q[1] & monto_estandar < Q[2]) %>%
            plot_ly(
              x = ~organismo_sigla, y = ~monto_estandar, split = ~organismo_sigla, type = "violin",
              box = list(visible = T), meanline = list(visible = F), spanmode = "hard", # hoverinfo="none"
              text = ~monto_estandar, hoverinfo = "y"
            ) %>%
            set_layout(
              title = list(
                text = "<b>Distribución del precio pagado según organismo</b>",
                y = 0.95, x = 0.19, xanchor = "center", yanchor = "bottom"
              ),
              show_lines = FALSE
            ) %>%
            set_config()
        })
      })
    )

    ####*** CUANDO **####
    output$tabs_cuando_a <- renderUI({
      nTabs <- length(input$anio.a1)

      myTabs <- lapply(seq_len(nTabs), function(i) {
        tabPanel(
          input$anio.a1[i],
          echarts4rOutput(ns(paste0("p_", input$anio.a1[i], "_cuando_a"))),
          br(),
          DT::DTOutput(ns(paste0("t_", input$anio.a1[i], "_cuando_a"))),
          bsTooltip(id = ns(paste0("t_", input$anio.a1[i], "_cuando_a")), title = pesos_miles)
        )
      })
      do.call(tabsetPanel, myTabs)
    })

    observe(
      lapply(seq_len(length(input$anio.a1)), function(i) {
        output[[paste0("t_", input$anio.a1[i], "_cuando_a")]] <- DT::renderDT(
          {
            if (input$plot_type_a == "DIARIO") {
              d_art() %>%
                filter(anio == input$anio.a1[i]) %>%
                group_by(fecha) %>%
                mutate(cantidad = n()) %>%
                ungroup() %>%
                arrange(fecha) %>%
                complete(fecha = seq(min(fecha), max(fecha), 1)) %>%
                mutate(monto_item_total = replace_na(monto_item_total, 0)) %>%
                mutate(cantidad = replace_na(cantidad, 0)) %>%
                group_by(fecha) %>%
                summarise(
                  cantidad = max(cantidad),
                  monto_total = sum(monto_item_total)
                ) %>%
                mutate(articulo = artic()) %>%
                select(articulo, fecha, monto_total, cantidad) %>%
                set_DT(colnames = c("Artículo", "Fecha", "Monto", "Cantidad"),
                       searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
                formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
            } else {
              d_art() %>%
                filter(anio == input$anio.a1[i]) %>%
                # mutate(mes = lubridate::month(fecha, abbr =F, label = F)) %>%
                group_by(mes) %>%
                summarise(
                  monto_total = round_off(sum(monto_item_total), 0),
                  cantidad = n()
                ) %>%
                ungroup() %>%
                arrange(mes) %>%
                mutate(
                  mes = stringr::str_to_title(mes),
                  articulo = artic()
                ) %>%
                select(articulo, mes, monto_total, cantidad) %>%
                set_DT(colnames = c("Artículo", "Mes", "Monto", "Cantidad"),
                       searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
                formatCurrency(columns = 3, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
            }
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.a1[i], "_cuando_a")]] <- renderEcharts4r({
          if (input$plot_type_a == "DIARIO") {
            aa <- d_art() %>%
              filter(anio == input$anio.a1[i]) # %>%
            validate(need(nrow(aa) > 0, ""))

            d_art() %>%
              filter(anio == input$anio.a1[i]) %>%
              data_lines() %>%
              mutate(fecha = as.Date(fecha)) %>%
              echarts_lines(
                varx = "fecha", value = input$mc.a1,
                text = "Ventas diarias (en miles de pesos)"
              )
          } else if (input$plot_type_a == "MENSUAL") {
            if (input$mc.a1 == "M") {
              d_art() %>%
                filter(anio == input$anio.a1[i]) %>%
                group_by(mes) %>% #
                mutate(monto_total = sum(monto_item_total)) %>%
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                # summarise(n = n())
                mutate(monto_total = round_off(monto_total / 1e3, 1)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "monto_total", vary = "mes", color = "#B6D0E2",
                  text = "Ventas mensuales (en miles de pesos)", l = "10%"
                )
            } else if (input$mc.a1 == "C") {
              d_art() %>%
                filter(anio == input$anio.a1[i]) %>%
                group_by(mes) %>% #
                mutate(cantidad = n()) %>%
                ungroup() %>%
                filter(!duplicated(mes)) %>%
                arrange(mes) %>%
                mutate(mes = stringr::str_to_title(mes)) %>%
                echarts_bar(
                  varx = "cantidad", vary = "mes", color = "#B6D0E2",
                  text = "Cantidad de ventas mensuales", l = "10%"
                )
            } else {}
          }
        })
      })
    )

    ###*** A QUIEN **####
    output$tabs_aquien_a <- renderUI({
      nTabs <- length(input$anio.a1)

      myTabs <- lapply(seq_len(nTabs), function(i) {
        tabPanel(
          input$anio.a1[i],
          echarts4rOutput(ns(paste0("p_", input$anio.a1[i], "_aquien_a"))),
          br(), br(), br(), br(), br(),
          DT::DTOutput(ns(paste0("t_", input$anio.a1[i], "_aquien_a"))),
          bsTooltip(id = ns(paste0("t_", input$anio.a1[i], "_aquien_a")), title = pesos_miles)
        )
      })
      do.call(tabsetPanel, myTabs)
    })

    observe(
      lapply(seq_len(length(input$anio.a1)), function(i) {
        output[[paste0("t_", input$anio.a1[i], "_aquien_a")]] <- DT::renderDT(
          {
            d_art() %>%
              filter(anio == input$anio.a1[i]) %>%
              group_by(denominacion_social, organismo_sigla) %>%
              summarise(monto_total = round_off(sum(monto_item_total) / 1e3, 1)) %>%
              # mutate(monto_total = round_off(monto_total / 1e6, 1), trim = TRUE) %>%
              ungroup() %>%
              arrange(-monto_total) %>%
              mutate(articulo = artic()) %>%
              select(articulo, denominacion_social, organismo_sigla, monto_total) %>%
              set_DT(colnames = c("Artículo", "Empresa", "Organismo", "Monto"),
                     searching = TRUE, buttons = TRUE, responsive = TRUE, dom = "Bfrtip") %>%
              formatCurrency(columns = 4, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.a1[i], "_aquien_a")]] <- renderEcharts4r({
          aa <- d_art() %>%
            filter(anio == input$anio.a1[i]) # %>%
          validate(need(nrow(aa) > 0, ""))

          d_art() %>%
            filter(anio == input$anio.a1[i]) %>%
            group_by(denominacion_social, organismo_sigla) %>%
            summarise(
              total = round_off(sum(monto_item_total) / 1e3, 1),
              q = n()
            ) %>%
            ungroup() %>%
            # sankey_data_prov(top = input$top5_a, unidad = input$mc.a1) #%>%
            # top_n(total, input$top5_a) %>%
            echarts_sankey(
              source = "denominacion_social", target = "organismo_sigla", 
              value = input$mc.a1, text = paste("Principales compradores"), t = "8%"
            ) %>%
            e_dims(height = "500", width = "1000")
        })
      })
    )


    ####** COMO #####

    art_como <- reactive({
      d_art() %>%
        left_join(., tipo_compra_codiguera, by = "tipo_compra_codigo") %>%
        group_by(tipo_compra, anio) %>%
        summarise(
          monto_total = sum(monto_item_total),
          cantidad = n()
        ) %>%
        mutate(monto_total = round_off(monto_total / 1e3, 1)) %>%
        ungroup()
    })

    output$tabs_como_a <- renderUI({
      nTabs <- length(input$anio.a1)

      myTabs <- lapply(seq_len(nTabs), function(i) {
        tabPanel(
          input$anio.a1[i],
          echarts4rOutput(ns(paste0("p_", input$anio.a1[i], "_como_a"))),
          br(),
          DT::DTOutput(ns(paste0("t_", input$anio.a1[i], "_como_a"))),
          bsTooltip(id = ns(paste0("t_", input$anio.a1[i], "_como_a")), title = pesos_miles)
        )
      })
      do.call(tabsetPanel, myTabs)
    })

    observe(
      lapply(seq_len(length(input$anio.a1)), function(i) {
        output[[paste0("t_", input$anio.a1[i], "_como_a")]] <- DT::renderDT(
          {
            art_como() %>%
              filter(anio == input$anio.a1[i]) %>%
              arrange(-monto_total) %>%
              mutate(articulo = artic()) %>%
              select(articulo, tipo_compra, anio, monto_total) %>%
              set_DT(colnames = c("Artículo", "Tipo de compra", "Año", "Monto total"),
                      responsive = FALSE, searching = FALSE, info = FALSE, paging = FALSE) %>% # , caption = DT_caption_mil
              formatCurrency(columns = 4, currency = "$", digits = 1, interval = 3, mark = ".", dec.mark = ",")
          },
          server = FALSE
        )

        output[[paste0("p_", input$anio.a1[i], "_como_a")]] <- renderEcharts4r({
          aa <- art_como() %>%
            filter(anio == input$anio.a1[i])
          validate(need(nrow(aa) > 0, ""))

          if (input$mc.a1 == "C") {
            t <- "Tipo de compra seǵun cantidad"
            x <- "cantidad"
          } else {
            t <- "Tipo de compra según monto (en millones de $)"
            x <- "monto_total"
          }

          art_como() %>%
            filter(anio == input$anio.a1[i]) %>%
            echarts_bar(
              vary = "tipo_compra", varx = x, l = "15%",
              text = t, color = "#B6D0E2"
            )
        })
      })
    )

    #### THE END #####
  })
}
