################################################################################
# MODULO HISTORICO
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

# UI ---------------------------------------------------------------------------

historico_ui <- function(id) { #
  ns <- NS(id)

  htmlTemplate(
    filename = "www/modules/comparativo/index.html",
    panel_comparativo = fluidPage(
      chooseSliderSkin("Square"),
      fluidRow(
        column(
          width = 3
        ),
        column(
          width = 6,
          prettyRadioButtons(
            "nivel3", "",
            c("ORGANISMOS" = "org", "PROVEEDORES" = "prov", "PRODUCTOS" = "arts"),
            inline = TRUE, status = "default"
          )
        ),
        column(
          width = 3
        )
      ),
      #### --------------- ORGANISMOS ---------------- ################
      conditionalPanel(
        condition = "input.nivel3 == 'org'",
        fluidRow(
          column(
            width = 4,
            br(),
            tags$style("#org.2 {border: 2px solid #dd4b39;}"),
            selectizeInput(
              inputId = ns("org.2"), label = "Selecciona organsimos",
              choices = organismos_lista, selected = "ASSE",
              multiple = TRUE
            )
          ),
          column(
            width = 4, br(),
            radioGroupButtons(
              ns("mc.o.2"),
              choices = c("MONTO" = "m.o.2", "CANTIDAD" = "m.c.2"),
              size = "sm", selected = "m.o.2"
            )
          ),
          column(
            width = 4,
            br(),
            sliderInput(
              ns("anio.o2"), "Período:",
              min = 2002, max = 2021, value = c(2002, 2021), sep = ""
            )
          )
        ),
        fluidRow(plotlyOutput(ns("p.area.o")))
      ),

      ##### ---------PROVEEDORES-------######
      conditionalPanel(
        condition = "input.nivel3 == 'prov'",
        fluidRow(
          useShinyjs(),
          column(4),
          column(
            width = 4,
            prettyRadioButtons(
              ns("radio.p2"),
              label = "Seleccionar empresa:",
              choices = list("Por nombre" = "empresa", "Por RUT" = "rut"),
              selected = "empresa", inline = TRUE, status = "default"
            )
          )
        ),
        fluidRow(
          column(
            width = 4,
            div(
              id = ns("combo.e.p2"),
              span(
                dqshiny::autocomplete_input(ns("nombre.p2"),
                  "Empresa", sort(proveedores$denominacion_social),
                  value = "FUNDACION A GANAR",
                  placeholder = "Ingresa nombre de empresa"
                ),
                style = 'color: #86acac; font-size: 80%; style = "border-style: solid; border-color:  #86acac;'
              ),
              textOutput(ns("nxr1.p2"))
            ),
            div(
              id = ns("combo.r.p2"),
              dqshiny::autocomplete_input(ns("rut.p2"),
                "RUT",
                sort(proveedores$codigo_fiscal),
                placeholder = "Ingresa RUT de la empresa",
                value = "215748230018",
                max_options = 50
              ),
              textOutput(ns("nxr2.p2"))
            )
          ),
          column(
            width = 4,
            radioGroupButtons(ns("mc.p.2"),
              choices = c("MONTO" = "p.monto", "CANTIDAD" = "p.cantidad"),
              size = "sm", selected = "p.monto"
            )
          ),
          column(
            width = 4,
            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
            sliderInput(ns("anio.p2"), "Período", min = 2002, max = 2021, value = c(2002, 2021), sep = "")
          )
        ),
        fluidRow(plotlyOutput(ns("p.area.p")))
      ),

      ##### ------------ARTICULOS-----------########
      conditionalPanel(
        condition = "input.nivel3 == 'arts'",
        fluidRow(
          column(4),
          column(
            4,
            prettyRadioButtons(ns("radio.a2"),
              label = "Seleccionar artículo:",
              choices = list("Por nombre" = "articulo", "Por familia" = "familia"),
              selected = "articulo", inline = TRUE, status = "default"
            )
          )
        ),
        fluidRow(
          useShinyjs(),
          column(
            width = 5,
            dqshiny::autocomplete_input(ns("nombre.a2"), "Artículo:", sort(articulos_detalle$articulo), placeholder = "Ingresa nombre del artículo", value = "CAFE", max_options = 50),
            div(
              id = ns("combo.a2"),
              selectInput(ns("familia.a2"), "Familia", multiple = F, choices = sort(familia_codiguera$familia), selected = "MATERIALES Y SUMINISTROS"),
              selectInput(ns("subfam.a2"), "SubFamilia", multiple = F, choices = NULL, selected = "ALIMENTOS Y PRODUCTOS AGROPECUARIOS, FORESTALES Y MARITIMOS"),
              selectInput(ns("articulo.a2"), "Articulo", multiple = F, choices = NULL, selected = "CAFE")
            )
          ),
          column(width = 4, radioGroupButtons(ns("mc.a.2"),
            choices = c("MONTO" = "a.monto", "PRECIO" = "a.cantidad"),
            size = "sm", selected = "a.monto"
          )),
          column(
            width = 4,
            sliderInput(ns("anio.a2"), "Período", min = 2002, max = 2021, value = c(2002, 2021), sep = "")
          )
        ),
        fluidRow(plotlyOutput(ns("p.area.a")))
      )

      #--------------THE END-----------#
    )
  )
}

# SERVER -----------------------------------------------------------------------

historico_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # OBSERVE EVENTS -----------------------------------------------------------
    observeEvent(input$radio.p2, {
      if (input$radio.p2 == "empresa") {
        shinyjs::show("combo.e.p2")
      } else {
        shinyjs::hide("combo.e.p2")
      }
    })
    #
    observeEvent(input$radio.p2, {
      if (input$radio.p2 == "rut") {
        shinyjs::show("combo.r.p2")
      } else {
        shinyjs::hide("combo.r.p2")
      }
    })

    observeEvent(input$radio.a2, {
      if (input$radio.a2 == "articulo") {
        shinyjs::show("nombre.a2")
      } else {
        shinyjs::hide("nombre.a2")
      }
    })

    observeEvent(input$radio.a2, {
      if (input$radio.a2 == "familia") {
        shinyjs::show("combo.a2")
      } else {
        shinyjs::hide("combo.a2")
      }
    })

    observeEvent(input$familia.a2, {
      updateSelectInput(session, "subfam.a2",
        choices = unique(sub_familia_codiguera$sub_familia[sub_familia_codiguera$familia == input$familia.a2])
      )
    })
    observeEvent(input$subfam.a2, {
      updateSelectInput(session, "articulo.a2",
        choices = unique(articulos_detalle$articulo[articulos_detalle$sub_familia == input$subfam.a2])
      )
    })
    

    # ORGANISMOS SERVER --------------------------------------------------------
    compras_ho <- reactive({
      n <- anios_codiguera %>%
        filter(anio >= input$anio.o2[1] & anio <= input$anio.o2[2]) %>%
        select(cod_anio) %>%
        pull()
      d <- rbindlist(compras_all[n])
      compras_ho <- d %>% 
        left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
        mutate(anio = year(fecha)) %>%
        filter(!duplicated(ocid)) %>%
        group_by(anio, organismo_sigla) %>%
        summarise(
          monto_total = sum(monto_compra, na.rm = T),
          cantidad = n()
        ) %>%
        mutate(monto_total = round(monto_total / 1e6), 0) %>%
        ungroup() 
    })

    compras_ho2 <- reactive({
      compras_ho2 <- compras_ho() %>% filter(organismo_sigla %in% input$org.2)
    })

    output$p.area.o <- renderPlotly({
      if (input$mc.o.2 == "m.o.2") {
        aa <- compras_ho2()
        validate(need(nrow(aa) > 0, ""))
        plot_ly(aa,
          x = ~anio, y = ~monto_total, split = ~organismo_sigla, type = "scatter",
          mode = "none", stackgroup = "one"
        ) %>%
          set_config() %>%
          set_layout(title = "Monto total comprado por año (millones de pesos)",
                     showlegend = TRUE,
                     show_lines = FALSE
                     )
      } else if (input$mc.o.2 == "m.c.2") {
        aa <- compras_ho2()
        validate(need(nrow(aa) > 0, ""))
        plot_ly(aa,
          x = ~anio, y = ~cantidad, split = ~organismo_sigla, type = "scatter",
          mode = "none", stackgroup = "one"
        ) %>%
          set_config() %>%
          set_layout(title = "Cantidad de compras por año", 
                     showlegend = TRUE,
                     show_lines = FALSE)
      }
    })

    # PROVEEDORES SERVER --------------------------------------------------------
    compras_hp <- reactive({
      n <- anios_codiguera %>%
        filter(anio >= input$anio.p2[1] & anio <= input$anio.p2[2]) %>%
        select(cod_anio) %>%
        pull()
      compras_hp <- rbindlist(compras_all[n])
    })

    d_hp <- reactive({
      if (input$radio.p2 == "empresa") {
        compras_hp() %>%
          filter(codigo_fiscal == as.character(proveedores[denominacion_social == input$nombre.p2, "codigo_fiscal"])) %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          mutate(anio = year(fecha))
      } else if (input$radio.p2 == "rut") {
        compras_hp() %>%
          filter(codigo_fiscal == as.character(input$rut.p2)) %>%
          left_join(., proveedores %>% filter(codigo_fiscal == as.character(input$rut.p2)), by = "codigo_fiscal") %>%
          left_join(., sub_familia_codiguera, by = c("cod_familia", "cod_sub_familia")) %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          mutate(anio = year(fecha))
      } else {}
    })

    emp <- reactive({
      d_hp()$denominacion_social[1]
    })

    output$p.area.p <- renderPlotly({
      if (input$mc.p.2 == "p.monto") {
        cp <- d_hp() %>%
          group_by(anio, denominacion_social) %>%
          summarise(monto_total = sum(monto_item_total, na.rm = T), .groups = "drop") %>%
          mutate(monto_total = round(monto_total, 0) / 1e3) %>%
          ungroup() %>%
          pivot_wider(names_from = denominacion_social, values_from = monto_total)
        validate(need(nrow(cp) > 0, "No hay datos para realizar el gráfico"))

        if (nrow(cp) == 1) {
          cp[2, 1] <- cp$anio[1] - 1
          cp[2, 2] <- 0
          cp[3, 1] <- cp$anio[1] + 1
          cp[3, 2] <- 0
        }

        plot_ly(cp,
          x = ~anio, y = ~ cp[[emp()]], type = "scatter",
          mode = "none", stackgroup = "one", fillcolor = color_am,
          name = emp()
        ) %>%
          set_config() %>%
          set_layout(title = "Monto total vendido por año (miles de pesos)",
                     showlegend = TRUE,
                     show_lines = FALSE
                     )
      } else if (input$mc.p.2 == "p.cantidad") {
        cp <- d_hp() %>%
          group_by(anio, denominacion_social) %>%
          summarise(cantidad = n()) %>%
          ungroup() %>%
          pivot_wider(names_from = denominacion_social, values_from = cantidad)
        validate(need(nrow(cp) > 0, "No hay datos para graficar"))

        if (nrow(cp) == 1) {
          cp[2, 1] <- cp$anio[1] - 1
          cp[2, 2] <- 0
          cp[3, 1] <- cp$anio[1] + 1
          cp[3, 2] <- 0
        }

        plot_ly(cp,
          x = ~anio, y = ~ cp[[emp()]], type = "scatter",
          mode = "none", stackgroup = "one", fillcolor = color_ac,
          name = emp()
        ) %>%
          set_config() %>%
          set_layout(title = "Cantidad de ventas por año", 
                     showlegend = TRUE,
                     show_lines = FALSE)
      } else {}
    })

    # ARTICULOS SERVER --------------------------------------------------------

    compras_ha <- reactive({
      n <- anios_codiguera %>%
        filter(anio >= input$anio.a2[1] & anio <= input$anio.a2[2]) %>%
        select(cod_anio) %>%
        pull()
      compras_ha <- rbindlist(compras_all[n])
    })

    d_ha <- reactive({
      if (input$radio.a2 == "familia") {
        compras_ha() %>%
          filter(cod_familia != 3) %>%
          filter(iditem == as.character(articulos[articulo == input$articulo.a2, "iditem"])) %>%
          left_join(., articulos %>% select(articulo, iditem), by = "iditem") %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          mutate(anio = year(fecha))
      } else if (input$radio.a2 == "articulo") {
        compras_ha() %>%
          filter(cod_familia != 3) %>%
          filter(iditem == as.character(articulos[articulo == input$nombre.a2, "iditem"])) %>%
          left_join(., articulos %>% select(articulo, iditem), by = "iditem") %>%
          left_join(., organismos %>% select(organismo_sigla, organismo_codigo), by = "organismo_codigo") %>%
          left_join(., proveedores, by = "codigo_fiscal") %>%
          mutate(anio = year(fecha))
      } else {}
    })

    artic <- reactive(d_ha()$articulo[1])
    miny <- reactive(min(d_ha()$anio))
    maxy <- reactive(max(d_ha()$anio))

    d_hap <- reactive({
      violin_data(
        data = precios_unit, year_from = miny(),
        year_to = maxy(), item = artic()
      )
    })

    output$p.area.a <- renderPlotly({
      if (input$mc.a.2 == "a.monto") {
        cp <- d_ha() %>%
          group_by(anio, articulo) %>%
          summarise(monto_total = sum(monto_item_total, na.rm = T)) %>%
          mutate(monto_total = round(monto_total, 0) / 1e3) %>%
          ungroup() %>%
          pivot_wider(names_from = articulo, values_from = monto_total)
        validate(need(nrow(cp) > 0, ""))

        plot_ly(cp,
          x = ~anio, y = ~ cp[[artic()]], type = "scatter",
          mode = "none", stackgroup = "one", fillcolor = color_am,
          name = artic()
        ) %>%
          set_config() %>%
          set_layout(title = "Monto total vendido por año (miles de pesos)",
                     showlegend = TRUE,
                     show_lines = FALSE
                     )
      } else if (input$mc.a.2 == "a.cantidad") {
        d_hap() %>%
          plot_ly(
            x = ~anio, y = ~monto_estandar, split = ~anio, type = "violin",
            box = list(visible = T), meanline = list(visible = F), spanmode = "hard", # hoverinfo="none"
            text = ~monto_estandar, hoverinfo = "y"
          ) %>%
          set_layout(font = list(color = "#515656"), show_lines = FALSE) %>% # font= list(family = "Arial", size = 10)
          set_config()
      } else {}
    })
    ## the end ####
  })
}
