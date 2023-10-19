##### MODULO DATOS #####
#------------------------#
# Autora: Gabriela Mathieu
# Fuente: https://github.com/calcita/compras_publicas
#------------------------#

variables <- c("fecha", "iditem", "monto_item", "cantidad", "monto_compra", "organismo_sigla", "codigo_fiscal", "cod_compra")
font.size <- "8pt"
conjuntos <- c("COMPRAS", "PRODUCTOS", "FAMILIA", "SUBFAMILIA", "PROVEEDORES", "TIPO_COMPRA", "DICCIONARIO")

datos_ui <- function(id) { #
  ns <- NS(id)

  htmlTemplate(
    filename = "www/modules/datos/index.html",
    panel_datos = fluidPage(
      fluidRow(
        column(width = 1),
        column(width = 3, selectInput(ns("sets"), "Conjunto de datos", multiple = F, choices = conjuntos, selected = "PRODUCTOS")),
        column(width = 3, selectizeInput(ns("vars"), "Variables", multiple = T, choices = variables, selected = variables)),
        column(width = 2, selectizeInput(ns("anio_p4"), "Año", multiple = T, choices = anios, selected = 2021)),
        column(width = 2, selectInput(ns("orga_p4"), "Organismo", multiple = F, choices = organismos$organismo_sigla, selected = "Presidencia"))
      ),
      fluidRow(
        column(width = 1),
        column(width = 10, DTOutput(ns("tab_datos_filtrados"))),
        column(width = 1)
      )
    )
  )
}

### server

datos_server <- function(id) { # , data_organismo

  moduleServer(id, function(input, output, session) {
    observeEvent(input$sets, {
      if (input$sets == "COMPRAS") {
        shinyjs::show("vars")
        shinyjs::show("anio_p4")
        shinyjs::show("orga_p4")
      } else {
        shinyjs::hide("vars")
        shinyjs::hide("anio_p4")
        shinyjs::hide("orga_p4")
      }
    })

    compras_periodo <- reactive({
      n <- anios_codiguera %>%
        filter(anio %in% input$anio_p4) %>%
        select(cod_anio) %>%
        pull()
      compras_periodo <- rbindlist(compras_all[n]) %>% 
        select(-unidad, -monto_item_total, - ends_with("estandar"))
    })

    datos_filtrados <- reactive({
      if (input$sets == "COMPRAS") {
        var_list <- dplyr::syms(input$vars)
        compras_periodo() %>%
          rename(cod_compra = tipo_compra_codigo) %>%
          mutate(cod_compra = as.integer(cod_compra)) %>%
          mutate(
            monto_compra = round(monto_compra, 0),
            monto_item = round(monto_item, 0)
          ) %>%
          left_join(., organismos %>% select(organismo_codigo, organismo_sigla), by = "organismo_codigo") %>%
          filter(organismo_sigla == input$orga_p4) %>%
          select(ocid, !!!var_list) %>%
          mutate(url = create_link(ocid))
      } else if (input$sets == "PRODUCTOS") {
        articulos %>%
          rename(
            cod_fam = cod_familia,
            cod_subfam = cod_sub_familia
          )
      } else if (input$sets == "FAMILIA") {
        familia_codiguera %>%
          rename(cod_fam = cod_familia)
      } else if (input$sets == "SUBFAMILIA") {
        sub_familia_codiguera %>%
          rename(cod_subfam = cod_sub_familia)
      } else if (input$sets == "PROVEEDORES") {
        proveedores
      } else if (input$sets == "TIPO_COMPRA") {
        tipo_compra_codiguera
      } else {
        diccionario
      }
    })

    output$tab_datos_filtrados <- renderDT(
      {
        if (length(input$vars) == 0) {
          print("Debe seleccionar alguna variable")
        } else {
          datos_filtrados() %>%
            # slice(1:10) %>%
            datatable(
              rownames = FALSE, extensions = "Buttons", escape = FALSE,
              class = "display compact",
              caption = "", width = "80%",
              filter = "none",
              options = list(
                searching = TRUE, dom = "Bfrtip", buttons = c("csv", "excel", "print"),
                responsive = TRUE,
                initComplete = htmlwidgets::JS(
                  "function(settings, json) {",
                  paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
                  "}"
                ),
                language = list(
                  paginate =
                    list(
                      "next" = "Siguiente",
                      previous = "Anterior"
                    ),
                  info = "Mostrando del _START_ al _END_ de un total de _TOTAL_ registros",
                  search = "Buscar",
                  headerCallback = DT::JS(
                    "function(thead) {",
                    "  $(thead).css('font-size', '0.8em');",
                    "}"
                  ), lengthMenu = "_MENU_"
                )
              )
            ) %>%
            DT::formatStyle(columns = colnames(datos_filtrados()), fontSize = "100%", fontFamily = "ubuntu", fontWeight = "100%")
        }
      },
      server = FALSE
    )

    ### fin
  })
}
