################################################################################
# UI
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

ui <- htmlTemplate("www/index.html",
                #tags$head(includeHTML("google-analytics.html")),
                carrusel = carrusel_ui("a"),
                explorar = explorar_ui("b"),
                historico = historico_ui("c"),
                datos = datos_ui("d"),
                faq = faq_ui("e")
)



