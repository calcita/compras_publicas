################################################################################
# MODULO FAQ
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################
preguntas <- faq(
  data = faq_df, elementId = "faq", faqtitle = "",
  expand_all_button_text = "+ Mostrar todo",
  collapse_all_button_text = "- Ocultar todo"
)

faq_ui <- function(id) { #
  ns <- NS(id)

  htmlTemplate(
    filename = "www/modules/faq/index.html",
    faq = fluidPage(fluidRow(
      faqOutput(ns("faq"))
    ))
  )
}

faq_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$faq <- renderFaq(preguntas)
  })
}
