################################################################################
# SERVER
#
# AUTORA: GABRIELA MATHIEU
# REPOSITORIO: https://github.com/calcita/compras_publicas
################################################################################

server <- function(input,
                   output,
                   session){ 
  carrusel_server("a")
  explorar_server("b")
  historico_server("c")
  datos_server("d")
  faq_server("e")
}