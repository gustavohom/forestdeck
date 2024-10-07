#' Calcular a Altura no Empacotamento Hexagonal de Obrounds
#'
#' Esta funcao calcula a distancia vertical necessaria para organizar formas geometricas
#' conhecidas como obrounds em um padrao de empacotamento hexagonal. Um obround, tambem
#' chamado de estadio ou discoide, e composto por dois semicerculos unidos por um retangulo.
#' Este arranjo combina a eficiencia do empacotamento de semicerculos com a versatilidade do
#' retangulo, resultando em uma forma que pode ser usada em diversas aplicacoes geometricas.
#'
#' ## Empacotamento Hexagonal Adaptado para Obrounds
#'
#' No empacotamento hexagonal de obrounds, cada obround e posicionado de forma que seja cercado
#' por seis outros obrounds, semelhante ao empacotamento de circulos. Esta configuracao resulta em
#' uma maxima densidade de empacotamento, otimizando o uso do espaco disponivel.
#'
#' @param diameter Diametro dos semicerculos que compoem o obround. Este parametro
#'   determina a largura da forma geometrica.
#' @param rect_height Altura do retangulo que une os semicerculos. Este parametro
#'   define a altura adicional da forma geometrica, resultando na altura total do obround.
#'
#' @return Retorna a distancia vertical calculada necessaria para o empacotamento hexagonal de obrounds.
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo: Calcular a altura para obrounds com diametro 6 e altura do retangulo 2
#'
#' altura_obround <- hex_height_semi_rect(diameter = 6, rect_height = 2)
#'
#' print(altura_obround)
#' }
#'
#' @export
hex_height_semi_rect <- function(diameter, rect_height) {
  r <- diameter / 2
  y_shift <- rect_height + sqrt(3) * r  # Soma a altura do retangulo e a distancia entre os semicerculos
  return(y_shift)
}
