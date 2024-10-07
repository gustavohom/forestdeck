#' Calcular a Altura no Empacotamento Hexagonal de Circulos
#'
#' Esta funcao calcula a distancia vertical necessaria para organizar circulos em um padrao
#' de empacotamento hexagonal, que e o metodo mais eficiente para empacotar circulos sem sobreposicao.
#' O empacotamento hexagonal maximiza o uso do espaco, reduzindo o desperdicio e aumentando a
#' densidade de empacotamento em comparacao com outros metodos, como o empacotamento quadrado.
#'
#' ## Empacotamento Hexagonal para Circulos
#'
#' No empacotamento hexagonal de circulos, cada circulo e posicionado de forma que seja cercado
#' por seis outros circulos, formando uma estrutura similar a uma colmeia. Este arranjo resulta
#' em uma maxima densidade de empacotamento de aproximadamente 90,7%, o que significa que 90,7%
#' da area esta ocupada pelos circulos, minimizando o espaco vazio.
#'
#' @param diameter Diametro dos circulos geometricos. Este parametro e essencial para
#'   determinar o tamanho das formas a serem empacotadas.
#'
#' @return Retorna a distancia vertical calculada necessaria para o empacotamento hexagonal de circulos.
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo: Calcular a altura para circulos com diametro 10
#' altura <- height_circle(diameter = 10)
#' print(altura)
#' }
#'
#' @export
height_circle <- function(diameter) {
  r <- diameter / 2
  y_shift <- sqrt(3) * r
  return(y_shift)
}
