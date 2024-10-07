#' Calcular a Altura entre Linhas no Empacotamento Hexagonal
#'
#' Esta funcao calcula a distancia vertical necessaria para organizar figuras geometricas
#' em um padrao de empacotamento hexagonal, que e o metodo mais eficiente para
#' empacotar circulos sem sobreposicao. O empacotamento hexagonal maximiza o uso do espaco,
#' reduzindo o desperdicio e aumentando a densidade de empacotamento em comparacao
#' com outros metodos, como o empacotamento quadrado.
#'
#' ## Empacotamento Hexagonal para Circulos
#'
#' No caso de empacotamento de circulos, o empacotamento hexagonal posiciona cada circulo
#' de forma que cada um seja cercado por seis outros circulos, formando uma estrutura em
#' formato de colmeia.
#'
#' ## Uso com Semicirculos e Retangulos
#'
#' Quando a forma especificada e `semi_rect`, a funcao considera uma geometria composta por
#' dois semicirculos unidos por um retangulo. Neste caso, os semicirculos utilizam o raio
#' definido e o retangulo e acrescentado entre eles com uma altura especificada. Por exemplo,
#' um semicirculo com raio 3 e um retangulo com altura 2 resultaria em uma forma geometrica
#' conhecida como **obround** (tambem chamada de **estadio** ou **discóide** em português).
#' Esta forma teria uma largura de 6 unidades (diametro dos semicirculos) e uma altura de 5
#' unidades (raio + altura do retangulo).
#'
#' @param diameter Diametro das figuras geometricas. Este parametro e essencial para
#'   determinar o tamanho das formas a serem empacotadas.
#' @param shape Forma das figuras a serem empacotadas. Pode ser "circle"
#'   para empacotamento de circulos ou "semi_rect" para empacotamento de
#'   semicirculos unidos por um retangulo.
#' @param rect_height Altura do retangulo que une os semicirculos. Este parametro
#'   e necessario apenas quando shape e "semi_rect".
#'
#' @return Retorna a distancia vertical calculada necessaria para o empacotamento das figuras.
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo 1: Empacotamento hexagonal de circulos com raio 5
#' altura_circulo <- hex_height(diameter = 3, shape = "circle")
#' print(altura_circulo)
#'
#' # Exemplo 2: Empacotamento de semicirculos unidos por um retangulo com raio 3 e altura do retangulo 2
#' altura_semi_rect <- hex_height(diameter = 2, shape = "semi_rect", rect_height = 1)
#' print(altura_semi_rect)
#'
#' }
#'
#' @export
hex_height <- function(diameter, shape = "circle", rect_height = NULL) {
  if (shape == "circle") {
    y_shift <- hex_height_circle(diameter)
  } else if (shape == "semi_rect") {
    if (is.null(rect_height)) {
      stop("O parametro 'rect_height' deve ser fornecido para a forma 'semi_rect'.")
    }
    y_shift <- hex_height_semi_rect(diameter, rect_height)
  } else {
    stop("Forma nao reconhecida. Use 'circle' ou 'semi_rect'.")
  }
  return(y_shift)
}
