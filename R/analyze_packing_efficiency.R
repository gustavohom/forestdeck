#' Analisar a Eficiencia do Empacotamento Hexagonal de Formas Geometricas
#'
#' Esta funcao calcula o numero de formas geometricas (circulos ou obrounds) que podem ser
#' empacotadas eficientemente dentro de uma area definida utilizando o metodo de empacotamento
#' hexagonal. Alem disso, compara a capacidade do empacotamento hexagonal com o empacotamento
#' retangular convencional, fornecendo uma analise detalhada da eficiencia.
#'
#' ## Empacotamento Hexagonal para Circulos
#'
#' No empacotamento hexagonal de circulos, cada circulo e cercado por seis outros circulos,
#' formando uma estrutura semelhante a uma colmeia. Este arranjo resulta em uma densidade de
#' empacotamento maxima de aproximadamente 90,7%, otimizando o uso do espaco disponivel.
#'
#' ## Empacotamento Hexagonal Adaptado para Obrounds
#'
#' Quando a forma especificada e \code{"semi_rect"}, a funcao considera uma geometria composta
#' por dois semicirculos unidos por um retangulo, formando uma **obround** (tambem conhecida como
#' **estadio** ou **discoide** em portugues). Este arranjo combina a eficiencia do empacotamento
#' de semicirculos com a versatilidade do retangulo, resultando em uma forma que pode ser utilizada
#' em diversas aplicacões geometricas.
#'
#' @param area_size Tamanho da area disponivel para empacotamento (por exemplo, em unidades).
#' @param diameter Diametro das figuras geometricas (circulos ou semicirculos). Este parametro e essencial para determinar o tamanho das formas a serem empacotadas.
#' @param shape Forma das figuras a serem empacotadas. Pode ser "circle" para empacotamento de circulos ou "semi_rect" para empacotamento de semicirculos unidos por retangulo.
#' @param rect_height Altura do retangulo que une os semicirculos. Este parametro e necessario apenas quando shape e "semi_rect".
#' @param print_results Booleano que determina se os resultados detalhados devem ser impressos. TRUE para imprimir e FALSE para nao imprimir. Padrao e FALSE.
#'
#' @return Retorna uma lista com os seguintes componentes:
#' \describe{
#'   \item{\code{total_shapes}}{Numero total de formas que cabem na area definida utilizando empacotamento hexagonal.}
#'   \item{\code{odd_rows}}{Numero de fileiras impares no empacotamento.}
#'   \item{\code{even_rows}}{Numero de fileiras pares no empacotamento.}
#'   \item{\code{shapes_per_row_odd}}{Numero de formas por fileira impar.}
#'   \item{\code{shapes_per_row_even}}{Numero de formas por fileira par.}
#'   \item{\code{num_rows}}{Numero total de fileiras no empacotamento.}
#'   \item{\code{rect_packing_shapes}}{Numero de formas que caberiam na area utilizando empacotamento retangular convencional.}
#'   \item{\code{increase_percentage}}{Porcentagem de aumento na capacidade de empacotamento hexagonal em relacao ao empacotamento retangular.}
#'   \item{\code{altura_total_empacotamento}}{Altura total do empacotamento hexagonal.}
#'   \item{\code{altura_superior_empacotamento}}{Altura superior do empacotamento hexagonal.}
#' }
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo 1: Analisar empacotamento hexagonal de circulos com diametro 10 em uma area de 100 unidades
#' resultado_circulos <- analyze_packing_efficiency(area_size = 100, diameter = 10, shape = "circle", print_results = TRUE)
#' print(resultado_circulos)
#'
#' # Exemplo 2: Analisar empacotamento hexagonal adaptado para obrounds com diametro 6 e altura do retangulo 2 em uma area de 100000 unidades
#' resultado_obrounds <- analyze_packing_efficiency(area_size = 100000, diameter = 6, shape = "semi_rect", rect_height = 2, print_results = TRUE)
#' print(resultado_obrounds)
#' }
#' @export
analyze_packing_efficiency <- function(area_size, diameter, shape = "circle", rect_height = NULL, print_results = FALSE) {
  # Calcular o raio
  r <- diameter / 2
  X <- diameter  # Distancia horizontal entre os centros

  # Calcular a distancia vertical entre as fileiras
  Y <- if (shape == "circle") {
    height_circle(diameter)
  } else if (shape == "semi_rect") {
    height_semi_rect(diameter, rect_height)
  } else {
    stop("Forma nao reconhecida. Use 'circle' ou 'semi_rect'.")
  }

  # Calcular o numero de fileiras que cabem na area
  if (shape == "semi_rect") {
    altura_total_empacotamento <- diameter + rect_height  # Altura da primeira fileira
    num_rows <- 1
    while (altura_total_empacotamento + Y <= area_size) {
      num_rows <- num_rows + 1
      altura_total_empacotamento <- altura_total_empacotamento + Y
    }
  } else {
    altura_total_empacotamento <- 0
    num_rows <- 0
    while (altura_total_empacotamento + Y <= area_size) {
      num_rows <- num_rows + 1
      altura_total_empacotamento <- altura_total_empacotamento + Y
    }
  }

  # Numero de formas nas fileiras impares
  shapes_per_row_odd <- floor(area_size / X)

  # Numero de formas nas fileiras pares (deslocadas horizontalmente pela metade do diametro)
  adjusted_width <- area_size - (X / 2)
  shapes_per_row_even <- floor(adjusted_width / X)

  # Contar quantas fileiras sao impares e quantas sao pares
  odd_rows <- ceiling(num_rows / 2)
  even_rows <- floor(num_rows / 2)

  # Calcular o total de formas utilizando empacotamento hexagonal
  total_shapes <- (odd_rows * shapes_per_row_odd) + (even_rows * shapes_per_row_even)

  # Calculo da capacidade no empacotamento retangular (sem otimizacao)
  rect_packing_shapes <- floor(area_size / diameter) * floor(area_size / ifelse(shape == "circle", diameter, diameter + rect_height))

  # Calculo da porcentagem de aumento
  increase_percentage <- if (rect_packing_shapes == 0) {
    NA
  } else {
    (total_shapes - rect_packing_shapes) / rect_packing_shapes * 100
  }

  # Calcular a altura total empacotamento e altura superior empacotamento
  altura_total_empacotamento <- if (shape == "circle") {
    diameter + (num_rows - 1) * height_circle(diameter)
  } else {
    (diameter + rect_height) + (num_rows - 1) * height_semi_rect(diameter, rect_height)
  }

  altura_superior_empacotamento <- if (shape == "circle") {
    diameter + num_rows * height_circle(diameter)
  } else {
    (diameter + rect_height) + num_rows * height_semi_rect(diameter, rect_height)
  }

  # Preparar os resultados
  resultados <- list(
    total_shapes = total_shapes,
    odd_rows = odd_rows,
    even_rows = even_rows,
    shapes_per_row_odd = shapes_per_row_odd,
    shapes_per_row_even = shapes_per_row_even,
    num_rows = num_rows,
    rect_packing_shapes = rect_packing_shapes,
    increase_percentage = increase_percentage,
    altura_total_empacotamento = altura_total_empacotamento,
    altura_superior_empacotamento = altura_superior_empacotamento
  )

  # Imprimir os resultados detalhados, se solicitado
  if (print_results) {
    cat('---------------------------------------------------------\n')
    if (shape == "circle") {
      cat("Em uma area de", area_size, "unidades, o empacotamento hexagonal de circulos aumentou a capacidade de",
          rect_packing_shapes, "para", total_shapes, "\n",
          "Ganho equivalente de", round(increase_percentage, 2), "%\n")
    } else if (shape == "semi_rect") {
      cat("Em uma area de", area_size, "unidades, o empacotamento hexagonal de obrounds aumentou a capacidade de",
          rect_packing_shapes, "para", total_shapes, "\n",
          "Ganho equivalente de", round(increase_percentage, 2), "%\n")
    }
    cat('---------------------------------------------------------\n')
    cat("Altura Total do Empacotamento:", altura_total_empacotamento, "\n")
    cat("Altura Superior do Empacotamento:", altura_superior_empacotamento, "\n")
  }

  return(resultados)
}
