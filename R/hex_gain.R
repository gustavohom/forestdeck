#' Analisar a Eficiencia do Empacotamento Hexagonal de Formas Geometricas
#'
#' Esta funcao calcula o ganho percentual na capacidade de empacotamento de formas geometricas
#' (circulos ou obrounds) utilizando o metodo hexagonal em comparacao com o empacotamento retangular.
#'
#' @param D Diametro das formas geometricas (em unidades).
#' @param S Tamanho da area disponivel para empacotamento (em unidades).
#' @param shape Forma geometrica a ser empacotada. Pode ser "circle" para circulos ou "semi_rect" para
#' semicerculos unidos por retangulo. Default e "circle".
#' @param rect_height Altura do retangulo que une os semicerculos. Necessario apenas quando shape e "semi_rect".
#' @param xlim Valor maximo do plot do grafico
#' @param exibir_resultados Booleano que determina se os resultados detalhados devem ser exibidos.
#' Default e FALSE.
#' @param exibir_plot Plot o grafico de ganho em relação a altura quando `semi_rect`.
#' Default e TRUE.
#'
#' @return Retorna uma lista com os seguintes componentes:
#' \describe{
#'   \item{total_shapes}{Numero total de formas que cabem na area utilizando empacotamento hexagonal.}
#'   \item{odd_rows}{Numero de fileiras impares no empacotamento.}
#'   \item{even_rows}{Numero de fileiras pares no empacotamento.}
#'   \item{shapes_per_row_odd}{Numero de formas por fileira impar.}
#'   \item{shapes_per_row_even}{Numero de formas por fileira par.}
#'   \item{num_rows}{Numero total de fileiras no empacotamento.}
#'   \item{rect_packing_shapes}{Numero de formas que caberiam na area utilizando empacotamento retangular.}
#'   \item{increase_percentage}{Porcentagem de aumento na capacidade com empacotamento hexagonal.}
#'   \item{altura_total_empacotamento}{Altura total do empacotamento hexagonal.}
#'   \item{altura_superior_empacotamento}{Altura superior do empacotamento hexagonal.}
#' }
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo 1: Analisar empacotamento hexagonal de circulos com diametro 1 em uma area de 1000000 unidades
#' resultado_circulos <- calculate_gain(D = 1, S = 10000, shape = "circle", exibir_resultados = TRUE)
#'
#' # Exemplo 2: Analisar empacotamento hexagonal de obrounds com diametro 1, altura do retangulo 2 em uma area de 1000000 unidades
#' resultado_obrounds <- calculate_gain(D = 1, S = 10000, shape = "semi_rect", rect_height = 2, exibir_resultados = TRUE)
#' }
#'
#' @export
hex_gain <- function(D, S, shape = "circle", rect_height = NULL, xlim = 20,
                           exibir_resultados = FALSE, exibir_plot = TRUE) {

  # Verificar se shape e semi_rect e se rect_height foi fornecido
  if (shape == "semi_rect" && is.null(rect_height)) {
    stop("Para shape 'semi_rect', o parametro 'rect_height' deve ser fornecido.")
  }

  # Vetor de H variando de 0 a xlim com passo de 1
  H_values <- seq(0, xlim, by = 1)

  # Funcao para calcular o ganho continuo
  calculate_gain <- function(H, D) {
    numerator <- (2 / (sqrt(3) * D + 2 * H)) * (D + H) - 1
    gain <- numerator * 100
    return(gain)
  }

  # Calcular o ganho para cada H
  gain_values <- sapply(H_values, calculate_gain, D = D)

  if(shape == "semi_rect" && exibir_plot == TRUE){
  # Plotar o grafico
  plot(H_values, gain_values, type = "l", col = "blue", lwd = 2,
       xlab = "Altura do Retangulo H (m)",
       ylab = "Porcentagem de Ganho (%)",
       main = "Relacao entre H e a Porcentagem de Ganho")

  graphics::abline(h = gain_values[length(gain_values)], col = "red", lty = 2)

  }

  # Calculo da capacidade no empacotamento retangular (sem otimizacao)
  if (shape == "circle") {
    num_shapes_rectangular <- floor(S / D) * floor(S / D)
  } else if (shape == "semi_rect") {
    num_shapes_rectangular <- floor(S / D) * floor(S / (D + rect_height))
  } else {
    num_shapes_rectangular <- NA
  }

  # Calculo da capacidade utilizando empacotamento hexagonal
  if (shape == "circle") {
    calc_shapes <- hex_efficiency (area_size = S, diameter = D, shape = "circle", rect_height = NULL)
    total_shapes <- calc_shapes$total_shapes
    increase_percentage <- (total_shapes - num_shapes_rectangular) / num_shapes_rectangular * 100
  } else if (shape == "semi_rect") {
    calc_shapes <- hex_efficiency (area_size = S, diameter = D, shape = "semi_rect", rect_height = rect_height)
    total_shapes <- calc_shapes$total_shapes
    increase_percentage <- (total_shapes - num_shapes_rectangular) / num_shapes_rectangular * 100
  } else {
    total_shapes <- NA
    increase_percentage <- NA
  }

  # Exibir os resultados detalhados, se solicitado
  if (exibir_resultados) {
    cat('---------------------------------------------------------\n')
    if (shape == "circle") {
      cat("Em uma area de", S, "unidades, a capacidade aumentou de",
          num_shapes_rectangular, "para", total_shapes, "\n",
          "Ganho equivalente de", round(increase_percentage, 2), "%\n")
    } else if (shape == "semi_rect") {
      cat("Em uma area de", S, "unidades, a capacidade aumentou de",
          num_shapes_rectangular, "para", total_shapes, "\n",
          "Ganho equivalente de", round(increase_percentage, 2), "%\n")
    }
    cat('---------------------------------------------------------\n')

    # Calcular a altura total empacotamento e altura superior empacotamento
    if (shape == "circle") {
      altura_total_empacotamento <- D + (calc_shapes$num_rows - 1) * hex_height_circle(D)
      altura_superior_empacotamento <- D + calc_shapes$num_rows * hex_height_circle(D)
    } else if (shape == "semi_rect") {
      altura_total_empacotamento <- (D + rect_height) + (calc_shapes$num_rows - 1) * hex_height_semi_rect(D, rect_height)
      altura_superior_empacotamento <- (D + rect_height) + calc_shapes$num_rows * hex_height_semi_rect(D, rect_height)
    } else {
      altura_total_empacotamento <- NA
      altura_superior_empacotamento <- NA
    }

    cat("Altura Total do Empacotamento:", altura_total_empacotamento, "\n")
    cat("Altura Superior do Empacotamento:", altura_superior_empacotamento, "\n")
  }

  # Retornar os resultados
  return(list(
    H_values = H_values,
    gain_values = gain_values,
    total_shapes = total_shapes,
    rect_packing_shapes = num_shapes_rectangular,
    increase_percentage = increase_percentage,
    altura_total_empacotamento = altura_total_empacotamento,
    altura_superior_empacotamento = altura_superior_empacotamento
  ))
}
