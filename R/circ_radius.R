#' Calcular Raio Necessario com os Parametros Fornecidos
#'
#' Esta funcao calcula o raio necessario para incluir um numero especifico de arvores com base nos parametros fornecidos.
#'
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros). Opcional se \code{n} for fornecido.
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros). Opcional se \code{n} for fornecido.
#' @param n Numero de arvores desejado. Opcional se \code{area}, \code{side_x} e \code{side_y} forem fornecidos.
#' @param methodology Metodologia a ser usada. Opcoes: "central", "vertical", "horizontal", "quadrante". Padrao e "central".
#' @param area Area da parcela. Pode ser fornecida em hectares ("ha") ou metros quadrados ("m2"). Opcional se \code{side_x} e \code{side_y} forem fornecidos.
#' @param side_x Comprimento do lado X da parcela (em metros). Opcional se \code{area} for fornecida.
#' @param side_y Comprimento do lado Y da parcela (em metros). Opcional se \code{area} for fornecida.
#' @param units Unidade de medida da area. Pode ser "ha" para hectares ou "m2" para metros quadrados. Padrao e "ha".
#' @param option Opcao de arredondamento para o numero de arvores. Pode ser "inferior" para arredondar para baixo, "superior" para arredondar para cima ou "exato" para manter o valor exato. Padrao e "inferior".
#'
#' @return Retorna o raio (em metros) necessario para incluir o numero desejado de arvores.
#'
#' @examples
#' # Exemplo 1: Calcular o raio para 1666 arvores
#' circ_radius(spacing_x = 2, spacing_y = 2, n = 1666, methodology = "central")
#'
#' # Exemplo 2: Calcular o raio para 2 hectares com metodologia quadrante
#' circ_radius(spacing_x = 3, area = 2, methodology = "quadrante", units = "ha", option = "superior")
#'
#' @export

circ_radius <- function(spacing_x = NULL,
                        spacing_y = NULL,
                        n = NULL,
                        methodology = "central",
                        area = NULL,
                        side_x = NULL,
                        side_y = NULL,
                        units = "ha",
                        option = "inferior") {

  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  # Definir opcoes permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opcao escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o numero de arvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando número de árvores fornecido: ", desired_trees)
  } else {
    # Calcular o numero de arvores usando a funcao cir_ntrees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y,
                             area = area, side_x = side_x, side_y = side_y,
                             units = units, option = option)
    message("Número de árvores calculado: ", desired_trees)
  }

  if (is.null(spacing_y)){spacing_y <- spacing_x}

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de arvores usando expanded_tree_grid
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia usando cir_central
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário usando circ_radius_for_trees
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  cat("Raio necessário para incluir", desired_trees, "árvores:", format(round(radius, 1), nsmall = 1), "metros\n")

  return(radius)
}
