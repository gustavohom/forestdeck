#' Plotar Arvores e Circulo
#'
#' Esta funcao plota as arvores e um circulo com base nos parametros fornecidos. Utiliza a metodologia especificada para determinar o ponto central e o raio do circulo que engloba o numero desejado de arvores.
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
#' @return Nenhum. A funcao gera um grafico das arvores e do circulo.
#'
#' @examples
#' # Exemplo 1: Plotar com espacamento 2m x 2m em 1 ha, arredondando para baixo
#' circ_plot(spacing_x = 2, spacing_y = 2, area = 1, units = "ha", option = "inferior")
#'
#' # Exemplo 2: Plotar com espacamento 3m x 3m em 5000 m2, arredondando para cima
#' circ_plot(spacing_x = 3, spacing_y = 3, area = 5000, units = "m2", option = "superior")
#'
#' # Exemplo 3: Plotar a partir dos lados da parcela (100m x 50m) com espacamento 2m, arredondando para exato
#' circ_plot(spacing_x = 2, spacing_y = 2, side_x = 100, side_y = 50, units = "m2", option = "exato")
#'
#' # Exemplo 4: Plotar fornecendo apenas o espacamento na direcao X (assume spacing_y = spacing_x)
#' circ_plot(spacing_x = 2.5, area = 2, option = "inferior")
#'
#' @export

circ_plot <- function(spacing_x = NULL, spacing_y = NULL, n = NULL,
                      methodology = "central",
                      area = NULL, side_x = NULL, side_y = NULL,
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
    # Calcular o numero de arvores usando a funcao n_trees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y, area = area,
                             side_x = side_x, side_y = side_y,
                             units = units,
                             option = option)
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

  # Obter o ponto central de acordo com a metodologia usando circ_central_point
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário usando circ_radius_for_trees
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  # Filtrar as arvores dentro do circulo usando circ_filter
  trees_circle <- circ_filter(trees, central_point, radius)

  # Plotar os resultados usando cir_plot
  plot_title <- paste("Metodologia", methodology)
  circ_aux_plot(trees, trees_circle, central_point, radius, plot_title)
}
