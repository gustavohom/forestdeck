#' Calculo das principais metricas e plots para parcelas circulares
#'
#' Esta funcao executa todo o processo de calculo e plotagem das metricas de parcelas circulares de inventario de arvores.
#'
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros). Opcional se n for fornecido.
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros). Opcional se n for fornecido.
#' @param n Numero de arvores desejado. Opcional se area, side_x e side_y forem fornecidos.
#' @param methodology Metodologia a ser usada. Opcoes: "central", "vertical", "horizontal", "quadrante". Padrao e "central".
#' @param area Area da parcela. Pode ser fornecida em hectares ("ha") ou metros quadrados ("m2"). Opcional se side_x e side_y forem fornecidos.
#' @param side_x Comprimento do lado X da parcela (em metros). Opcional se area for fornecida.
#' @param side_y Comprimento do lado Y da parcela (em metros). Opcional se area for fornecida.
#' @param units Unidade de medida da area. Pode ser "ha" para hectares ou "m2" para metros quadrados. Padrao e "ha".
#' @param option Opcao de arredondamento para o numero de arvores. Pode ser "inferior" para arredondar para baixo, "superior" para arredondar para cima ou "exato" para manter o valor exato. Padrao e "inferior".
#'
#' @return Retorna uma lista com informacoes sobre o ponto central, raio, arvores dentro do circulo, contagem por linha e esquema de caminhamento.
#'
#' @examples
#' # Exemplo 1: Executar o processo com espacamento 2m x 2m em 1 ha, arredondando para baixo
#' circ_plot_metrics(spacing_x = 2, spacing_y = 2, area = 1, units = "ha", option = "inferior")
#'
#' # Exemplo 2: Executar o processo com espacamento 3m x 3m em 5000 m2, arredondando para cima
#' circ_plot_metrics(spacing_x = 3, spacing_y = 3, area = 5000, units = "m2", option = "superior")
#'
#' # Exemplo 3: Executar o processo a partir dos lados da parcela (100m x 50m) com espacamento 2m, arredondando para exato
#' circ_plot_metrics(spacing_x = 2, spacing_y = 2, side_x = 100, side_y = 50, units = "m2", option = "exato")
#'
#' # Exemplo 4: Executar o processo fornecendo apenas o espacamento na direcao X (assume spacing_y = spacing_x)
#' circ_plot_metrics(spacing_x = 2.5, area = 2, units = "ha", option = "inferior")
#'
#' @export

circ_plot_metrics <- function(spacing_x = NULL, spacing_y = NULL, n = NULL,
                              methodology = "central",
                              area = NULL, side_x = NULL, side_y = NULL,
                              units = "ha",
                              option = "inferior") {

  # Definir opcoes permitidas para metodologia
  methodology <- match.arg(methodology, choices = c('central', 'vertical', 'horizontal', 'quadrante'))

  # Definir opcoes permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opcao escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o numero de arvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando numero de arvores fornecido: ", desired_trees)
  } else {
    # Calcular o numero de arvores usando a funcao n_trees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y, area = area,
                             side_x = side_x, side_y = side_y,
                             units = units,
                             option = option)
    message("Numero de arvores calculado: ", desired_trees)
  }

  if (is.null(spacing_y)){spacing_y <- spacing_x}

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de arvores
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessario
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  # Filtrar as arvores dentro do circulo
  trees_circle <- circ_filter(trees, central_point, radius)

  # Contar o numero de arvores por linha
  line_counter <- circ_count(trees_circle, central_point, spacing_y)

  # Gerar esquema de caminhamento como tibble
  walk_scheme <- circ_walk_scheme(line_counter, central_point, spacing_x)

  # Plotar os resultados
  plot_title <- paste("Metodologia", methodology)
  circ_aux_plot(trees, trees_circle, central_point, radius, plot_title)

  # Exibir informacoes
  cat("\n---", plot_title, "---\n")
  cat("Ponto central:", paste0("(", central_point$x, ", ", central_point$y, ")"), "\n")
  cat("Raio da parcela circular:", round(radius, 2), "metros\n")
  cat("Numero de arvores na parcela circular:", nrow(trees_circle), "\n")
  cat("Esquema de caminhamento:\n")
  print(walk_scheme)

  # Retornar uma lista com todas as informacoes
  return(list(
    central_point = central_point,
    radius = radius,
    walk_scheme = walk_scheme
  ))
}
