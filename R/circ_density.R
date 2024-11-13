#' Analise de Densidade de arvores em Funcao do Raio e Metodologia
#'
#' Esta funcao calcula o numero de arvores que cabem em circulos de diferentes raios,
#' utilizando quatro metodologias diferentes. Em seguida, extrapola o numero de arvores
#' para 1 hectare e calcula o erro percentual em relacao ao valor observado.
#'
#' @param radius_values Vetor de raios (em metros) para os quais calcular o numero de arvores.
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros).
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#' @param methodologies Vetor com as metodologias a serem utilizadas. Opcoes: "central", "vertical", "horizontal", "quadrante".
#' @param observed_tph Numero observado de arvores por hectare (calculado com base no espacamento real).
#'
#' @return Retorna um data frame com o raio, metodologia, numero de arvores no circulo,
#'         area do circulo, densidade estimada (arvores/ha) e erro percentual.
#'
#' @examples
#'
#' # Exemplo
#'
#' radius_values <- seq(1, 30, by = 0.1)
#' results <- circ_density(
#'   radius_values = radius_values,
#'   spacing_x = 3,
#'   spacing_y = 3,
#'   methodologies = c("central", "vertical", "horizontal", "quadrante")
#' )
#'
#' # Visualizar os primeiros resultados
#'
#' head(results)
#'
#' @export
circ_density <- function(radius_values,
                                  spacing_x,
                                  spacing_y,
                                  methodologies = c("central", "vertical", "horizontal", "quadrante"),
                                  observed_tph = NULL) {
  # Validar metodologias
  methodologies <- match.arg(methodologies, choices = c("central", "vertical", "horizontal", "quadrante"), several.ok = TRUE)

  # Calcular o numero observado de arvores por hectare, se nao fornecido
  if (is.null(observed_tph)) {
    area_per_tree <- spacing_x * spacing_y  # area ocupada por cada arvore
    observed_tph <- 10000 / area_per_tree   # Numero de arvores por hectare
  }

  # Criar data frame para armazenar os resultados
  results <- data.frame()

  for (method in methodologies) {
    for (radius in radius_values) {
      # Gerar grade de arvores dentro do limite necessario
      limit_x <- ceiling(radius / spacing_x) * spacing_x
      limit_y <- ceiling(radius / spacing_y) * spacing_y
      trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

      # Obter o ponto central de acordo com a metodologia
      central_point <- circ_central_point(method, spacing_x, spacing_y)

      # Filtrar as arvores dentro do circulo
      trees_in_circle <- circ_filter(trees, central_point, radius)

      # Calcular o numero de arvores no circulo
      num_trees <- nrow(trees_in_circle)

      # Calcular a area do circulo
      circle_area <- pi * radius^2  # area em metros quadrados

      # Extrapolar para arvores por hectare (10.000 m²)
      estimated_tph <- (num_trees / circle_area) * 10000  # arvores por hectare

      # Calcular o erro percentual
      error_percent <- ((observed_tph - estimated_tph) / observed_tph) * 100

      # Adicionar os resultados ao data frame
      results <- rbind(results, data.frame(
        Radius = radius,
        Methodology = method,
        Num_Trees = num_trees,
        Circle_Area_m2 = circle_area,
        Estimated_TPH = estimated_tph,
        Observed_TPH = observed_tph,
        Error_Percent = error_percent
      ))
    }
  }

  return(results)
}
