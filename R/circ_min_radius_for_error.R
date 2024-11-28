#' Encontrar o Menor Raio onde o Erro Permanece dentro do Intervalo Especificado
#'
#' Esta função recebe os resultados da função `circ_density` e um vetor de valores de erro (error bands).
#' Para cada metodologia e cada valor de erro especificado, a função identifica o menor raio
#' após o último raio onde o erro percentual ultrapassa o valor do erro especificado (positivo ou negativo).
#'
#' @param results Data frame contendo os resultados da função `circ_density`.
#' @param error_bands Vetor numérico com os valores de erro para os quais deseja encontrar os raios mínimos.
#'
#' @return Data frame com as colunas: `Methodology`, `Error_Band`, `Min_Radius`.
#'
#' @examples
#'
#' # Define parameters
#' radius_values <- seq(5, 30, by = 0.1)
#' spacing_x <- 3  # Spacing between trees in the X direction (in meters)
#' spacing_y <- 2  # Spacing between trees in the Y direction (in meters)
#'
#' # Run analysis
#' results <- circ_density(
#'   radius_values = radius_values,
#'   spacing_x = spacing_x,
#'   spacing_y = spacing_y,
#'   methodologies = c("central", "vertical", "horizontal", "quadrant")
#' )
#' error_bands <- c(2.5, 5, 10)
#' min_radius_df <- circ_min_radius_for_error(results, error_bands)
#' print(min_radius_df)
#'
#' @export
circ_min_radius_for_error <- function(results, error_bands) {
  # Verificar se a coluna 'Methodology' existe
  if (!"Methodology" %in% names(results)) {
    stop("O data frame 'results' deve conter a coluna 'Methodology'.")
  }

  # Inicializar uma lista para armazenar os resultados
  output_list <- list()

  # Obter as metodologias únicas
  methodologies <- unique(results$Methodology)

  # Para cada metodologia
  for (method in methodologies) {
    # Filtrar os resultados para a metodologia atual
    method_results <- results[results$Methodology == method, ]

    # Ordenar os resultados por raio crescente
    method_results <- method_results[order(method_results$Radius), ]

    # Para cada valor de erro especificado
    for (error_band in error_bands) {
      # Encontrar os índices onde o erro ultrapassa o intervalo especificado
      outside_indices <- which(abs(method_results$Error_Percent) > error_band)

      if (length(outside_indices) == 0) {
        # Se nunca ultrapassa o intervalo, o raio mínimo é o menor raio disponível
        min_radius <- min(method_results$Radius)
      } else if (max(outside_indices) == nrow(method_results)) {
        # Se o erro está sempre fora do intervalo, não há raio que atenda ao critério
        min_radius <- NA
      } else {
        # O raio mínimo é o próximo raio após o último índice onde o erro ultrapassa o intervalo
        min_radius <- method_results$Radius[max(outside_indices) + 1]
      }

      # Armazenar os resultados
      output_list <- append(output_list, list(data.frame(
        Methodology = method,
        Error_Band = error_band,
        Min_Radius = min_radius
      )))
    }
  }

  # Combinar os resultados em um único data frame
  output_df <- do.call(rbind, output_list)

  # Ordenar o data frame
  output_df <- output_df[order(output_df$Methodology, output_df$Error_Band), ]

  return(output_df)
}
