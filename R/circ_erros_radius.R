#' Calcular Erros e Raios para Diferentes Tamanhos de Amostra
#'
#' Esta função calcula o erro amostral e o raio necessário para diferentes tamanhos de amostra,
#' permitindo analisar como o erro diminui conforme o tamanho da amostra aumenta.
#'
#' @param n_values Vetor de tamanhos de amostra (n) para os quais calcular os erros e raios.
#' @param N Tamanho total da população (número total de árvores). Padrão é 1666.
#' @param spacing_x Espaçamento entre as árvores na direção X (em metros).
#' @param spacing_y Espaçamento entre as árvores na direção Y (em metros).
#' @param Z Valor Z correspondente ao nível de confiança desejado (padrão é 1,96 para 95% de confiança).
#' @param p Proporção esperada (use 0,5 para máxima variabilidade se a proporção for desconhecida).
#' @param methodology Metodologia para determinar o ponto central (padrão é "central").
#' @param units Unidade de medida da área ("ha" ou "m2"). Padrão é "ha".
#' @param option Opção de arredondamento para o número de árvores ("inferior", "superior" ou "exato"). Padrão é "inferior".
#'
#' @return Retorna um data.frame com os tamanhos de amostra, erros amostrais e raios correspondentes.
#'
#' @examples
#' # Exemplo de uso
#' n_values <- seq(10, 500, by = 10)
#' results <- circ_errors_radius(
#'   n_values = n_values,
#'   N = 1666,
#'   spacing_x = 2,
#'   spacing_y = 2,
#'   Z = 1.96,
#'   p = 0.5,
#'   methodology = "central"
#' )
#'
#' @export
circ_errors_radius <- function(n_values,
                                       N = 1666,
                                       spacing_x,
                                       spacing_y,
                                       Z = 1.96,
                                       p = 0.5,
                                       methodology = "central",
                                       units = "ha",
                                       option = "inferior") {
  errors <- numeric(length(n_values))
  radii <- numeric(length(n_values))

  for (i in seq_along(n_values)) {
    n <- n_values[i]

    # Calcular erro amostral para proporção (população finita)
    E <- Z * sqrt((p * (1 - p)) / n * ((N - n) / (N - 1)))
    errors[i] <- E

    # Calcular raio usando a função circ_radius
    radius <- circ_radius(n = n, spacing_x = spacing_x, spacing_y = spacing_y,
                          methodology = methodology, units = units, option = option)
    radii[i] <- radius
  }

  results <- data.frame(
    Sample_Size = n_values,
    Error = errors,
    Radius = radii
  )

  return(results)
}
