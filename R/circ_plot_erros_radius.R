#' Plotar Erro Amostral e Raio em Função do Tamanho da Amostra
#'
#' Esta função gera gráficos do erro amostral e do raio da parcela circular em função do tamanho da amostra.
#'
#' @param results Data.frame resultante da função `circ_erros_radius`.
#'
#' @return Gera dois gráficos: Erro vs. Tamanho da Amostra e Raio vs. Tamanho da Amostra.
#'
#' @examples
#'
#' \dontrun{
#'
#' # Preparando os dados
#'
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
#'  Apos executar a função `circ_erros_radius` e obtido `results`
#'
#' circ_plot_erros_radius(results)
#'
#'}
#'
#' @export
circ_plot_erros_radius <- function(results) {
  # Plotar erro amostral vs. tamanho da amostra
  plot(results$Sample_Size, results$Error, type = "b",
       xlab = "Tamanho da Amostra (n)",
       ylab = "Erro Amostral (E)",
       main = "Erro Amostral vs. Tamanho da Amostra",
       col = "blue", pch = 19)

  # Plotar raio vs. tamanho da amostra
  plot(results$Sample_Size, results$Radius, type = "b",
       xlab = "Tamanho da Amostra (n)",
       ylab = "Raio da Parcela Circular (m)",
       main = "Raio da Parcela vs. Tamanho da Amostra",
       col = "blue", pch = 19)

  # Plotar erro amostral vs. tamanho da amostra no eixo Y primário
  plot(results$Sample_Size, results$Error, type = "b",
       xlab = "Tamanho da Amostra (n)",
       ylab = "Erro Amostral (E)",
       main = "Erro Amostral e Raio vs. Tamanho da Amostra",
       col = "blue", pch = 19, lty = 1)

  # Adicionar o raio da parcela circular no eixo Y secundário
  par(new = TRUE)

  plot(results$Sample_Size, results$Radius, type = "b",
       axes = FALSE, xlab = "", ylab = "",
       col = "red", pch = 17, lty = 2)
  axis(side = 4, at = pretty(range(results$Radius)))
  mtext("Raio da Parcela Circular (m)", side = 4, line = 3)

  # Adicionar legenda
  legend("topright", legend = c("Erro Amostral", "Raio da Parcela"),
         col = c("blue", "red"), pch = c(19, 17), lty = c(1, 2))
}
