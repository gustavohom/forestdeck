#' Plotar Erro Percentual vs. Raio com Envelope de Erro para Diferentes Metodologias
#'
#' Esta funcao gera graficos do Erro Percentual em funcao do Raio,
#' com base nos resultados da funcao `circ_density_analysis`.
#' Pode incluir um envelope de erro (faixa de erro aceitavel) como uma area sombreada.
#'
#' @param results Data frame contendo os resultados da funcao `circ_density_analysis`.
#' @param methodology Metodologia a ser plotada. Pode ser "central", "vertical",
#'        "horizontal", "quadrante" ou `NULL`. Se for `NULL`, todas as metodologias
#'        serao plotadas em um unico grafico.
#' @param colors Vetor nomeado de cores personalizadas para cada metodologia (opcional).
#'        Exemplo: `colors = c("central" = "#5DA5DA", "vertical" = "#FAA43A")`.
#' @param error_band Valor percentual para o envelope de erro (opcional). Se fornecido,
#'        um envelope de ±`error_band`% sera adicionado ao grafico.
#'
#' @return Gera graficos do Erro Percentual vs. Raio com o envelope de erro.
#'
#' @examples
#' # Essa funcao necessita executar anteriormente  `circ_density_analysis`
#'
#' # Preparando os dados
#'
#' radius_values <- seq(1, 30, by = 0.1)
#'
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
#' # Para plotar o Erro Percentual vs. Raio para todas as metodologias:
#'
#' circ_plot_density_error(results)
#'
#' # Para plotar todas as metodologias com cores personalizadas:
#'
#' circ_plot_density_error(
#'   results,
#'   colors = c("central" = "#F17CB0", "vertical" = "#60BD68",
#'              "horizontal" = "#FAA43A", "quadrante" = "#5DA5DA")
#' )
#'
#' # Para plotar apenas a metodologia "central":
#'
#' circ_plot_density_error(results, methodology = "central")
#'
#' #Para plotar o Erro Percentual vs. Raio para todas as metodologias com envelope de ±2,5%:
#'
#' circ_plot_density_error(
#'   results,
#'   colors = c(
#'     "central" = "#5DA5DA",
#'     "vertical" = "#FAA43A",
#'     "horizontal" = "#60BD68",
#'     "quadrante" = "#F17CB0"
#'   ),
#'   error_band = 2.5
#' )
#'
#' # Para plotar apenas a metodologia "central" com envelope de ±2,5%:
#'
#' circ_plot_density_error(
#'   results,
#'   methodology = "central",
#'   colors = c("central" = "#5DA5DA"),
#'   error_band = 2.5
#' )
#'
#'
#' # Outros exemplos
#'
#' circ_plot_density_error(results, methodology = "central", colors = c(central = '#F17CB0'))
#' circ_plot_density_error(results, methodology = "vertical", colors = c(vertical = '#60BD68'))
#' circ_plot_density_error(results, methodology = "horizontal")
#' circ_plot_density_error(results, methodology = "quadrante")
#'
#' @export
#'
#' @import ggplot2
#'
circ_plot_density_error <- function(results, methodology = NULL, colors = NULL) {
  library(ggplot2)

  # Verificar se o parametro methodology e valido
  available_methods <- unique(results$Methodology)

  if (!is.null(methodology)) {
    if (!methodology %in% available_methods) {
      stop("A metodologia especificada nao esta presente nos resultados fornecidos.")
    }
    # Filtrar os resultados para a metodologia especificada
    results_to_plot <- results[results$Methodology == methodology, ]
    # Definir a cor para a metodologia especificada
    line_color <- if (!is.null(colors) && methodology %in% names(colors)) {
      colors[[methodology]]
    } else {
      "blue"  # Cor padrao
    }
    # Gerar o grafico
    ggplot(results_to_plot, aes(x = Radius, y = Error_Percent)) +
      geom_line(color = line_color, size = 1) +
      labs(
        title = paste("Erro Percentual vs. Raio - Metodologia:", methodology),
        x = "Raio (m)",
        y = "Erro Percentual (%)"
      ) +
      theme_minimal()
  } else {
    # Plotar todas as metodologias em um unico grafico
    p <- ggplot(results, aes(x = Radius, y = Error_Percent, color = Methodology)) +
      geom_line(size = 1) +
      labs(
        title = "Erro Percentual vs. Raio para Todas as Metodologias",
        x = "Raio (m)",
        y = "Erro Percentual (%)",
        color = "Metodologia"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")

    # Adicionar cores personalizadas se fornecidas
    if (!is.null(colors)) {
      p <- p + scale_color_manual(values = colors)
    }

    print(p)
  }
}
