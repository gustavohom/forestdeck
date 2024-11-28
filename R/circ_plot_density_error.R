#' Plot Percent Error vs. Radius with Error Envelopes for Different Methodologies
#'
#' This function generates plots of Percent Error as a function of Radius,
#' based on the results from the `circ_density_analysis` function.
#' It can include multiple error envelopes (acceptable error ranges) as shaded areas.
#'
#' @param results Data frame containing the results from the `circ_density_analysis` function.
#' @param methodology Methodology to plot. Can be "central", "vertical",
#'        "horizontal", "quadrant", or `NULL`. If `NULL`, all methodologies
#'        will be plotted on a single graph.
#' @param colors Named vector of custom colors for each methodology (optional).
#'        Example: `colors = c("central" = "#5DA5DA", "vertical" = "#FAA43A")`.
#' @param error_bands Vector of percent values for the error envelopes (optional). If provided,
#'        envelopes of ± each value in `error_bands`% will be added to the plot.
#' @param error_band_colors Vector of colors for the error bands (optional). Should have the same length as `error_bands`.
#' @param y_limits Numeric vector of length 2 to specify y-axis limits (optional). Example: `y_limits = c(-50, 50)`.
#'
#' @return Generates plots of Percent Error vs. Radius with error envelopes.
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
#'
#' # Plot all methodologies with multiple error envelopes
#' circ_plot_density_error(
#'   results,
#'   colors = c(
#'     "central" = "#e41a1c",
#'     "vertical" = "#377eb8",
#'     "horizontal" = "black",
#'     "quadrant" = "#4daf4a"
#'   ),
#'   error_bands = c(2.5, 5, 10),
#'   error_band_colors = c("#D9D9D9", "#B3B3B3", "#7c7879"),
#'   y_limits = c(-46, 46)
#' )
#'
#' # Calculate minimum radius for each error band
#' error_bands <- c(2.5, 5, 10)
#' min_radius_df <- circ_min_radius_for_error(results, error_bands)
#' print(min_radius_df)
#'
#' # Plot a single methodology ("central") with multiple error envelopes
#' circ_plot_density_error(
#'   results,
#'   methodology = 'central',
#'   colors = c("central" = "black"),
#'   error_bands = c(2.5, 5, 10),
#'   error_band_colors = c("#D9D9D9", "#B3B3B3", "#7c7879"),
#'   y_limits = c(-50, 50)
#' )
#'
#' @export
#'
#' @import ggplot2
#'

circ_plot_density_error <- function(results, methodology = NULL, colors = NULL, error_bands = NULL, error_band_colors = c("#F0F0F0", "#D9D9D9", "#B3B3B3"), y_limits = NULL) {
  # Carregar o ggplot2
  library(ggplot2)

  # Verificar se o parâmetro methodology é válido
  available_methods <- unique(results$Methodology)

  if (!is.null(methodology)) {
    if (!methodology %in% available_methods) {
      stop("A metodologia especificada não está presente nos resultados fornecidos.")
    }
    # Filtrar os resultados para a metodologia especificada
    results_to_plot <- results[results$Methodology == methodology, ]
    # Definir a cor para a metodologia especificada
    line_color <- if (!is.null(colors) && methodology %in% names(colors)) {
      colors[[methodology]]
    } else {
      "black"  # Cor padrão
    }
    # Gerar o gráfico
    p <- ggplot(results_to_plot, aes(x = Radius, y = Error_Percent)) +
      labs(
        title = paste("Erro Percentual vs. Raio - Metodologia:", methodology),
        x = "Raio (m)",
        y = "Erro Percentual (%)"
      ) +
      theme_minimal()

    # Adicionar os envelopes de erro, se especificados
    if (!is.null(error_bands)) {
      # Ordenar os error_bands em ordem crescente
      sorted_indices <- order(error_bands, decreasing = FALSE)
      error_bands <- error_bands[sorted_indices]

      # Verificar se o número de cores corresponde ao número de bandas de erro
      if (length(error_band_colors) != length(error_bands)) {
        stop("O número de cores em 'error_band_colors' deve corresponder ao número de 'error_bands'.")
      }
      error_band_colors <- error_band_colors[sorted_indices]

      # Plotar os envelopes, do maior para o menor, para que o menor fique na frente
      for (i in seq_along(error_bands)) {
        band <- error_bands[i]
        fill_color <- error_band_colors[i]
        # Criar data frame para o envelope
        band_df <- data.frame(
          Radius = results_to_plot$Radius,
          ymin = rep(-band, nrow(results_to_plot)),
          ymax = rep(band, nrow(results_to_plot))
        )
        p <- p + geom_ribbon(
          data = band_df,
          aes(x = Radius, ymin = ymin, ymax = ymax),
          fill = fill_color,
          alpha = 0.5,
          inherit.aes = FALSE  # Impede a herança dos estéticos globais
        )
      }
    }

    # Adicionar a linha da metodologia
    p <- p + geom_line(color = line_color, size = 0.1)

    # Definir os limites do eixo y, se especificados
    if (!is.null(y_limits)) {
      p <- p + ylim(y_limits)
    }

    print(p)
  } else {
    # Plotar todas as metodologias em um único gráfico
    p <- ggplot(results, aes(x = Radius, y = Error_Percent, color = Methodology)) +
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

    # Adicionar os envelopes de erro, se especificados
    if (!is.null(error_bands)) {
      # Ordenar os error_bands em ordem crescente
      sorted_indices <- order(error_bands, decreasing = FALSE)
      error_bands <- error_bands[sorted_indices]

      # Verificar se o número de cores corresponde ao número de bandas de erro
      if (length(error_band_colors) != length(error_bands)) {
        stop("O número de cores em 'error_band_colors' deve corresponder ao número de 'error_bands'.")
      }
      error_band_colors <- error_band_colors[sorted_indices]

      # Plotar os envelopes, do maior para o menor, para que o menor fique na frente
      for (i in seq_along(error_bands)) {
        band <- error_bands[i]
        fill_color <- error_band_colors[i]
        # Criar data frame para o envelope
        band_df <- data.frame(
          Radius = results$Radius,
          ymin = rep(-band, nrow(results)),
          ymax = rep(band, nrow(results))
        )
        p <- p + geom_ribbon(
          data = band_df,
          aes(x = Radius, ymin = ymin, ymax = ymax),
          fill = fill_color,
          alpha = 0.5,
          inherit.aes = FALSE  # Impede a herança dos estéticos globais
        )
      }
    }

    # Adicionar as linhas das metodologias
    p <- p + geom_line(aes(color = Methodology), size = 0.1)

    # Definir os limites do eixo y, se especificados
    if (!is.null(y_limits)) {
      p <- p + ylim(y_limits)
    }

    print(p)
  }
}
