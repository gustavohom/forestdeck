% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circ_plot_density_error.R
\name{circ_plot_density_error}
\alias{circ_plot_density_error}
\title{Plot Percent Error vs. Radius with Error Envelopes for Different Methodologies}
\usage{
circ_plot_density_error(
  results,
  methodology = NULL,
  colors = NULL,
  error_bands = NULL,
  error_band_colors = c("#B3B3B3", "#D9D9D9", "#F0F0F0"),
  y_limits = NULL,
  title = TRUE
)
}
\arguments{
\item{results}{Data frame containing the results from the \code{circ_density_analysis} function.}

\item{methodology}{Methodology to plot. Can be "central", "vertical",
"horizontal", "quadrant", or \code{NULL}. If \code{NULL}, all methodologies
will be plotted on a single graph.}

\item{colors}{Named vector of custom colors for each methodology (optional).
Example: \code{colors = c("central" = "#5DA5DA", "vertical" = "#FAA43A")}.}

\item{error_bands}{Vector of percent values for the error envelopes (optional). If provided,
envelopes of ± each value in \code{error_bands}\% will be added to the plot.}

\item{error_band_colors}{Vector of colors for the error bands (optional). Should have the same length as \code{error_bands}.}

\item{y_limits}{Numeric vector of length 2 to specify y-axis limits (optional). Example: \code{y_limits = c(-50, 50)}.}

\item{title}{Title of the plot. If \code{TRUE}, the default title is used. If \code{FALSE} or \code{NULL}, no title is displayed.
If a character string is provided, it is used as the custom title. Default is \code{TRUE}.}
}
\value{
Generates plots of Percent Error vs. Radius with error envelopes.
}
\description{
This function generates plots of Percent Error as a function of Radius,
based on the results from the \code{circ_density_analysis} function.
It can include multiple error envelopes (acceptable error ranges) as shaded areas.
}
\examples{

# Define parameters
radius_values <- seq(5, 30, by = 0.1)
spacing_x <- 3  # Spacing between trees in the X direction (in meters)
spacing_y <- 2  # Spacing between trees in the Y direction (in meters)

# Run analysis
results <- circ_density(
  radius_values = radius_values,
  spacing_x = spacing_x,
  spacing_y = spacing_y,
  methodologies = c("central", "vertical", "horizontal", "quadrant")
)

# Plot all methodologies with multiple error envelopes
circ_plot_density_error(
  results,
  colors = c(
    "central" = "#e41a1c",
    "vertical" = "#377eb8",
    "horizontal" = "black",
    "quadrante" = "#4daf4a"
  ),
  error_bands = c(2.5, 5, 10),
  error_band_colors = c("#D9D9D9", "#B3B3B3", "#7c7879"),
  y_limits = c(-46, 46),
  title = TRUE  # Use default title
)

# Plot without a title
circ_plot_density_error(
  results,
  methodology = 'central',
  colors = c("central" = "black"),
  error_bands = c(2.5, 5, 10),
  error_band_colors = c("#D9D9D9", "#B3B3B3", "#7c7879"),
  y_limits = c(-50, 50),
  title = FALSE  # No title
)

# Plot with a custom title
circ_plot_density_error(
  results,
  title = "Custom Plot Title"
)

}
