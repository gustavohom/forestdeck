#' Calcular a Distancia entre Dois Pontos
#'
#' @param x1 Coordenada x do primeiro ponto.
#' @param y1 Coordenada y do primeiro ponto.
#' @param x2 Coordenada x do segundo ponto.
#' @param y2 Coordenada y do segundo ponto.
#' @return Distancia Euclidiana entre os pontos.

calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Criar Coordenadas de um Circulo
#'
#' @param x0 Coordenada x do centro.
#' @param y0 Coordenada y do centro.
#' @param r Raio do circulo.
#' @param npoints Numero de pontos para desenhar o circulo.
#' @return Data frame com as coordenadas x e y do circulo.


circle_coordinates <- function(x0, y0, r, npoints = 100) {
  angles <- seq(0, 2 * pi, length.out = npoints)
  data.frame(
    x = x0 + r * cos(angles),
    y = y0 + r * sin(angles)
  )
}

#' Criar Coordenadas de um Semicirculo com Retangulo
#'
#' @param x0 Coordenada x do centro.
#' @param y0 Coordenada y do centro.
#' @param r Raio do semicirculo.
#' @param rect_height Altura do retangulo.
#' @param npoints Numero de pontos para desenhar o semicirculo.
#' @return Data frame com as coordenadas x e y do semicirculo com retangulo.


semi_rect_coordinates <- function(x0, y0, r, rect_height, npoints = 100) {
  # Semicirculo superior
  theta_top <- seq(0, pi, length.out = npoints)
  semicirc_top_x <- r * cos(theta_top) + x0
  semicirc_top_y <- r * sin(theta_top) + y0 + rect_height

  # Semicirculo inferior
  theta_bottom <- seq(pi, 2 * pi, length.out = npoints)
  semicirc_bottom_x <- r * cos(theta_bottom) + x0
  semicirc_bottom_y <- r * sin(theta_bottom) + y0

  # Retangulo
  rect_x <- c(x0 - r, x0 + r, x0 + r, x0 - r, x0 - r)
  rect_y <- c(y0, y0, y0 + rect_height, y0 + rect_height, y0)

  data.frame(
    x = c(semicirc_top_x, rect_x, semicirc_bottom_x),
    y = c(semicirc_top_y, rect_y, semicirc_bottom_y)
  )
}
