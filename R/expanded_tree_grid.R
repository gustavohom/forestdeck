#' Gerar Grade de Arvores
#'
#' Esta funcao gera uma grade de coordenadas de arvores com base nos limites e espacamento fornecidos.
#'
#' @param limit_x Limite na direcao X (em metros).
#' @param limit_y Limite na direcao Y (em metros).
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros).
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#'
#' @return Retorna um dataframe com as coordenadas x e y das arvores na grade.
#'
#' @examples
#' # Exemplo 1: Gerar uma grade de arvores com limite de 10m e espacamento de 2m
#' expanded_tree_grid(limit_x = 10, limit_y = 10, spacing_x = 2, spacing_y = 2)
#'
#' @export
expanded_tree_grid <- function(limit_x, limit_y, spacing_x, spacing_y) {
  x_coords <- seq(-limit_x, limit_x, by = spacing_x)
  y_coords <- seq(-limit_y, limit_y, by = spacing_y)
  grid <- expand.grid(x = x_coords, y = y_coords)
  return(grid)
}
