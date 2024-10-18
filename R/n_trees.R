#' Calcular o Numero de arvores por Hectare ou Metro Quadrado
#'
#' Esta funcao calcula o numero de arvores por hectare ou por metro quadrado com base no espacamento entre as arvores e na area da parcela. Permite especificar opcoes de arredondamento para o numero de arvores resultante.
#'
#' ## Detalhes
#'
#' A funcao pode determinar a area da parcela de varias maneiras:
#' \itemize{
#'   \item Utilizando uma area fornecida diretamente em hectares ou metros quadrados.
#'   \item Calculando a area a partir dos lados `side_x` e `side_y`.
#'   \item Assumindo que `side_x` e igual a `side_y` caso apenas um dos lados seja fornecido.
#' }
#'
#' O espacamento entre as arvores pode ser fornecido em ambas as direcoes `spacing_x` e `spacing_y`. Se apenas um dos espacamentos for fornecido, o outro sera assumido como igual.
#'
#' A opcao de arredondamento (`option`) permite escolher entre arredondar o numero de arvores para baixo (`inferior`), para cima (`superior`) ou manter o valor exato (`exato`).
#'
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros). Opcional se `spacing_y` for fornecido.
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros). Opcional se `spacing_x` for fornecido.
#' @param area area da parcela. Pode ser fornecida em hectares (`units = "ha"`) ou metros quadrados (`units = "m2"`). Opcional se `side_x` e `side_y` forem fornecidos.
#' @param side_x Comprimento do lado X da parcela (em metros). Opcional se `area` for fornecida.
#' @param side_y Comprimento do lado Y da parcela (em metros). Opcional se `area` for fornecida.
#' @param units Unidade de medida da area. Pode ser `"ha"` para hectares ou `"m2"` para metros quadrados. Padrao e `"ha"`.
#' @param option Opcao de arredondamento para o numero de arvores. Pode ser `"inferior"` para arredondar para baixo, `"superior"` para arredondar para cima ou `"exato"` para manter o valor exato. Padrao e `"inferior"`.
#'
#' @return Retorna um numero que representa a quantidade de arvores na parcela, de acordo com as opcoes de arredondamento especificadas.
#'
#' @examples
#' # Exemplo 1: Calcular o numero de arvores em 1 hectare com espacamento de 2m x 2m e arredondar para baixo
#' n_trees(spacing_x = 2, spacing_y = 2, area = 1, units = "ha", option = "inferior")
#'
#' # Exemplo 2: Calcular o numero de arvores em 5000 metros quadrados com espacamento de 3m x 2m e arredondar para cima
#' n_trees(spacing_x = 3, spacing_y = 2, area = 5000, units = "m2", option = "superior")
#'
#' # Exemplo 3: Calcular o numero de arvores a partir dos lados da parcela (100m x 50m) com espacamento de 2m e arredondar para exato
#' n_trees(spacing_x = 2, spacing_y = 2, side_x = 100, side_y = 50, option = "exato")
#'
#' # Exemplo 4: Calcular o numero de arvores fornecendo apenas o espacamento na direcao X (assume spacing_y = spacing_x)
#' n_trees(spacing_x = 2.5, area = 2, units = "ha", option = "inferior")
#'
#' @export

n_trees <- function(spacing_x = NULL, spacing_y = NULL, area = NULL,
                    side_x = NULL, side_y = NULL,
                    units = "ha",
                    option = "inferior") {

  # Definir opcoes permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opcao escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # 1. Determinar a area
  if (!is.null(area)) {
    if (units == "ha") {
      area_m2 <- area * 10000  # Converter hectares para metros quadrados
      message("Metodo: Usando a area fornecida em hectares.")
      message("area fornecida: ", area, " ha (", area_m2, " m²).")
    } else if (units == "m2") {
      area_m2 <- area
      message("Metodo: Usando a area fornecida em metros quadrados.")
      message("area fornecida: ", area_m2, " m².")
    }
  } else if (!is.null(side_x) && !is.null(side_y)) {
    area_m2 <- side_x * side_y
    message("Metodo: Calculando area a partir de side_x e side_y.")
    message("side_x = ", side_x, " m, side_y = ", side_y, " m.")
    message("area calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(side_x)) {
    area_m2 <- side_x^2
    message("Metodo: Calculando area a partir de side_x, assumindo side_y = side_x.")
    message("side_x = ", side_x, " m.")
    message("area calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(side_y)) {
    area_m2 <- side_y^2
    message("Metodo: Calculando area a partir de side_y, assumindo side_x = side_y.")
    message("side_y = ", side_y, " m.")
    message("area calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else {
    stop("Erro: area nao fornecida e side_x e side_y nao fornecidos.")
  }

  # 2. Determinar o espacamento entre arvores
  if (!is.null(spacing_x) && !is.null(spacing_y)) {
    message("Metodo: Usando espacamento fornecido para spacing_x e spacing_y.")
    message("Espacamento x = ", spacing_x, " m, espacamento y = ", spacing_y, " m.")
  } else if (!is.null(spacing_x)) {
    spacing_y <- spacing_x
    message("Metodo: Apenas 'spacing_x' fornecido. Definindo spacing_y = spacing_x = ", spacing_x, " m.")
  } else if (!is.null(spacing_y)) {
    spacing_x <- spacing_y
    message("Metodo: Apenas 'spacing_y' fornecido. Definindo spacing_x = spacing_y = ", spacing_y, " m.")
  } else {
    stop("Erro: Espacamento nao fornecido. Forneca 'spacing_x', 'spacing_y', ou ambos.")
  }

  # Validar que spacing_x e spacing_y sao numeros positivos
  if (!is.numeric(spacing_x) || !is.numeric(spacing_y) || spacing_x <= 0 || spacing_y <= 0) {
    stop("Erro: Os espacamentos 'spacing_x' e 'spacing_y' devem ser numeros positivos.")
  }

  # 3. Calcular o numero de arvores
  trees_exact <- area_m2 / (spacing_x * spacing_y)  # Numero exato de arvores na area

  if (option == "exato") {
    trees <- trees_exact
    message("Opcao: Exato.")
    message("Numero exato de arvores: ", trees, ".")
  } else if (option == "inferior") {
    trees <- floor(trees_exact)
    message("Opcao: Inferior (arredondando para baixo).")
    message("Numero de arvores inferior: ", trees, ".")
  } else if (option == "superior") {
    trees <- ceiling(trees_exact)
    message("Opcao: Superior (arredondando para cima).")
    message("Numero de arvores superior: ", trees, ".")
  }

  return(trees)
}

# Funcao para gerar a grade de arvores
expanded_tree_grid <- function(limit_x, limit_y, spacing_x, spacing_y) {
  x_coords <- seq(-limit_x, limit_x, by = spacing_x)
  y_coords <- seq(-limit_y, limit_y, by = spacing_y)
  grid <- expand.grid(x = x_coords, y = y_coords)
  return(grid)
}
