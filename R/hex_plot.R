#' Plotar o Arranjo Hexagonal das Formas
#'
#' Esta funcao plota o arranjo hexagonal das formas geometricas (circulos ou obrounds) dentro de uma area
#' definida. Utiliza o metodo de empacotamento hexagonal para organizar as formas de maneira eficiente,
#' maximizando a densidade de empacotamento e minimizando o desperdicio de espaco.
#'
#' ## Empacotamento Hexagonal para Circulos
#'
#' No empacotamento hexagonal de circulos, cada circulo e cercado por seis outros circulos, formando
#' uma estrutura semelhante a uma colmeia. Este arranjo resulta em uma densidade de empacotamento
#' maxima de aproximadamente 90,7%, otimizando o uso do espaco disponivel.
#'
#' ## Empacotamento Hexagonal Adaptado para Obrounds
#'
#' Quando a forma especificada e \code{"semi_rect"}, a funcao considera uma geometria composta por
#' dois semicirculos unidos por um retangulo, formando uma **obround** (tambem conhecida como
#' **estadio** ou **discoide** em portugues). Este arranjo combina a eficiencia do empacotamento
#' de semicirculos com a versatilidade do retangulo, resultando em uma forma que pode ser utilizada
#' em diversas aplicacões geometricas.
#'
#' @param area_size Tamanho da area disponivel para empacotamento (por exemplo, em unidades).
#' @param diameter Diametro das figuras geometricas (circulos ou semicirculos). Este parametro e essencial para
#'   determinar o tamanho das formas a serem empacotadas.
#' @param shape Forma das figuras a serem empacotadas. Pode ser "circle"
#'   para empacotamento de circulos ou "semi_rect" para empacotamento de semicirculos unidos por retangulo.
#' @param rect_height Altura do retangulo que une os semicirculos. Este parametro e necessario apenas quando shape e "semi_rect".
#'
#'
#' @return Retorna um objeto ggplot representando o arranjo hexagonal das formas dentro da area definida.
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo 1: Plotar o arranjo hexagonal de circulos com diametro 10 em uma area de 100 unidades
#' plot_circles <- hex_plot(area_size = 100, diameter = 10, shape = "circle")
#' print(plot_circles)
#'
#' # Exemplo 2: Plotar o arranjo hexagonal adaptado para obrounds com diametro 6 e altura do retangulo 2 em uma area de 100000 unidades
#' plot_obrounds <- hex_plot(area_size = 100000, diameter = 6, shape = "semi_rect", rect_height = 2)
#' print(plot_obrounds)
#'
#' }
#'
#' @import ggplot2
#'
#' @export
hex_plot <- function(area_size, diameter, shape = "circle", rect_height = NULL) {
  r <- diameter / 2

  # Determinar a distancia vertical entre as fileiras com base na forma
  Y <- if (shape == "circle") {
    hex_height_circle(diameter)
  } else if (shape == "semi_rect") {
    hex_height_semi_rect(diameter, rect_height)
  } else {
    stop("Forma nao reconhecida. Use 'circle' ou 'semi_rect'.")
  }

  # Calcular quantas formas cabem na area
  shape_counts <- hex_efficiency(area_size, diameter, shape, rect_height)
  total_shapes <- shape_counts$total_shapes
  num_rows <- shape_counts$num_rows
  shapes_per_row_odd <- shape_counts$shapes_per_row_odd
  shapes_per_row_even <- shape_counts$shapes_per_row_even
  odd_rows <- shape_counts$odd_rows
  even_rows <- shape_counts$even_rows

  # Lista para armazenar os dados de cada forma
  all_shapes <- data.frame()

  for (row in 1:num_rows) {
    # Determinar a coordenada Y para a fileira atual
    if (shape == "semi_rect") {
      if (row == 1) {
        y_coord <- r  # A primeira fileira comeca no nivel r (metade do diametro)
      } else {
        y_coord <- (row - 1) * Y + r
      }
    } else {
      y_coord <- (row - 1) * Y + r
    }

    # Determinar as coordenadas X para a fileira atual
    if (shape == "semi_rect") {
      if (row %% 2 == 1) {  # Fileira impar
        x_coords <- seq(r, r + (shapes_per_row_odd - 1) * diameter, by = diameter)
      } else {  # Fileira par
        x_coords <- seq(diameter, diameter + (shapes_per_row_even - 1) * diameter, by = diameter)
      }
    } else {
      if (row %% 2 == 1) {  # Fileira impar
        x_coords <- seq(r, area_size - r, by = diameter)
      } else {  # Fileira par
        x_coords <- seq(r + (diameter / 2), area_size - r - (diameter / 2), by = diameter)
      }
    }

    for (x in x_coords) {
      if (shape == "circle") {
        # Gerar as coordenadas do circulo
        shape_data <- hex_circle_coordinates(x, y_coord, r)
      } else if (shape == "semi_rect") {
        # Gerar as coordenadas do semicirculo com retangulo
        shape_data <- hex_semi_rect_coordinates(x, y_coord, r, rect_height)
      }
      shape_data$group <- paste0(row, "-", x)  # Grupo para cada forma
      all_shapes <- rbind(all_shapes, shape_data)
    }
  }

  # Plotar as formas
  p <- ggplot() +
    geom_polygon(data = all_shapes, aes(x = x, y = y, group = group), fill = "lightblue", color = "blue") +
    coord_fixed() +
    expand_limits(x = c(0, area_size), y = c(0, area_size)) +  # Definir os limites dos eixos
    labs(x = "X", y = "Y") +
    theme_minimal()

  # Ajustar o titulo do grafico
  if (shape == "circle") {
    p <- p + ggtitle(paste("Arranjo Hexagonal de Circulos com Diametro de", diameter, "unidades"))
  } else if (shape == "semi_rect") {
    p <- p + ggtitle(paste("Arranjo Hexagonal de Obrounds (Diametro =", diameter, "unidades, Altura do Retangulo =", rect_height, "unidades)"))
  }

  return(p)
}
