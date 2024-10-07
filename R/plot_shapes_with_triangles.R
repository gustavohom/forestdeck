#' Plotar Formas com Triangulos Conectando os Centros
#'
#' Esta funcao plota formas geometricas organizadas em um padrao de empacotamento hexagonal,
#' que e o metodo mais eficiente para empacotar circulos sem sobreposicao. O empacotamento
#' hexagonal maximiza o uso do espaco, reduzindo o desperdicio e aumentando a densidade de
#' empacotamento em comparacao com outros metodos, como o empacotamento quadrado.
#'
#' ## Empacotamento Hexagonal para Circulos
#'
#' No caso de empacotamento de circulos, o empacotamento hexagonal posiciona cada circulo
#' de forma que cada um seja cercado por seis outros circulos, formando uma estrutura em
#' formato de colmeia.
#'
#' ## Empacotamento Hexagonal Adaptado para Obround
#'
#' Quando a forma especificada e \code{"semi_rect"}, a funcao considera uma geometria composta
#' por dois semicerculos unidos por um retangulo. Nesta situacao, os semicerculos utilizam o
#' raio definido e o retangulo e acrescentado entre eles com uma altura especificada.
#' Por exemplo, um semicerculo com raio 3 e um retangulo com altura 2 resultaria em uma forma
#' geometrica conhecida como **obround** (tambem chamada de **estadio** ou **discoide** em portugues).
#' Esta forma teria uma largura de 6 unidades (diametro dos semicerculos) e uma altura de 5
#' unidades (raio + altura do retangulo).
#'
#' @param diameter Diametro das figuras geometricas. Este parametro e essencial para
#'   determinar o tamanho das formas a serem empacotadas.
#' @param shape Forma das figuras a serem empacotadas. Pode ser "circle"
#'   para empacotamento de circulos ou "semi_rect" para empacotamento de semicerculos unidos por retangulo.
#' @param rect_height Altura do retangulo que une os semicerculos. Este parametro
#'   e necessario apenas quando shape e "semi_rect".
#'
#' @return Retorna uma lista de objetos ggplot representando diferentes visualizacoes do empacotamento.
#'
#' @examples
#'
#' \dontrun{
#' # Exemplo 1: Empacotamento hexagonal de circulos com diametro 10
#' plots_circle <- plot_shapes_with_triangles(diameter = 10, shape = "circle")
#' print(plots_circle[[1]])
#' print(plots_circle[[2]])
#' print(plots_circle[[3]])
#'
#' # Exemplo 2: Empacotamento hexagonal adaptado para obround com diametro 6 e altura do retangulo 2
#' plots_obround <- plot_shapes_with_triangles(diameter = 6, shape = "semi_rect", rect_height = 2)
#' print(plots_obround[[1]])
#' print(plots_obround[[2]])
#' print(plots_obround[[3]])
#' print(plots_obround[[4]])
#' print(plots_obround[[5]])
#' print(plots_obround[[6]])
#' print(plots_obround[[7]])
#' print(plots_obround[[8]])
#'
#' }
#'
#'@import ggplot2
#'
#' @export
plot_shapes_with_triangles <- function(diameter, shape = "circle", rect_height = NULL) {
  r <- diameter / 2
  if (shape == "circle") {
    Y <- sqrt(3) * r  # Distancia vertical para o circulo de baixo
    # Coordenadas dos circulos
    figure1 <- circle_coordinates(-r, 0, r)
    figure2 <- circle_coordinates(r, 0, r)
    figure3 <- circle_coordinates(0, -Y, r)
    # Coordenadas dos centros
    centers <- data.frame(
      x = c(-r, r, 0),
      y = c(0, 0, -Y)
    )
  } else if (shape == "semi_rect") {
    Y <- height(diameter, shape, rect_height)
    # Coordenadas das figuras
    figure1 <- semi_rect_coordinates(-r, 0, r, rect_height)
    figure2 <- semi_rect_coordinates(r, 0, r, rect_height)
    figure3 <- semi_rect_coordinates(0, -Y, r, rect_height)
    # Coordenadas dos centros dos retangulos
    centers <- data.frame(
      x = c(-r, r, 0),
      y = c(rect_height / 2, rect_height / 2, -Y + rect_height / 2)
    )
  } else {
    stop("Forma nao reconhecida. Use 'circle' ou 'semi_rect'.")
  }

  # Calculo das distancias entre os centros
  dist_12 <- calculate_distance(centers$x[1], centers$y[1], centers$x[2], centers$y[2])
  dist_13 <- calculate_distance(centers$x[1], centers$y[1], centers$x[3], centers$y[3])
  dist_23 <- calculate_distance(centers$x[2], centers$y[2], centers$x[3], centers$y[3])

  # Ponto de interseccao dos circulos superiores
  intersection_x <- 0
  intersection_y <- ifelse(shape == "circle", 0, 0)

  dist_to_intersection <- calculate_distance(centers$x[3], centers$y[3], intersection_x, intersection_y)

  # Primeiro grafico - Figuras com triangulo conectando os centros
  p1 <- ggplot() +
    geom_path(data = figure1, aes(x = x, y = y)) +
    geom_path(data = figure2, aes(x = x, y = y)) +
    geom_path(data = figure3, aes(x = x, y = y)) +
    geom_segment(aes(x = centers$x[1], y = centers$y[1], xend = centers$x[2], yend = centers$y[2]), color = "green", size = 1.5) +
    geom_segment(aes(x = centers$x[1], y = centers$y[1], xend = centers$x[3], yend = centers$y[3]), color = "green", size = 1.5) +
    geom_segment(aes(x = centers$x[2], y = centers$y[2], xend = centers$x[3], yend = centers$y[3]), color = "green", size = 1.5) +
    geom_text(aes(x = (centers$x[1] + centers$x[2]) / 2, y = (centers$y[1] + centers$y[2]) / 2,
                  label = paste0("d12=", round(dist_12, 3))), color = "blue") +
    geom_text(aes(x = (centers$x[1] + centers$x[3]) / 2, y = (centers$y[1] + centers$y[3]) / 2,
                  label = paste0("d13=", round(dist_13, 3))), color = "blue") +
    geom_text(aes(x = (centers$x[2] + centers$x[3]) / 2, y = (centers$y[2] + centers$y[3]) / 2,
                  label = paste0("d23=", round(dist_23, 3))), color = "blue") +
    geom_point(data = centers, aes(x = x, y = y), color = "red") +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),           # Remove grades
      axis.title = element_blank(),           # Remove titulos dos eixos
      axis.text = element_blank(),            # Remove rotulos dos eixos
      axis.ticks = element_blank()            # Remove marcas dos eixos
    ) +
    ggtitle("Figuras com triangulo conectando os centros")

  # Segundo grafico - Apenas o triangulo conectando os centros
  p2 <- ggplot() +
    geom_segment(aes(x = centers$x[1], y = centers$y[1], xend = centers$x[2], yend = centers$y[2]), color = "green", size = 1.5) +
    geom_segment(aes(x = centers$x[1], y = centers$y[1], xend = centers$x[3], yend = centers$y[3]), color = "green", size = 1.5) +
    geom_segment(aes(x = centers$x[2], y = centers$y[2], xend = centers$x[3], yend = centers$y[3]), color = "green", size = 1.5) +
    geom_text(aes(x = (centers$x[1] + centers$x[2]) / 2, y = (centers$y[1] + centers$y[2]) / 2,
                  label = paste0("d12=", round(dist_12, 3))), color = "blue") +
    geom_text(aes(x = (centers$x[1] + centers$x[3]) / 2, y = (centers$y[1] + centers$y[3]) / 2,
                  label = paste0("d13=", round(dist_13, 3))), color = "blue") +
    geom_text(aes(x = (centers$x[2] + centers$x[3]) / 2, y = (centers$y[2] + centers$y[3]) / 2,
                  label = paste0("d23=", round(dist_23, 3))), color = "blue") +
    geom_point(data = centers, aes(x = x, y = y), color = "red") +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggtitle("Triangulo conectando os centros")

  # Terceiro grafico - Distancia entre o centro inferior e o ponto de encontro superior
  p3 <- ggplot() +
    geom_path(data = figure1, aes(x = x, y = y)) +
    geom_path(data = figure2, aes(x = x, y = y)) +
    geom_path(data = figure3, aes(x = x, y = y)) +
    geom_segment(aes(x = centers$x[3], y = centers$y[3], xend = intersection_x, yend = intersection_y), color = "green", size = 1.5) +
    geom_text(aes(x = (centers$x[3] + intersection_x) / 2, y = (centers$y[3] + intersection_y) / 2,
                  label = paste0("Dist. = ", round(dist_to_intersection, 3))), color = "blue") +
    geom_point(data = centers, aes(x = x, y = y), color = "red") +
    geom_point(aes(x = intersection_x, y = intersection_y), color = "red") +
    coord_fixed() +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    ggtitle("Distancia entre centro inferior e ponto de encontro superior")

  # Graficos adicionais especificos para semicerculos com retangulos
  if (shape == "semi_rect") {
    # Calculo adicional para semicerculos com retangulos
    p4 <- ggplot() +
      geom_path(data = figure1, aes(x = x, y = y)) +
      geom_path(data = figure2, aes(x = x, y = y)) +
      geom_path(data = figure3, aes(x = x, y = y)) +
      geom_segment(aes(x = centers$x[3], y = centers$y[3] + rect_height / 2 + r,
                       xend = intersection_x, yend = intersection_y), color = "green", size = 1.5) +
      geom_text(aes(x = (centers$x[3] + intersection_x) / 2,
                    y = (centers$y[3] + rect_height / 2 + r + intersection_y) / 2,
                    label = paste0("Dist. = ", round(Y - r - rect_height, 3))), color = "blue") +
      geom_point(data = centers, aes(x = x, y = y + rect_height / 2 + r), color = "red") +
      geom_point(aes(x = intersection_x, y = intersection_y), color = "red") +
      coord_fixed() +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle("Conectando topo do retangulo inferior ao ponto superior")

    p5 <- ggplot() +
      geom_path(data = figure1, aes(x = x, y = y)) +
      geom_path(data = figure2, aes(x = x, y = y)) +
      geom_path(data = figure3, aes(x = x, y = y)) +
      geom_segment(aes(x = centers$x[3], y = centers$y[3],
                       xend = intersection_x, yend = intersection_y + rect_height / 2), color = "green", size = 1.5) +
      geom_text(aes(x = (centers$x[3] + intersection_x) / 2,
                    y = (centers$y[3] + intersection_y + rect_height / 2) / 2,
                    label = paste0("Dist. = ", round(Y, 3))), color = "blue") +
      geom_point(data = centers, aes(x = x, y = y), color = "red") +
      geom_point(aes(x = intersection_x, y = intersection_y + rect_height / 2), color = "red") +
      coord_fixed() +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle("Conectando centros com valor Y")

    p6 <- ggplot() +
      geom_path(data = figure1, aes(x = x, y = y)) +
      geom_path(data = figure2, aes(x = x, y = y)) +
      geom_path(data = figure3, aes(x = x, y = y)) +
      geom_rect(aes(xmin = -2 * r, xmax = 2 * r, ymin = centers$y[3], ymax = centers$y[2]),
                fill = "lightblue", alpha = 0.3) +
      geom_segment(aes(x = 0, y = centers$y[3], xend = 0, yend = centers$y[2]),
                   color = "green", size = 1.5) +
      geom_text(aes(x = 0.2, y = (centers$y[3] + centers$y[2]) / 2,
                    label = paste0("Dist. = ", round(Y, 3))), color = "blue", size = 4) +
      geom_point(data = centers, aes(x = x, y = y), color = "red") +
      geom_point(aes(x = intersection_x, y = intersection_y + rect_height / 2), color = "red") +
      coord_fixed() +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle("Area compartilhada verticalmente")

    # Area de interseccao entre semicerculos superiores e inferior
    intersect_height <- diameter + rect_height - Y
    p7 <- ggplot() +
      geom_path(data = figure1, aes(x = x, y = y), color = "black") +
      geom_path(data = figure2, aes(x = x, y = y), color = "black") +
      geom_path(data = figure3, aes(x = x, y = y), color = "black") +
      geom_rect(aes(xmin = -2 * r, xmax = 2 * r, ymin = -r, ymax = -r + intersect_height),
                fill = "lightblue", alpha = 0.5) +
      geom_text(aes(x = 0, y = (-r + intersect_height) / 2,
                    label = paste0("Altura da Interseccao = ", round(intersect_height, 3))), color = "blue") +
      coord_fixed() +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle("Area de Interseccao entre Semicerculos")

    # Grafico com linha verde e pontos vermelhos
    p8 <- ggplot() +
      geom_path(data = figure1, aes(x = x, y = y), color = "black") +
      geom_path(data = figure2, aes(x = x, y = y), color = "black") +
      geom_path(data = figure3, aes(x = x, y = y), color = "black") +
      geom_rect(aes(xmin = -2 * r, xmax = 2 * r, ymin = -r, ymax = -r + intersect_height),
                fill = "lightblue", alpha = 0.5) +
      geom_text(aes(x = 0, y = (-r + intersect_height) / 2,
                    label = paste0("Altura da Interseccao = ", round(intersect_height, 3))), color = "blue") +
      geom_segment(aes(x = 0, y = -r, xend = 0, yend = -r + intersect_height), color = "green", size = 1.5) +
      geom_point(aes(x = 0, y = -r), color = "red", size = 3) +
      geom_point(aes(x = 0, y = -r + intersect_height), color = "red", size = 3) +
      coord_fixed() +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      ) +
      ggtitle("Interseccao com linha e pontos")

    return(list(p1, p2, p3, p4, p5, p6, p7, p8))
  } else {
    return(list(p1, p2, p3))
  }
}
