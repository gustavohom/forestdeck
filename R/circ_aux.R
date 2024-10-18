#' Obter Ponto Central de Acordo com a Metodologia
#'
#' Esta funcao retorna o ponto central baseado na metodologia selecionada.
#'
#' @param methodology Metodologia a ser usada. Opcoes: "central", "vertical", "horizontal", "quadrante".
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros).
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#'
#' @return Retorna um dataframe com as coordenadas x e y do ponto central.

circ_central_point <- function(methodology, spacing_x, spacing_y) {
  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  if (methodology == "central") {
    # arvore central como ponto central do circulo (ponto de uma arvore)
    central_point <- data.frame(x = 0, y = 0)
  } else if (methodology == "vertical") {
    # Ponto central entre duas arvores na vertical
    central_point <- data.frame(x = 0, y = spacing_y / 2)
  } else if (methodology == "horizontal") {
    # Ponto central entre duas arvores na horizontal
    central_point <- data.frame(x = spacing_x / 2, y = 0)
  } else if (methodology == "quadrante") {
    # Ponto central entre quatro arvores
    central_point <- data.frame(x = spacing_x / 2, y = spacing_y / 2)
  } else {
    stop("Metodologia invalida. Escolha 'central', 'vertical', 'horizontal' ou 'quadrante'.")
  }

  return(central_point)
}


#' Calcular Raio Necessario para Incluir Numero Desejado de Arvores
#'
#' Esta funcao calcula o raio necessario para incluir um numero especifico de arvores a partir de um ponto central.
#'
#' @param trees Dataframe contendo as coordenadas x e y das arvores.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param desired_tree_count Numero desejado de arvores a serem incluidas no circulo.
#'
#' @return Retorna o raio (em metros) necessario para incluir o numero desejado de arvores.

circ_radius_for_trees <- function(trees, central_point, desired_tree_count) {
  distances <- sqrt((trees$x - central_point$x)^2 + (trees$y - central_point$y)^2)
  trees$distance <- distances
  ordered_trees <- trees[order(trees$distance), ]

  if (desired_tree_count > nrow(trees)) {
    stop("O numero desejado de arvores e maior do que o total disponivel.")
  }

  radius <- ordered_trees$distance[desired_tree_count]

  return(radius)
}

#' Filtrar Arvores Dentro de um Circulo
#'
#' Esta funcao filtra as arvores que estao dentro de um circulo definido por um ponto central e um raio.
#'
#' @param trees Dataframe contendo as coordenadas x e y das arvores.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param radius Raio do circulo (em metros).
#'
#' @return Retorna um dataframe com as arvores que estao dentro do circulo.

circ_filter <- function(trees, central_point, radius) {
  distances <- sqrt((trees$x - central_point$x)^2 + (trees$y - central_point$y)^2)
  trees_in_circle <- trees[distances <= radius, ]
  return(trees_in_circle)
}


#' Obter Ponto Central de Acordo com a Metodologia
#'
#' Esta funcao retorna o ponto central baseado na metodologia selecionada.
#'
#' @param methodology Metodologia a ser usada. Opcoes: "central", "vertical", "horizontal", "quadrante".
#' @param spacing_x Espacamento entre as arvores na direcao X (em metros).
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#'
#' @return Retorna um dataframe com as coordenadas x e y do ponto central.

circ_central_point <- function(methodology, spacing_x, spacing_y) {
  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  if (methodology == "central") {
    # Arvore central como ponto central do circulo (ponto de uma arvore)
    central_point <- data.frame(x = 0, y = 0)
  } else if (methodology == "vertical") {
    # Ponto central entre duas arvores na vertical
    central_point <- data.frame(x = 0, y = spacing_y / 2)
  } else if (methodology == "horizontal") {
    # Ponto central entre duas arvores na horizontal
    central_point <- data.frame(x = spacing_x / 2, y = 0)
  } else if (methodology == "quadrante") {
    # Ponto central entre quatro arvores
    central_point <- data.frame(x = spacing_x / 2, y = spacing_y / 2)
  } else {
    stop("Metodologia invalida. Escolha 'central', 'vertical', 'horizontal' ou 'quadrante'.")
  }

  return(central_point)
}

#' Filtrar Arvores Dentro de um Circulo
#'
#' Esta funcao filtra as arvores que estao dentro de um circulo definido por um ponto central e um raio.
#'
#' @param trees Dataframe contendo as coordenadas x e y das arvores.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param radius Raio do circulo (em metros).
#'
#' @return Retorna um dataframe com as arvores que estao dentro do circulo.

circ_filter <- function(trees, central_point, radius) {
  distances <- sqrt((trees$x - central_point$x)^2 + (trees$y - central_point$y)^2)
  trees_in_circle <- trees[distances <= radius, ]
  return(trees_in_circle)
}

#' Contar Numero de Arvores por Linha a Partir do Ponto Central
#'
#' Esta funcao conta o numero de arvores em cada linha horizontal dentro de um circulo a partir do ponto central.
#'
#' @param trees_circle Dataframe contendo as coordenadas x e y das arvores dentro do circulo.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#'
#' @return Retorna um dataframe com o numero de arvores por linha e a distancia vertical da linha ao ponto central.

circ_count <- function(trees_circle, central_point, spacing_y) {
  lines <- unique(trees_circle$y)
  lines <- sort(lines)
  line_counter <- data.frame(
    Line = lines,
    Vertical_distance = abs(lines - central_point$y),
    Number_of_trees = integer(length(lines))
  )

  for (i in seq_along(lines)) {
    line <- lines[i]
    trees_line <- trees_circle[trees_circle$y == line, ]
    line_counter$Number_of_trees[i] <- nrow(trees_line)
  }

  # Ordenar pelas distancias verticais
  line_counter <- line_counter[order(line_counter$Vertical_distance), ]
  return(line_counter)
}


#' Gerar Esquema de Caminhamento como Tibble com Line_number
#'
#' Esta funcao gera um esquema de caminhamento baseado no contador de linhas, ponto central e espaçamento vertical. O esquema inclui a direcao (Norte, Sul ou Central), o numero da linha, a distancia vertical e uma mensagem descritiva.
#'
#' @param line_counter Dataframe contendo as informacoes de cada linha, incluindo `Line`, `Vertical_distance` e `Number_of_trees`.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param spacing_y Espacamento entre as arvores na direcao Y (em metros).
#'
#' @return Retorna um tibble com as colunas `Line_number`, `Direction`, `Vertical_distance`, `Number_of_trees` e `Message`, representando o esquema de caminhamento.

circ_walk_scheme <- function(line_counter, central_point, spacing_y) {
  # Ordenar as linhas pela distancia vertical ascendente
  sorted_lines <- line_counter[order(line_counter$Vertical_distance), ]

  # Inicializar vetores para Direction, Line_number e Message
  Direction <- character(nrow(sorted_lines))
  Line_number <- integer(nrow(sorted_lines))
  Message <- character(nrow(sorted_lines))

  # Inicializar contadores para linhas ao Sul e Norte
  south_rank <- 1
  north_rank <- 1

  for (i in 1:nrow(sorted_lines)) {
    line <- sorted_lines[i, ]

    if (line$Vertical_distance == 0) {
      # Linha central
      Direction[i] <- "Central"
      Line_number[i] <- 0
      Message[i] <- paste0("Na linha Central a ", line$Vertical_distance,
                           " m do centro, ha ", line$Number_of_trees, " arvores.")
    } else {
      if (line$Line < central_point$y) {
        # Linha ao Sul
        Direction[i] <- "Sul"
        Line_number[i] <- -south_rank
        Message[i] <- paste0("Na linha Sul ", abs(Line_number[i]),
                             " a ", line$Vertical_distance,
                             " m do centro, ha ", line$Number_of_trees, " arvores.")
        south_rank <- south_rank + 1
      } else if (line$Line > central_point$y) {
        # Linha ao Norte
        Direction[i] <- "Norte"
        Line_number[i] <- north_rank
        Message[i] <- paste0("Na linha Norte ", Line_number[i],
                             " a ", line$Vertical_distance,
                             " m do centro, ha ", line$Number_of_trees, " arvores.")
        north_rank <- north_rank + 1
      } else {
        # Caso a linha esteja exatamente no ponto central mas Vertical_distance != 0
        # Isso pode ocorrer se central_point$y nao e exatamente um dos valores em line$Line
        stop("Erro: Linha nao central com Vertical_distance igual a 0.")
      }
    }
  }

  # Criar um tibble usando tibble::tibble
  walk_scheme <- tibble::tibble(
    Line_number = Line_number,
    Direction = Direction,
    Vertical_distance = sorted_lines$Vertical_distance,
    Number_of_trees = sorted_lines$Number_of_trees,
    Message = Message
  )

  return(walk_scheme)
}

#' Plotar Arvores e Circulo
#'
#' Esta funcao plota as arvores e um circulo com base nos parametros fornecidos.
#'
#' @param trees Dataframe contendo as coordenadas x e y das arvores.
#' @param trees_circle Dataframe contendo as coordenadas x e y das arvores dentro do circulo.
#' @param central_point Dataframe com as coordenadas x e y do ponto central.
#' @param radius Raio do circulo (em metros).
#' @param title Titulo do grafico.
#'
#' @return Nenhum. A funcao gera um grafico.

circ_aux_plot <- function(trees, trees_circle, central_point, radius, title) {
  plot(trees$x, trees$y, pch = 19, col = "grey",
       xlab = "Eixo X (m)", ylab = "Eixo Y (m)",
       main = title, asp = 1)
  points(trees_circle$x, trees_circle$y, pch = 19, col = "green")
  symbols(central_point$x, central_point$y, circles = radius,
          add = TRUE, inches = FALSE, border = "red")
  points(central_point$x, central_point$y, pch = 4, col = "blue", cex = 1)
}
