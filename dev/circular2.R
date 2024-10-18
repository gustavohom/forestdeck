# Se ainda não tiver o pacote tibble instalado, descomente a linha abaixo:
# install.packages("tibble")

# Função para calcular o número de árvores por hectare ou metro quadrado
n_trees <- function(spacing_x = NULL, spacing_y = NULL, area = NULL,
                            side_x = NULL, side_y = NULL,
                            units = "ha",
                            option = "inferior") {

  # Definir opções permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # 1. Determinar a área
  if (!is.null(area)) {
    if (units == "ha") {
      area_m2 <- area * 10000  # Converter hectares para metros quadrados
      message("Método: Usando a área fornecida em hectares.")
      message("Área fornecida: ", area, " ha (", area_m2, " m²).")
    } else if (units == "m2") {
      area_m2 <- area
      message("Método: Usando a área fornecida em metros quadrados.")
      message("Área fornecida: ", area_m2, " m².")
    }
  } else if (!is.null(side_x) && !is.null(side_y)) {
    area_m2 <- side_x * side_y
    message("Método: Calculando área a partir de side_x e side_y.")
    message("side_x = ", side_x, " m, side_y = ", side_y, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(side_x)) {
    area_m2 <- side_x^2
    message("Método: Calculando área a partir de side_x, assumindo side_y = side_x.")
    message("side_x = ", side_x, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(side_y)) {
    area_m2 <- side_y^2
    message("Método: Calculando área a partir de side_y, assumindo side_x = side_y.")
    message("side_y = ", side_y, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else {
    stop("Erro: Área não fornecida e side_x e side_y não fornecidos.")
  }

  # 2. Determinar o espaçamento entre árvores
  if (!is.null(spacing_x) && !is.null(spacing_y)) {
    message("Método: Usando espaçamento fornecido para spacing_x e spacing_y.")
    message("Espaçamento x = ", spacing_x, " m, espaçamento y = ", spacing_y, " m.")
  } else if (!is.null(spacing_x)) {
    spacing_y <- spacing_x
    message("Método: Apenas 'spacing_x' fornecido. Definindo spacing_y = spacing_x = ", spacing_x, " m.")
  } else if (!is.null(spacing_y)) {
    spacing_x <- spacing_y
    message("Método: Apenas 'spacing_y' fornecido. Definindo spacing_x = spacing_y = ", spacing_y, " m.")
  } else {
    stop("Erro: Espaçamento não fornecido. Forneça 'spacing_x', 'spacing_y', ou ambos.")
  }

  # Validar que spacing_x e spacing_y são números positivos
  if (!is.numeric(spacing_x) || !is.numeric(spacing_y) || spacing_x <= 0 || spacing_y <= 0) {
    stop("Erro: Os espaçamentos 'spacing_x' e 'spacing_y' devem ser números positivos.")
  }

  # 3. Calcular o número de árvores
  trees_exact <- area_m2 / (spacing_x * spacing_y)  # Número exato de árvores na área

  if (option == "exato") {
    trees <- trees_exact
    message("Opção: Exato.")
    message("Número exato de árvores: ", trees, ".")
  } else if (option == "inferior") {
    trees <- floor(trees_exact)
    message("Opção: Inferior (arredondando para baixo).")
    message("Número de árvores inferior: ", trees, ".")
  } else if (option == "superior") {
    trees <- ceiling(trees_exact)
    message("Opção: Superior (arredondando para cima).")
    message("Número de árvores superior: ", trees, ".")
  }

  return(trees)
}

# Função para gerar a grade de árvores
expanded_tree_grid <- function(limit_x, limit_y, spacing_x, spacing_y) {
  x_coords <- seq(-limit_x, limit_x, by = spacing_x)
  y_coords <- seq(-limit_y, limit_y, by = spacing_y)
  grid <- expand.grid(x = x_coords, y = y_coords)
  return(grid)
}

# Função para obter o ponto central de acordo com a metodologia usando nomes
circ_central_point <- function(methodology, spacing_x, spacing_y) {
  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  if (methodology == "central") {
    # Árvore central como ponto central do círculo (ponto de uma árvore)
    central_point <- data.frame(x = 0, y = 0)
  } else if (methodology == "vertical") {
    # Ponto central entre duas árvores na vertical
    central_point <- data.frame(x = 0, y = spacing_y / 2)
  } else if (methodology == "horizontal") {
    # Ponto central entre duas árvores na horizontal
    central_point <- data.frame(x = spacing_x / 2, y = 0)
  } else if (methodology == "quadrante") {
    # Ponto central entre quatro árvores
    central_point <- data.frame(x = spacing_x / 2, y = spacing_y / 2)
  } else {
    stop("Metodologia inválida. Escolha 'central', 'vertical', 'horizontal' ou 'quadrante'.")
  }

  return(central_point)
}

# Função para calcular o raio necessário para incluir o número desejado de árvores
circ_radius_for_trees <- function(trees, central_point, desired_tree_count) {
  distances <- sqrt((trees$x - central_point$x)^2 + (trees$y - central_point$y)^2)
  trees$distance <- distances
  ordered_trees <- trees[order(trees$distance), ]

  if (desired_tree_count > nrow(trees)) {
    stop("O número desejado de árvores é maior do que o total disponível.")
  }

  radius <- ordered_trees$distance[desired_tree_count]

  return(radius)
}

# Função para filtrar as árvores dentro do círculo
circ_filter <- function(trees, central_point, radius) {
  distances <- sqrt((trees$x - central_point$x)^2 + (trees$y - central_point$y)^2)
  trees_in_circle <- trees[distances <= radius, ]
  return(trees_in_circle)
}

# Função para contar o número de árvores por linha a partir do ponto central
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

  # Ordenar pelas distâncias verticais
  line_counter <- line_counter[order(line_counter$Vertical_distance), ]
  return(line_counter)
}

# Função para gerar o esquema de caminhamento como tibble com Line_number
circ_walk_scheme <- function(line_counter, central_point, spacing_y) {
  # Ordenar as linhas pela distância vertical ascendente
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
                           " m do centro, há ", line$Number_of_trees, " árvores.")
    } else {
      if (line$Line < central_point$y) {
        # Linha ao Sul
        Direction[i] <- "Sul"
        Line_number[i] <- -south_rank
        Message[i] <- paste0("Na linha Sul ", abs(Line_number[i]),
                             " a ", line$Vertical_distance,
                             " m do centro, há ", line$Number_of_trees, " árvores.")
        south_rank <- south_rank + 1
      } else if (line$Line > central_point$y) {
        # Linha ao Norte
        Direction[i] <- "Norte"
        Line_number[i] <- north_rank
        Message[i] <- paste0("Na linha Norte ", Line_number[i],
                             " a ", line$Vertical_distance,
                             " m do centro, há ", line$Number_of_trees, " árvores.")
        north_rank <- north_rank + 1
      } else {
        # Caso a linha esteja exatamente no ponto central mas Vertical_distance != 0
        # Isso pode ocorrer se central_point$y não é exatamente um dos valores em line$Line
        stop("Erro: Linha não central com Vertical_distance igual a 0.")
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


# Função para plotar as árvores e o círculo
circ_aux_plot <- function(trees, trees_circle, central_point, radius, title) {
  plot(trees$x, trees$y, pch = 19, col = "grey",
       xlab = "Eixo X (m)", ylab = "Eixo Y (m)",
       main = title, asp = 1)
  points(trees_circle$x, trees_circle$y, pch = 19, col = "green")
  symbols(central_point$x, central_point$y, circles = radius,
          add = TRUE, inches = FALSE, border = "red")
  points(central_point$x, central_point$y, pch = 4, col = "blue", cex = 1)
}

# Função principal para executar todo o processo
circ_plot_metrics <- function(spacing_x = NULL, spacing_y = NULL, n = NULL,
                            methodology = "central",
                            area = NULL, side_x = NULL, side_y = NULL,
                            units = "ha",
                            option = "inferior") {

  # Definir opções permitidas para metodologia
  methodology <- match.arg(methodology, choices = c('central', 'vertical', 'horizontal', 'quadrante'))

  # Definir opções permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o número de árvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando número de árvores fornecido: ", desired_trees)
  } else {
    # Calcular o número de árvores usando a função n_trees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y, area = area,
                                     side_x = side_x, side_y = side_y,
                                     units = units,
                                     option = option)
    message("Número de árvores calculado: ", desired_trees)
  }

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de árvores
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  # Filtrar as árvores dentro do círculo
  trees_circle <- circ_filter(trees, central_point, radius)

  # Contar o número de árvores por linha
  line_counter <- circ_count(trees_circle, central_point, spacing_y)

  # Gerar esquema de caminhamento como tibble
  walk_scheme <- circ_walk_scheme(line_counter, central_point, spacing_x)

  # Plotar os resultados
  plot_title <- paste("Metodologia", methodology)
  circ_aux_plot(trees, trees_circle, central_point, radius, plot_title)

  # Exibir informações
  cat("\n---", plot_title, "---\n")
  cat("Ponto central:", paste0("(", central_point$x, ", ", central_point$y, ")"), "\n")
  cat("Raio da parcela circular:", round(radius, 2), "metros\n")
  cat("Número de árvores na parcela circular:", nrow(trees_circle), "\n")
  cat("Esquema de caminhamento:\n")
  print(walk_scheme)

  # Retornar uma lista com todas as informações
  return(list(
    central_point = central_point,
    radius = radius,
    trees_circle = trees_circle,
    line_counter = line_counter,
    walk_scheme = walk_scheme
  ))
}


# Função para plotar as árvores e o círculo
circ_plot <- function(spacing_x = NULL, spacing_y = NULL, n = NULL,
                      methodology = "central",
                      area = NULL, side_x = NULL, side_y = NULL,
                      units = "ha",
                      option = "inferior") {

  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  # Definir opções permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o número de árvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando número de árvores fornecido: ", desired_trees)
  } else {
    # Calcular o número de árvores usando a função n_trees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y, area = area,
                             side_x = side_x, side_y = side_y,
                             units = units,
                             option = option)
    message("Número de árvores calculado: ", desired_trees)
  }

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de árvores usando expanded_tree_grid
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia usando circ_central_point
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário usando circ_radius_for_trees
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  # Filtrar as árvores dentro do círculo usando circ_filter
  trees_circle <- circ_filter(trees, central_point, radius)

  # Plotar os resultados usando cir_plot
  plot_title <- paste("Metodologia", methodology)
  circ_aux_plot(trees, trees_circle, central_point, radius, plot_title)
}


# Função para gerar o esquema de caminhamento como tibble
circ_scheme <- function(spacing_x = NULL, spacing_y = NULL, n = NULL,
                        methodology = "central",
                        area = NULL, side_x = NULL, side_y = NULL,
                        units = "ha",
                        option = "inferior") {

  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  # Definir opções permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o número de árvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando número de árvores fornecido: ", desired_trees)
  } else {
    # Calcular o número de árvores usando a função n_trees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y, area = area,
                             side_x = side_x, side_y = side_y,
                             units = units,
                             option = option)
    message("Número de árvores iniciais: ", desired_trees)
  }

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de árvores usando expanded_tree_grid
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia usando circ_central_point
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário usando circ_radius_for_trees
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  # Filtrar as árvores dentro do círculo usando circ_filter
  trees_circle <- circ_filter(trees, central_point, radius)

  # Contar o número de árvores por linha usando circ_count
  line_counter <- circ_count(trees_circle, central_point, spacing_y)

  # Gerar esquema de caminhamento usando circ_walk_scheme
  walk_scheme <- circ_walk_scheme(line_counter, central_point, spacing_y)

  sum_trees <- sum(walk_scheme$Number_of_trees)

  message("Número de árvores calculado: ", sum_trees)

  # Exibir esquema de caminhamento
  cat("Esquema de caminhamento:\n")
  print(walk_scheme)

  # Retornar o esquema de caminhamento
  return(walk_scheme)
}


# Função para calcular o raio necessário com os parâmetros fornecidos
circ_radius <- function(spacing_x = NULL,
                        spacing_y = NULL,
                        n = NULL,
                        methodology = "central",
                        area = NULL,
                        side_x = NULL,
                        side_y = NULL,
                        units = "ha",
                        option = "inferior") {

  # Validar e selecionar a metodologia usando match.arg
  methodology <- match.arg(methodology, choices = c("central", "vertical", "horizontal", "quadrante"))

  # Definir opções permitidas para unidades
  units <- match.arg(units, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  option <- match.arg(option, choices = c("inferior", "superior", "exato"))

  # Determinar o número de árvores desejado
  if (!is.null(n)) {
    desired_trees <- n
    message("Usando número de árvores fornecido: ", desired_trees)
  } else {
    # Calcular o número de árvores usando a função cir_ntrees
    desired_trees <- n_trees(spacing_x = spacing_x, spacing_y = spacing_y,
                             area = area, side_x = side_x, side_y = side_y,
                             units = units, option = option)
    message("Número de árvores calculado: ", desired_trees)
  }

  # Estimativa inicial do raio
  desired_area <- desired_trees * spacing_x * spacing_y
  estimated_radius <- sqrt(desired_area / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limit_x <- ceiling(estimated_radius / spacing_x) * spacing_x
  limit_y <- ceiling(estimated_radius / spacing_y) * spacing_y

  # Gerar grade de árvores usando expanded_tree_grid
  trees <- expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)

  # Obter o ponto central de acordo com a metodologia usando cir_central
  central_point <- circ_central_point(methodology, spacing_x, spacing_y)

  # Calcular o raio necessário usando circ_radius_for_trees
  radius <- circ_radius_for_trees(trees, central_point, desired_trees)

  cat("Raio necessário para incluir", desired_trees, "árvores:", format(round(radius, 1), nsmall = 1), "metros\n")

  return(radius)
}


# Calcular o raio
circ_radius(spacing_x = 3,
            spacing_y = 2,
            area = 1)


# Exemplo de uso da função cir_plot
circ_plot(spacing_x = 3, spacing_y = 2, n = 1666,
          methodology = "quadrante",
          area = 1)


# Exemplo de uso da função circ_walk_scheme_scheme_func
circ_scheme(spacing_x = 3, spacing_y = 2,
            methodology = "central",
            side_x = 100, side_y = 100)


# Exemplos de uso da função n_trees
cat("\n--- Exemplos de uso da função n_trees ---\n")

# Exemplo 1: Usando a área diretamente em hectares, espaçamento x = 3 m, y = 2 m, opção inferior
# Esperado: 1666 árvores (1 ha = 10,000 m²; 10,000 / (3*2) = 1666.666... => 1666)
cat("\n--- Exemplo 1: Área em hectares ---\n")
n_trees(area = 1, units = "ha", spacing_x = 3, spacing_y = 2, option = "inferior")
# Saída esperada: 1666 árvores

# Exemplo 2: Calculando a área a partir de lados, espaçamento x = 3 m, y = 2 m, opção inferior
# side_x = 100 m, side_y = 100 m (área = 10,000 m² = 1 ha), esperando 1666 árvores
cat("\n--- Exemplo 2: Calculando a área a partir de lados ---\n")
n_trees(side_x = 100, side_y = 100, spacing_x = 3, spacing_y = 2, option = "inferior")
# Saída esperada: 1666 árvores

# Exemplo 3: Calculando a área a partir de apenas um lado, espaçamento x = 3 m, y = 2 m, opção exato
# side_x = 100 m (área = 10,000 m² = 1 ha), esperando 1666 árvores
cat("\n--- Exemplo 3: Calculando a área a partir de um lado ---\n")
n_trees(side_x = 100, spacing_x = 3, spacing_y = 2, option = "exato")
# Saída esperada: 1666 árvores

# Exemplo 4: Usando opção superior com área em metros quadrados, espaçamento x = 3 m, y = 2 m
# área = 10,000 m², 10,000 / (3*2) = 1666.666... => 1667 árvores
cat("\n--- Exemplo 4: Área em metros quadrados com opção superior ---\n")
n_trees(area = 10000, units = "m2", spacing_x = 3, spacing_y = 2, option = "superior")
# Saída esperada: 1667 árvores

# Exemplo 5: Tentativa de usar área em unidades inválidas
cat("\n--- Exemplo 5: Área com unidades inválidas ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# n_trees(area = 1, units = "invalid_unit", spacing_x = 3, spacing_y = 2, option = "inferior")

# Exemplo 6: Tentativa de usar área exata onde não é possível
cat("\n--- Exemplo 6: Opção exato onde não é possível ---\n")
# Isso deve gerar um erro, pois 10,000 / 6 = 1666.666... não é inteiro
# Descomente a linha abaixo para testar
# n_trees(area = 10000, units = "m2", spacing_x = 3, spacing_y = 2, option = "exato")

# Exemplos de uso da função circ_plot_metrics
cat("\n--- Exemplos de uso da função circ_plot_metrics ---\n")

# Exemplo 1: Usando o número de árvores fornecido
# Metodologia "central", espaçamento x = 3 m, y = 2 m, n = 1666
cat("\n--- Exemplo 1: Usando número de árvores fornecido ---\n")
result1 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = 1666, methodology = "central")

# Exemplo 2: Calculando o número de árvores a partir da área em hectares
# Metodologia "vertical", espaçamento x = 3 m, y = 2 m, area = 1 ha, units = "ha", option = "inferior"
cat("\n--- Exemplo 2: Calculando número de árvores a partir da área ---\n")
result2 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = NULL, methodology = "vertical",
                           area = 1, units = "ha", option = "inferior")

# Exemplo 3: Calculando o número de árvores a partir dos lados, opção superior
# Metodologia "horizontal", espaçamento x = 3 m, y = 2 m, side_x = 100 m, side_y = 100 m, option = "superior"
cat("\n--- Exemplo 3: Calculando número de árvores a partir dos lados ---\n")
result3 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = NULL, methodology = "horizontal",
                           side_x = 100, side_y = 100, units = "ha",
                           option = "superior")

# Exemplo 4: Usando o número de árvores fornecido com metodologia "quadrante"
# Metodologia "quadrante", espaçamento x = 3 m, y = 2 m, n = 1667
cat("\n--- Exemplo 4: Usando número de árvores fornecido com metodologia quadrante ---\n")
result4 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = 1667, methodology = "quadrante")

# Exemplo 5: Tentativa de usar área com unidades inválidas no circ_plot_metrics
cat("\n--- Exemplo 5: Executar metodologia com unidades inválidas ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# result5 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = NULL, methodology = "central",
#                             area = 1, units = "invalid_unit", option = "inferior")

# Exemplo 6: Executar metodologia sem fornecer n e sem parâmetros para calcular n
cat("\n--- Exemplo 6: Executar metodologia sem fornecer n e sem parâmetros para calcular n ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# result6 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = NULL, methodology = "central")

# Exemplo 7: Executar metodologia com n não sendo possível exato
cat("\n--- Exemplo 7: Executar metodologia com n não sendo exato ---\n")
# Isso deve gerar um erro se option = "exato"
# Descomente a linha abaixo para testar
# result7 <- circ_plot_metrics(spacing_x = 3, spacing_y = 2, n = 1666.666, methodology = "central",
#                             option = "exato")

# Exemplo adicional: Visualizar o esquema de caminhamento do Exemplo 3
# Certifique-se de que o Exemplo 3 foi executado e 'result3' está disponível
# Para visualizar, use:
View(result2$walk_scheme)
