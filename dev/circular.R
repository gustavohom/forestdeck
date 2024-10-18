# Se ainda não tiver o pacote tibble instalado, descomente a linha abaixo:
# install.packages("tibble")

# Função para calcular o número de árvores por hectare ou metro quadrado
n_arvores <- function(x = NULL, y = NULL, area = NULL,
                      lado_x = NULL, lado_y = NULL,
                      unidades = "ha",
                      opcao = "inferior") {

  # Definir opções permitidas para unidades
  unidades <- match.arg(unidades, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  opcao <- match.arg(opcao, choices = c("inferior", "superior", "exato"))

  # 1. Determinar a área
  if (!is.null(area)) {
    if (unidades == "ha") {
      area_m2 <- area * 10000  # Converter hectares para metros quadrados
      message("Método: Usando a área fornecida em hectares.")
      message("Área fornecida: ", area, " ha (", area_m2, " m²).")
    } else if (unidades == "m2") {
      area_m2 <- area
      message("Método: Usando a área fornecida em metros quadrados.")
      message("Área fornecida: ", area_m2, " m².")
    }
  } else if (!is.null(lado_x) && !is.null(lado_y)) {
    area_m2 <- lado_x * lado_y
    message("Método: Calculando área a partir de lado_x e lado_y.")
    message("lado_x = ", lado_x, " m, lado_y = ", lado_y, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(lado_x)) {
    area_m2 <- lado_x^2
    message("Método: Calculando área a partir de lado_x, assumindo lado_y = lado_x.")
    message("lado_x = ", lado_x, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else if (!is.null(lado_y)) {
    area_m2 <- lado_y^2
    message("Método: Calculando área a partir de lado_y, assumindo lado_x = lado_y.")
    message("lado_y = ", lado_y, " m.")
    message("Área calculada: ", area_m2, " m² (", round(area_m2 / 10000, 4), " ha).")
  } else {
    stop("Erro: Área não fornecida e lados_x e lados_y não fornecidos.")
  }

  # 2. Determinar o espaçamento entre árvores
  if (!is.null(x) && !is.null(y)) {
    message("Método: Usando espaçamento fornecido para x e y.")
    message("Espaçamento x = ", x, " m, espaçamento y = ", y, " m.")
  } else if (!is.null(x)) {
    y <- x
    message("Método: Apenas 'x' fornecido. Definindo y = x = ", x, " m.")
  } else if (!is.null(y)) {
    x <- y
    message("Método: Apenas 'y' fornecido. Definindo x = y = ", y, " m.")
  } else {
    stop("Erro: Espaçamento não fornecido. Forneça 'x', 'y', ou ambos.")
  }

  # Validar que x e y são números positivos
  if (!is.numeric(x) || !is.numeric(y) || x <= 0 || y <= 0) {
    stop("Erro: Os espaçamentos 'x' e 'y' devem ser números positivos.")
  }

  # 3. Calcular o número de árvores
  trees_exact <- area_m2 / (x * y)  # Número exato de árvores na área

  if (opcao == "exato") {
    trees <- trees_exact
    message("Opção: Exato.")
    message("Número exato de árvores: ", trees, ".")
  } else if (opcao == "inferior") {
    trees <- floor(trees_exact)
    message("Opção: Inferior (arredondando para baixo).")
    message("Número de árvores inferior: ", trees, ".")
  } else if (opcao == "superior") {
    trees <- ceiling(trees_exact)
    message("Opção: Superior (arredondando para cima).")
    message("Número de árvores superior: ", trees, ".")
  }

  return(trees)
}

# Função para gerar a grade de árvores
gerar_grade_arvores <- function(limite_x, limite_y, espacamento_x, espacamento_y) {
  x_coords <- seq(-limite_x, limite_x, by = espacamento_x)
  y_coords <- seq(-limite_y, limite_y, by = espacamento_y)
  grid <- expand.grid(x = x_coords, y = y_coords)
  return(invisible(grid))
}
#
# gerar_grade_arvores(100,100,3,3)

# Função para obter o ponto central de acordo com a metodologia usando nomes
get_ponto_central <- function(metodologia, espacamento_x, espacamento_y) {
  # Validar e selecionar a metodologia usando match.arg
  metodologia <- match.arg(metodologia, choices = c("central", "vertical", "horizontal", "quadrante"))

  if (metodologia == "central") {
    # Árvore central como ponto central do círculo (ponto de uma árvore)
    ponto_central <- data.frame(x = 0, y = 0)
  } else if (metodologia == "vertical") {
    # Ponto central entre duas árvores na vertical
    ponto_central <- data.frame(x = 0, y = espacamento_y / 2)
  } else if (metodologia == "horizontal") {
    # Ponto central entre duas árvores na horizontal
    ponto_central <- data.frame(x = espacamento_x / 2, y = 0)
  } else if (metodologia == "quadrante") {
    # Ponto central entre quatro árvores
    ponto_central <- data.frame(x = espacamento_x / 2, y = espacamento_y / 2)
  } else {
    stop("Metodologia inválida. Escolha 'central', 'vertical', 'horizontal' ou 'quadrante'.")
  }

  return(ponto_central)
}

# Função para calcular o raio necessário para incluir o número desejado de árvores
calcular_raio_por_n_arvores <- function(arvores, ponto_central, n_arvores_desejado) {
  distancias <- sqrt((arvores$x - ponto_central$x)^2 + (arvores$y - ponto_central$y)^2)
  arvores$distancia <- distancias
  arvores_ordenadas <- arvores[order(arvores$distancia), ]

  if (n_arvores_desejado > nrow(arvores)) {
    stop("O número desejado de árvores é maior do que o total disponível.")
  }

  raio <- arvores_ordenadas$distancia[n_arvores_desejado]

  return(raio)
}

# Função para filtrar as árvores dentro do círculo
filtrar_arvores_circulo <- function(arvores, ponto_central, raio) {
  distancias <- sqrt((arvores$x - ponto_central$x)^2 + (arvores$y - ponto_central$y)^2)
  arvores_no_circulo <- arvores[distancias <= raio, ]
  return(arvores_no_circulo)
}

# Função para contar o número de árvores por linha a partir do ponto central
contar_arvores_por_linha <- function(arvores_circulo, ponto_central, espacamento_y) {
  linhas <- unique(arvores_circulo$y)
  linhas <- sort(linhas)
  contador_linhas <- data.frame(
    Linha = linhas,
    Distancia_vertical = abs(linhas - ponto_central$y),
    Numero_de_arvores = integer(length(linhas))
  )

  for (i in seq_along(linhas)) {
    linha <- linhas[i]
    arvores_linha <- arvores_circulo[arvores_circulo$y == linha, ]
    contador_linhas$Numero_de_arvores[i] <- nrow(arvores_linha)
  }

  # Ordenar pelas distâncias verticais
  contador_linhas <- contador_linhas[order(contador_linhas$Distancia_vertical), ]
  return(contador_linhas)
}

# Função para gerar o esquema de caminhamento como tibble
gerar_esquema_caminhamento <- function(contador_linhas, ponto_central, espacamento_x) {
  # Criar vetores de Direcao e Mensagem
  Direcao <- character(nrow(contador_linhas))
  Mensagem <- character(nrow(contador_linhas))

  for (i in 1:nrow(contador_linhas)) {
    linha <- contador_linhas[i, ]
    if (linha$Distancia_vertical > 0) {
      direcao <- ifelse(linha$Linha > ponto_central$y, "Norte", "Sul")
    } else {
      direcao <- "Central"
    }
    Direcao[i] <- direcao
    Mensagem[i] <- paste0("Na linha ", direcao, " a ", linha$Distancia_vertical, " m do centro, há ", linha$Numero_de_arvores, " árvores.")
  }

  # Criar um tibble usando tibble::tibble
  esquema <- tibble::tibble(
    Direcao = Direcao,
    Distancia_vertical = contador_linhas$Distancia_vertical,
    Numero_de_arvores = contador_linhas$Numero_de_arvores,
    Mensagem = Mensagem
  )

  return(esquema)
}

# Função para plotar as árvores e o círculo
plotar_parcela_circular <- function(arvores, arvores_circulo, ponto_central, raio, titulo) {
  plot(arvores$x, arvores$y, pch = 19, col = "grey",
       xlab = "Eixo X (m)", ylab = "Eixo Y (m)",
       main = titulo, asp = 1)
  points(arvores_circulo$x, arvores_circulo$y, pch = 19, col = "green")
  symbols(ponto_central$x, ponto_central$y, circles = raio,
          add = TRUE, inches = FALSE, border = "red")
  points(ponto_central$x, ponto_central$y, pch = 4, col = "blue", cex = 1)
}

# Função principal para executar todo o processo
executar_metodologia <- function(x = NULL, y = NULL, n = NULL,
                                 metodologia = "central",
                                 area = NULL, lado_x = NULL, lado_y = NULL,
                                 unidades = "ha",
                                 opcao = "inferior") {

  # Definir opções permitidas para metodologia
  metodologia <- match.arg(metodologia, choices = c('central', 'vertical', 'horizontal', 'quadrante'))

  # Definir opções permitidas para unidades
  unidades <- match.arg(unidades, choices = c("ha", "m2"))

  # Validar a opção escolhida para arredondamento
  opcao <- match.arg(opcao, choices = c("inferior", "superior", "exato"))

  # Determinar o número de árvores desejado
  if (!is.null(n)) {
    n_arvores_desejado <- n
    message("Usando número de árvores fornecido: ", n_arvores_desejado)
  } else {
    # Calcular o número de árvores usando a função n_arvores
    n_arvores_desejado <- n_arvores(x = x, y = y, area = area,
                                    lado_x = lado_x, lado_y = lado_y,
                                    unidades = unidades,
                                    opcao = opcao)
    message("Número de árvores calculado: ", n_arvores_desejado)
  }

  # Estimativa inicial do raio
  area_desejada <- n_arvores_desejado * x * y
  raio_estimado <- sqrt(area_desejada / pi)

  # Limites para a grade (um pouco maiores que o raio estimado)
  limite_x <- ceiling(raio_estimado / x) * x
  limite_y <- ceiling(raio_estimado / y) * y

  # Gerar grade de árvores
  arvores <- gerar_grade_arvores(limite_x, limite_y, x, y)

  # Obter o ponto central de acordo com a metodologia
  ponto_central <- get_ponto_central(metodologia, x, y)

  # Calcular o raio necessário
  raio <- calcular_raio_por_n_arvores(arvores, ponto_central, n_arvores_desejado)

  # Filtrar as árvores dentro do círculo
  arvores_circulo <- filtrar_arvores_circulo(arvores, ponto_central, raio)

  # Contar o número de árvores por linha
  contador_linhas <- contar_arvores_por_linha(arvores_circulo, ponto_central, y)

  # Gerar esquema de caminhamento como tibble
  esquema <- gerar_esquema_caminhamento(contador_linhas, ponto_central, x)

  # Plotar os resultados
  titulo <- paste("Metodologia", metodologia)
  plotar_parcela_circular(arvores, arvores_circulo, ponto_central, raio, titulo)

  # Exibir informações
  cat("\n---", titulo, "---\n")
  cat("Ponto central:", paste0("(", ponto_central$x, ", ", ponto_central$y, ")"), "\n")
  cat("Raio da parcela circular:", round(raio, 2), "metros\n")
  cat("Número de árvores na parcela circular:", nrow(arvores_circulo), "\n")
  cat("Esquema de caminhamento:\n")
  print(esquema)

  # Retornar uma lista com todas as informações
  return(list(
    ponto_central = ponto_central,
    raio = raio,
    arvores_circulo = arvores_circulo,
    contador_linhas = contador_linhas,
    esquema_caminhamento = esquema
  ))
}

# Exemplos de uso da função n_arvores
cat("\n--- Exemplos de uso da função n_arvores ---\n")

# Exemplo 1: Usando a área diretamente em hectares, espaçamento x = 3 m, y = 2 m, opção inferior
# Esperado: 1666 árvores (1 ha = 10,000 m²; 10,000 / (3*2) = 1666.666... => 1666)
cat("\n--- Exemplo 1: Área em hectares ---\n")
n_arvores(area = 1, unidades = "ha", x = 3, y = 2, opcao = "inferior")
# Saída esperada: 1666 árvores

# Exemplo 2: Calculando a área a partir de lados, espaçamento x = 3 m, y = 2 m, opção inferior
# lado_x = 100 m, lado_y = 100 m (área = 10,000 m² = 1 ha), esperando 1666 árvores
cat("\n--- Exemplo 2: Calculando a área a partir de lados ---\n")
n_arvores(lado_x = 100, lado_y = 100, x = 3, y = 2, opcao = "inferior")
# Saída esperada: 1666 árvores

# Exemplo 3: Calculando a área a partir de apenas um lado, espaçamento x = 3 m, y = 2 m, opção exato
# lado_x = 100 m (área = 10,000 m² = 1 ha), esperando 1666 árvores
cat("\n--- Exemplo 3: Calculando a área a partir de um lado ---\n")
n_arvores(lado_x = 100, x = 3, y = 2, opcao = "exato")
# Saída esperada: 1666 árvores

# Exemplo 4: Usando opção superior com área em metros quadrados, espaçamento x = 3 m, y = 2 m
# área = 10,000 m², 10,000 / (3*2) = 1666.666... => 1667 árvores
cat("\n--- Exemplo 4: Área em metros quadrados com opção superior ---\n")
n_arvores(area = 10000, unidades = "m2", x = 3, y = 2, opcao = "superior")
# Saída esperada: 1667 árvores

# Exemplo 5: Tentativa de usar área em unidades inválidas
cat("\n--- Exemplo 5: Área com unidades inválidas ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# n_arvores(area = 1, unidades = "invalid_unit", x = 3, y = 2, opcao = "inferior")

# Exemplo 6: Tentativa de usar área exata onde não é possível
cat("\n--- Exemplo 6: Opção exato onde não é possível ---\n")
# Isso deve gerar um erro, pois 10,000 / 6 = 1666.666... não é inteiro
# Descomente a linha abaixo para testar
# n_arvores(area = 10000, unidades = "m2", x = 3, y = 2, opcao = "exato")

# Exemplos de uso da função executar_metodologia
cat("\n--- Exemplos de uso da função executar_metodologia ---\n")

# Exemplo 1: Usando o número de árvores fornecido
# Metodologia 1, espaçamento x = 3 m, y = 2 m, n = 1666, metodologia = 1
cat("\n--- Exemplo 1: Usando número de árvores fornecido ---\n")
resultado1 <- executar_metodologia(x = 3, y = 2, n = 1666, metodologia = "central")

# Exemplo 2: Calculando o número de árvores a partir da área em hectares
# Metodologia 2, espaçamento x = 3 m, y = 2 m, area = 1 ha, unidades = "ha", opcao = "inferior"
cat("\n--- Exemplo 2: Calculando número de árvores a partir da área ---\n")
resultado2 <- executar_metodologia(x = 3, y = 2, n = NULL, metodologia = "vertical",
                                   area = 1, unidades = "ha", opcao = "inferior")

# Exemplo 3: Calculando o número de árvores a partir dos lados, opção superior
# Metodologia 3, espaçamento x = 3 m, y = 2 m, lado_x = 100 m, lado_y = 100 m, opcao = "superior"
cat("\n--- Exemplo 3: Calculando número de árvores a partir dos lados ---\n")
resultado3 <- executar_metodologia(x = 3, y = 3, n = NULL, metodologia = "horizontal",
                                   lado_x = 100, lado_y = 100, unidades = "ha",
                                   opcao = "superior")

# Exemplo 4: Usando o número de árvores fornecido com metodologia 4
# Metodologia 4, espaçamento x = 3 m, y = 2 m, n = 1667, metodologia = 4
cat("\n--- Exemplo 4: Usando número de árvores fornecido com metodologia 4 ---\n")
resultado4 <- executar_metodologia(x = 3, y = 3, area = 1, metodologia = "quadrante")

10000/9
# Exemplo 5: Tentativa de usar área com unidades inválidas no executar_metodologia
cat("\n--- Exemplo 5: Executar metodologia com unidades inválidas ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# resultado5 <- executar_metodologia(x = 3, y = 2, n = NULL, metodologia = 1,
#                                    area = 1, unidades = "invalid_unit", opcao = "inferior")

# Exemplo 6: Executar metodologia sem fornecer n e sem parâmetros para calcular n
cat("\n--- Exemplo 6: Executar metodologia sem fornecer n e sem parâmetros para calcular n ---\n")
# Isso deve gerar um erro
# Descomente a linha abaixo para testar
# resultado6 <- executar_metodologia(x = 3, y = 2, n = NULL, metodologia = 1)

# Exemplo 7: Executar metodologia com n não sendo possível exato
cat("\n--- Exemplo 7: Executar metodologia com n não sendo exato ---\n")
# Isso deve gerar um erro se opcao = "exato"
# Descomente a linha abaixo para testar
# resultado7 <- executar_metodologia(x = 3, y = 2, n = 1666.666, metodologia = 1,
#                                    opcao = "exato")

View(resultado3$esquema_caminhamento)

