# Definir parâmetros iniciais
n_values <- seq(10, 500, by = 10)
N <- 1666  # Número total de árvores
spacing_x <- 2  # Ajuste conforme necessário
spacing_y <- 2  # Ajuste conforme necessário

# Calcular erros e raios
results <- circ_errors_radius(
  n_values = n_values,
  N = N,
  spacing_x = spacing_x,
  spacing_y = spacing_y,
  Z = 1.96,  # Nível de confiança de 95%
  p = 0.5,   # Máxima variabilidade
  methodology = "central",
  units = "ha",
  option = "inferior"
)

# Exibir resultados
print(results$Radius)
