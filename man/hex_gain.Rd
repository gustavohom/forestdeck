% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_gain.R
\name{hex_gain}
\alias{hex_gain}
\title{Analisar a Eficiencia do Empacotamento Hexagonal de Formas Geometricas}
\usage{
hex_gain(
  diameter,
  area_size,
  rect_height = NULL,
  xlim = 20,
  shape = "circle",
  exibir_resultados = TRUE,
  exibir_plot = TRUE
)
}
\arguments{
\item{diameter}{Diametro das formas geometricas (em unidades).}

\item{area_size}{Tamanho da area disponivel para empacotamento (em unidades).}

\item{rect_height}{Altura do retangulo que une os semicerculos. Necessario apenas quando shape e "semi_rect".}

\item{xlim}{Valor maximo do plot do grafico}

\item{shape}{Forma geometrica a ser empacotada. Pode ser "circle" para circulos ou "semi_rect" para
semicerculos unidos por retangulo. Default e "circle".}

\item{exibir_resultados}{Booleano que determina se os resultados detalhados devem ser exibidos.
Default e FALSE.}

\item{exibir_plot}{Plot o grafico de ganho em relação a altura quando \code{semi_rect}.
Default e TRUE.}
}
\value{
Retorna uma lista com os seguintes componentes:
\describe{
\item{total_shapes}{Numero total de formas que cabem na area utilizando empacotamento hexagonal.}
\item{odd_rows}{Numero de fileiras impares no empacotamento.}
\item{even_rows}{Numero de fileiras pares no empacotamento.}
\item{shapes_per_row_odd}{Numero de formas por fileira impar.}
\item{shapes_per_row_even}{Numero de formas por fileira par.}
\item{num_rows}{Numero total de fileiras no empacotamento.}
\item{rect_packing_shapes}{Numero de formas que caberiam na area utilizando empacotamento retangular.}
\item{increase_percentage}{Porcentagem de aumento na capacidade com empacotamento hexagonal.}
\item{altura_total_empacotamento}{Altura total do empacotamento hexagonal.}
\item{altura_superior_empacotamento}{Altura superior do empacotamento hexagonal.}
}
}
\description{
Esta funcao calcula o ganho percentual na capacidade de empacotamento de formas geometricas
(circulos ou obrounds) utilizando o metodo hexagonal em comparacao com o empacotamento retangular.
}
\examples{

\dontrun{
# Exemplo 1: Analisar empacotamento hexagonal de circulos com diametro 1 em uma area de 1000000 unidades
resultado_circulos <- hex_gain(diameter = 1, area_size = 10000, xlim = 30)

# Exemplo 2: Analisar empacotamento hexagonal de obrounds com diametro 1, altura do retangulo 2 em uma area de 1000000 unidades
resultado_obrounds <- hex_gain(diameter = 1, area_size = 10000, rect_height = 2,
                                     shape = "semi_rect", exibir_resultados = TRUE)
}

}
