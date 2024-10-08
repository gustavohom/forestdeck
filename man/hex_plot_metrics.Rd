% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hex_plot_metrics.R
\name{hex_plot_metrics}
\alias{hex_plot_metrics}
\title{Plotar Formas com Triangulos Conectando os Centros}
\usage{
hex_plot_metrics(diameter, rect_height = NULL, shape = "circle")
}
\arguments{
\item{diameter}{Diametro das figuras geometricas. Este parametro e essencial para
determinar o tamanho das formas a serem empacotadas.}

\item{rect_height}{Altura do retangulo que une os semicerculos. Este parametro
e necessario apenas quando shape e "semi_rect".}

\item{shape}{Forma das figuras a serem empacotadas. Pode ser "circle"
para empacotamento de circulos ou "semi_rect" para empacotamento de semicerculos unidos por retangulo.}
}
\value{
Retorna uma lista de objetos ggplot representando diferentes visualizacoes do empacotamento.
}
\description{
Esta funcao plota formas geometricas organizadas em um padrao de empacotamento hexagonal,
que e o metodo mais eficiente para empacotar circulos sem sobreposicao. O empacotamento
hexagonal maximiza o uso do espaco, reduzindo o desperdicio e aumentando a densidade de
empacotamento em comparacao com outros metodos, como o empacotamento quadrado.
}
\details{
\subsection{Empacotamento Hexagonal para Circulos}{

No caso de empacotamento de circulos, o empacotamento hexagonal posiciona cada circulo
de forma que cada um seja cercado por seis outros circulos, formando uma estrutura em
formato de colmeia.
}

\subsection{Empacotamento Hexagonal Adaptado para Obround}{

Quando a forma especificada e \code{"semi_rect"}, a funcao considera uma geometria composta
por dois semicerculos unidos por um retangulo. Nesta situacao, os semicerculos utilizam o
raio definido e o retangulo e acrescentado entre eles com uma altura especificada.
Por exemplo, um semicerculo com raio 3 e um retangulo com altura 2 resultaria em uma forma
geometrica conhecida como \strong{obround} (tambem chamada de \strong{estadio} ou \strong{discoide} em portugues).
Esta forma teria uma largura de 6 unidades (diametro dos semicerculos) e uma altura de 5
unidades (raio + altura do retangulo).
}
}
\examples{

\dontrun{
# Exemplo 1: Empacotamento hexagonal de circulos com diametro 10
plots_circle <- hex_plot_metrics(diameter = 10)
print(plots_circle[[1]])
print(plots_circle[[2]])
print(plots_circle[[3]])

# Exemplo 2: Empacotamento hexagonal adaptado para obround com diametro 6 e altura do retangulo 2
plots_obround <- hex_plot_metrics(diameter = 6, rect_height = 2, shape = "semi_rect")
print(plots_obround[[1]])
print(plots_obround[[2]])
print(plots_obround[[3]])
print(plots_obround[[4]])
print(plots_obround[[5]])
print(plots_obround[[6]])
print(plots_obround[[7]])
print(plots_obround[[8]])

}

}
