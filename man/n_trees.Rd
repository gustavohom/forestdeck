% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_trees.R
\name{n_trees}
\alias{n_trees}
\title{Calcular o Numero de arvores por Hectare ou Metro Quadrado}
\usage{
n_trees(
  spacing_x = NULL,
  spacing_y = NULL,
  area = NULL,
  side_x = NULL,
  side_y = NULL,
  units = "ha",
  option = "inferior"
)
}
\arguments{
\item{spacing_x}{Espacamento entre as arvores na direcao X (em metros). Opcional se \code{spacing_y} for fornecido.}

\item{spacing_y}{Espacamento entre as arvores na direcao Y (em metros). Opcional se \code{spacing_x} for fornecido.}

\item{area}{area da parcela. Pode ser fornecida em hectares (\code{units = "ha"}) ou metros quadrados (\code{units = "m2"}). Opcional se \code{side_x} e \code{side_y} forem fornecidos.}

\item{side_x}{Comprimento do lado X da parcela (em metros). Opcional se \code{area} for fornecida.}

\item{side_y}{Comprimento do lado Y da parcela (em metros). Opcional se \code{area} for fornecida.}

\item{units}{Unidade de medida da area. Pode ser \code{"ha"} para hectares ou \code{"m2"} para metros quadrados. Padrao e \code{"ha"}.}

\item{option}{Opcao de arredondamento para o numero de arvores. Pode ser \code{"inferior"} para arredondar para baixo, \code{"superior"} para arredondar para cima ou \code{"exato"} para manter o valor exato. Padrao e \code{"inferior"}.}
}
\value{
Retorna um numero que representa a quantidade de arvores na parcela, de acordo com as opcoes de arredondamento especificadas.
}
\description{
Esta funcao calcula o numero de arvores por hectare ou por metro quadrado com base no espacamento entre as arvores e na area da parcela. Permite especificar opcoes de arredondamento para o numero de arvores resultante.
}
\details{
\subsection{Detalhes}{

A funcao pode determinar a area da parcela de varias maneiras:
\itemize{
\item Utilizando uma area fornecida diretamente em hectares ou metros quadrados.
\item Calculando a area a partir dos lados \code{side_x} e \code{side_y}.
\item Assumindo que \code{side_x} e igual a \code{side_y} caso apenas um dos lados seja fornecido.
}

O espacamento entre as arvores pode ser fornecido em ambas as direcoes \code{spacing_x} e \code{spacing_y}. Se apenas um dos espacamentos for fornecido, o outro sera assumido como igual.

A opcao de arredondamento (\code{option}) permite escolher entre arredondar o numero de arvores para baixo (\code{inferior}), para cima (\code{superior}) ou manter o valor exato (\code{exato}).
}
}
\examples{
# Exemplo 1: Calcular o numero de arvores em 1 hectare com espacamento de 2m x 2m e arredondar para baixo
n_trees(spacing_x = 2, spacing_y = 2, area = 1, units = "ha", option = "inferior")

# Exemplo 2: Calcular o numero de arvores em 5000 metros quadrados com espacamento de 3m x 2m e arredondar para cima
n_trees(spacing_x = 3, spacing_y = 2, area = 5000, units = "m2", option = "superior")

# Exemplo 3: Calcular o numero de arvores a partir dos lados da parcela (100m x 50m) com espacamento de 2m e arredondar para exato
n_trees(spacing_x = 2, spacing_y = 2, side_x = 100, side_y = 50, option = "exato")

# Exemplo 4: Calcular o numero de arvores fornecendo apenas o espacamento na direcao X (assume spacing_y = spacing_x)
n_trees(spacing_x = 2.5, area = 2, units = "ha", option = "inferior")

}
