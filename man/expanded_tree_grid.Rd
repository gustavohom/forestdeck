% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/expanded_tree_grid.R
\name{expanded_tree_grid}
\alias{expanded_tree_grid}
\title{Gerar Grade de Arvores}
\usage{
expanded_tree_grid(limit_x, limit_y, spacing_x, spacing_y)
}
\arguments{
\item{limit_x}{Limite na direcao X (em metros).}

\item{limit_y}{Limite na direcao Y (em metros).}

\item{spacing_x}{Espacamento entre as arvores na direcao X (em metros).}

\item{spacing_y}{Espacamento entre as arvores na direcao Y (em metros).}
}
\value{
Retorna um dataframe com as coordenadas x e y das arvores na grade.
}
\description{
Esta funcao gera uma grade de coordenadas de arvores com base nos limites e espacamento fornecidos.
}
\examples{
# Exemplo 1: Gerar uma grade de arvores com limite de 10m e espacamento de 2m
expanded_tree_grid(limit_x = 10, limit_y = 10, spacing_x = 2, spacing_y = 2)

}
