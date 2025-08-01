% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/incidence.from.probability.R
\name{incidence.from.probability}
\alias{incidence.from.probability}
\title{Generates an incidence matrix with a given cell-filling probability}
\usage{
incidence.from.probability(
  R,
  C,
  P = 0,
  constrain = TRUE,
  class = "matrix",
  narrative = FALSE
)
}
\arguments{
\item{R}{integer: number of rows}

\item{C}{integer: number of columns}

\item{P}{numeric: probability that a cell contains a 1; if P = 0 a probability will be chosen randomly}

\item{constrain}{boolean: ensure that no rows or columns sum to 0 (i.e., contain all 0s) or to 1 (i.e., contain all 1s)}

\item{class}{string: the class of the returned backbone graph, one of c("matrix", "Matrix", "igraph").}

\item{narrative}{boolean: TRUE if suggested text & citations should be displayed.}
}
\value{
An incidence matrix of class \code{matrix} or \code{Matrix}, or a bipartite graph of class \link[igraph]{igraph}.
}
\description{
\code{incidence.from.probability} generates a random incidence matrix in which each cell is filled
with a 1 with a given probability.
}
\examples{
I <- incidence.from.probability(R = 10, C = 10)
I <- incidence.from.probability(R = 10, C = 10, P = .5)
I <- incidence.from.probability(R = 10, C = 10, P = .5,
     class = "igraph", narrative = TRUE)
}
\references{
{Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. \emph{Scientific Reports, 11}, 23929. \doi{10.1038/s41598-021-03238-3}}

{Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. \emph{CRAN} \doi{10.32614/CRAN.package.incidentally}}
}
