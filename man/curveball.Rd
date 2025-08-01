% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/curveball.R
\name{curveball}
\alias{curveball}
\title{Randomize an incidence matrix or bipartite graph using the curveball algorithm}
\usage{
curveball(M, trades = 5 * nrow(M), class = NULL)
}
\arguments{
\item{M}{a binary matrix of class \code{matrix} or \code{Matrix}, or a bipartite graph of class \link[igraph]{igraph}.}

\item{trades}{integer: number of trades; the default is 5 * nrow(M) (approx. mixing time)}

\item{class}{string: Return object as \code{matrix}, \code{Matrix}, \code{igraph}. If \code{NULL}, object is returned in the same class as \code{M}.}
}
\value{
An incidence matrix of class \code{matrix} or \code{Matrix}, or a bipartite graph of class \link[igraph]{igraph}.
}
\description{
\code{curveball} randomizes an incidence matrix or bipartite graph, preserving the row and column sums
}
\details{
Strona et al. (2014) provided an initial implementation of the Curveball algorithm in R. \code{curveball()} is a modified R
implementation that is slightly more efficient. For an even more efficient algorithm, see \code{backbone::fastball()}.
}
\examples{
M <- incidence.from.probability(5,5,.5)  #A matrix
Mrand <- curveball(M)  #Random matrix with same row/col sums
all.equal(rowSums(M), rowSums(curveball(M)))
all.equal(colSums(M), colSums(curveball(M)))
}
\references{
{Strona, Giovanni, Domenico Nappo, Francesco Boccacci, Simone Fattorini, and Jesus San-Miguel-Ayanz. 2014. A Fast and Unbiased Procedure to Randomize Ecological Binary Matrices with Fixed Row and Column Totals. \emph{Nature Communications, 5}, 4114. \doi{10.1038/ncomms5114}}

{Godard, Karl and Neal, Zachary P. 2022. fastball: A fast algorithm to sample bipartite graphs with fixed degree sequences. \emph{Journal of Complex Networks, 10}, cnac049. \doi{10.1093/comnet/cnac049}}

{Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. \emph{CRAN} \doi{10.32614/CRAN.package.incidentally}}
}
