% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add.blocks.R
\name{add.blocks}
\alias{add.blocks}
\title{Adds a block structure to an incidence matrix}
\usage{
add.blocks(
  I,
  rowblock = sample(1:2, replace = T, nrow(I)),
  colblock = sample(1:2, replace = T, ncol(I)),
  density = 0.5,
  sorted = FALSE
)
}
\arguments{
\item{I}{An incidence matrix or \link[igraph]{igraph} bipartite graph}

\item{rowblock}{numeric: vector indicating each row node's block membership}

\item{colblock}{numeric: vector indicating each column node's block membership}

\item{density}{numeric: desired within-block density}

\item{sorted}{boolean: if TRUE, return incidence matrix permuted by block}
}
\value{
An incidence matrix or \link[igraph]{igraph} bipartite graph with a block structure
}
\description{
\code{add.blocks} shuffles an incidence matrix to have a block structure or planted partition while preserving the row and column sums
}
\details{
Stochastic block and planted partition models generate graphs in which the probability that two nodes are connected
depends on whether they are members of the same or different blocks/partitions. Functions such as \link[igraph]{sample_sbm}
can randomly sample from stochastic block models with given probabilities. In contrast \code{add.blocks} adds a block
structure to an existing incidence matrix while preserving the row and column sums. Row nodes' and column nodes'
block memberships are supplied in separate vectors. If block membership vectors are not provided, then nodes are
randomly assigned to two groups.
}
\examples{
I <- incidence.from.probability(R = 100, C = 100, P = .1)
blocked <- add.blocks(I, density = .7)
all(rowSums(I)==rowSums(blocked))
all(colSums(I)==colSums(blocked))

B <- igraph::sample_bipartite(100, 100, p=.1)
blocked <- add.blocks(B, density = .7)
all(igraph::degree(B)==igraph::degree(blocked))
}
\references{
{Neal, Z. P., Domagalski, R., and Sagan, B. 2021. Comparing alternatives to the fixed degree sequence model for extracting the backbone of bipartite projections. \emph{Scientific Reports, 11}, 23929. \doi{10.1038/s41598-021-03238-3}}

{Neal, Z. P. 2022. incidentally: An R package to generate incidence matrices and bipartite graphs. \emph{CRAN} \doi{10.32614/CRAN.package.incidentally}}
}
