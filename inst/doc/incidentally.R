## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_knit$set(global.par = TRUE)

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(igraph)
oldpar <- par(mar = c(0, 0, 1, 0))
par(mar = c(0, 0, 1, 0))

## ----setup--------------------------------------------------------------------
set.seed(5)
library(incidentally)

## -----------------------------------------------------------------------------
I <- incidence.from.probability(10, 10, .2)
I
mean(I)  #Fill rate/Density

## -----------------------------------------------------------------------------
I <- incidence.from.probability(10, 10, .2, constrain = FALSE)
I
mean(I)  #Fill rate/Density

## -----------------------------------------------------------------------------
I <- incidence.from.vector(c(4,3,2), c(1,2,2,2,2))
I
rowSums(I)  #Row marginals
colSums(I)  #Column marginals

## ---- fig.show="hold", out.width="33%"----------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(1,1), coldist = c(1,1))
hist(rowSums(I), main = "Row Marginals")
hist(colSums(I), main = "Column Marginals")

## ---- fig.show="hold", out.width="33%"----------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(1,10), coldist = c(1,10))
hist(rowSums(I), main = "Row Marginals")
hist(colSums(I), main = "Column Marginals")

## ---- fig.show="hold", out.width="33%"----------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(10,1), coldist = c(10,1))
hist(rowSums(I), main = "Row Marginals")
hist(colSums(I), main = "Column Marginals")

## ---- fig.show="hold", out.width="33%"----------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(10,10), coldist = c(10,10))
hist(rowSums(I), main = "Row Marginals")
hist(colSums(I), main = "Column Marginals")

## -----------------------------------------------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(10000,10000), coldist = c(10000,10000))
rowSums(I)
colSums(I)

## ---- fig.show="hold", out.width="33%"----------------------------------------
I <- incidence.from.distribution(R = 100, C = 100, P = 0.2,
  rowdist = c(1,10), coldist = c(10,1))
hist(rowSums(I), main = "Row Marginals")
hist(colSums(I), main = "Column Marginals")

## ---- fig.show="hold", out.width="40%"----------------------------------------
G <- erdos.renyi.game(15, .5)  #A random social network of 15 people, as igraph
I <- incidence.from.adjacency(G, k = 3, p = .75, model = "team")  #Teams model
class(I)  #Incidence matrix returned as igraph object
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, main="Social Network")
plot(I, layout = layout_as_bipartite(I), main="New Teams")

## ---- fig.show="hold", out.width="40%"----------------------------------------
G <- erdos.renyi.game(15, .33)  #A random social network of 15 people, as igraph
I <- incidence.from.adjacency(G, k = 3, p = .75, model = "group")  #Groups model
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, main="Social Network")
plot(I, layout = layout_as_bipartite(I), main="New Groups")

## ---- fig.show="hold", out.width="40%"----------------------------------------
G <- erdos.renyi.game(15, .33)  #A random social network of 15 people, as igraph
I <- incidence.from.adjacency(G, k = 3, d = 2, p = .90, model = "blau")  #Groups model
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, layout = layout_with_mds(G), main="Social Network")
plot(I, layout = layout_as_bipartite(I), main="New Organizations")

## -----------------------------------------------------------------------------
I <- incidence.from.probability(10, 10, .3)
I
rowSums(I)
colSums(I)
I <- add.blocks(I, blocks = 2, density = .8)
I
rowSums(I)
colSums(I)

## ---- echo = FALSE, message = FALSE-------------------------------------------
par(oldpar) #Restore old parameters

