## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_knit$set(global.par = TRUE)

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(igraph)
oldmar <- par()$mar
par(mar = c(0, 0, 1, 0))

## ---- echo = FALSE, results = FALSE-------------------------------------------
I <- matrix(c(1,0,0,0,1,1,1,1,0,0,1,1,1,1,0),3,5)
B <- graph_from_incidence_matrix(I)
V(B)$name <- c("A","B","C","a","b","c","d","e")
plot(B, layout = layout_as_bipartite(B))

## ----setup--------------------------------------------------------------------
set.seed(5)
library(incidentally)

## -----------------------------------------------------------------------------
I <- incidence.from.probability(10, 10, .2)
I
mean(I)  #Fill rate/density

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
I <- incidence.from.adjacency(G, k = 1, p = .75, model = "team")  #Teams model
class(I)  #Incidence matrix returned as igraph object
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, main="Prior Team Collaborations")
plot(I, layout = layout_as_bipartite(I), main="New Team")

## ---- fig.show="hold", out.width="40%"----------------------------------------
G <- erdos.renyi.game(15, .33)  #A random social network of 15 people, as igraph
I <- incidence.from.adjacency(G, k = 1, p = .75, model = "club")  #Groups model
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, main="Social Network")
plot(I, layout = layout_as_bipartite(I), main="New Group")

## ---- fig.show="hold", out.width="40%"----------------------------------------
G <- erdos.renyi.game(15, .5)  #A random social network of 15 people, as igraph
I <- incidence.from.adjacency(G, k = 1, p = .95, model = "org")  #Groups model
V(I)$shape <- ifelse(V(I)$type, "square", "circle")  #Add shapes
plot(G, layout = layout_with_mds(G), main="Social Network")
plot(I, layout = layout_as_bipartite(I), main="New Organizations")

## -----------------------------------------------------------------------------
I <- matrix(c(1,0,0,0,1,0,1,0,1),3,3)
I
curveball(I)

## ---- echo = TRUE, results = 'hide'-------------------------------------------
I <- incidence.from.probability(R = 10, C = 10, P = .2)
blocked <- add.blocks(I, density = .75, 
                       rowblock = c(1,1,1,1,1,2,2,2,2,2),
                       colblock = c(1,1,1,1,1,2,2,2,2,2))

## -----------------------------------------------------------------------------
I  #Original matrix
blocked  #Blocked matrix
all(rowSums(I)==rowSums(blocked)) #Row marginals preserved
all(colSums(I)==colSums(blocked)) #Column marginals preserved

## ---- echo = FALSE------------------------------------------------------------
par(mar = oldmar)

