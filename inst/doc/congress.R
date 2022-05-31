## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
knitr::opts_knit$set(global.par = TRUE)

## ---- echo = FALSE, message = FALSE-------------------------------------------
library(igraph)
oldpar <- par(mar = c(0, 0, 1, 0))
par(mar = c(0, 0, 1, 0))

## -----------------------------------------------------------------------------
library(incidentally)

## ---- echo = TRUE, results = 'hide', warning = FALSE--------------------------
I <- incidence.from.congress(session = 115, types = c("sres"), areas = c("All"), format = "data", narrative = TRUE)

## -----------------------------------------------------------------------------
I$matrix[1:5,1:5]
I$legislator[1:5,1:5]
I$bills[1:5,c(1,2,4,5)]

## ---- echo = TRUE, results = 'hide', warning = FALSE--------------------------
B <- incidence.from.congress(session = 115, types = c("sres"), areas = c("All"), format = "igraph")

## -----------------------------------------------------------------------------
B

## -----------------------------------------------------------------------------
library(backbone)

## -----------------------------------------------------------------------------
network <- sdsm(B, alpha = 0.05, narrative = TRUE)
network

## -----------------------------------------------------------------------------
V(network)$color <- rgb(1,0,0,.5)  #Define the color of Republicans
V(network)$color[which(V(network)$party=="D")] <- rgb(0,0,1,.5)  #...of Democrats
V(network)$color[which(V(network)$party=="I")] <- rgb(0,1,0,.5)  #...of Independents
plot(network, vertex.label = NA, vertex.color = V(network)$color, vertex.frame.color = NA, vertex.size = 10)

## -----------------------------------------------------------------------------
signed <- sdsm(B, alpha = 0.05, signed = TRUE)

## -----------------------------------------------------------------------------
V(signed)$color <- rgb(1,0,0,.5)  #Define the color of Republicans
V(signed)$color[which(V(signed)$party=="D")] <- rgb(0,0,1,.5)  #...of Democrats
V(signed)$color[which(V(signed)$party=="I")] <- rgb(0,1,0,.5)  #...of Independents
E(signed)$color <- rgb(0,1,0,1)  #Define color of positive edges
E(signed)$color[which(E(signed)$weight==-1)] <- rgb(1,0,0,.01)  #Define color of negative edges
layout <- layout_nicely(delete_edges(signed, which(E(signed)$weight==-1)))  #Get layout based on positive edges
plot(signed, vertex.label = NA, vertex.color = V(signed)$color, vertex.frame.color = NA, vertex.size = 10, layout = layout)

## ---- echo = FALSE, message = FALSE-------------------------------------------
par(oldpar) #Restore old parameters

