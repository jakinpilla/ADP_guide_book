setwd("C:/Users/dsc/adp_guidebook")

# install.packages('igraph')
library(igraph)

ga.data <- read.csv('./data/ga_edgelist.csv', header=T)

g <- graph.data.frame(ga.data, directed = F)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

V(g)$label <- NA
V(g)$size <- degree(g)*2
plot(g)

clo <- closeness(g)
clo.score <- round( (clo - min(clo))*length(clo)/max(clo) ) + 1
clo.colors <- rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[clo.score]
plot(g)

btw <- betweenness(g)
btw.score <- round(btw) + 1
btw.colors <- rev(heat.colors(max(btw.score)))
V(g)$color <- btw.colors[btw.score]
plot(g)
