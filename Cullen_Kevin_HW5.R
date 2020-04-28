# ---- CIS576 HW5 ----
#
# Kevin Cullen
# Network Visualization

setwd("~/Projects/cis576/HW5")

# ---- Libraries ----
# library()  # all statements needed to load libs
# library(tidyverse)
# library(reshape)
library(Matrix)
library(igraph)
library(RColorBrewer)
library(plyr)
library(HiveR)
library(grid)
# library(scales)
# library(ggraph)



# ---- References ---- 
# Tutorial...
# Static and dynamic network visualization with R, Katherine Ognyanova
# https://kateto.net/network-visualization
# 
# Sample file provided to class: plotNetworkUsingHiveR_Good.R
# 
# Data...
# http://networkrepository.com/soc-dolphins.php
# 

# ----~~ and source()s ----

source("mod.adj2HPD.R")
source("mod.mineHPD.R")
source("mod.edge2HPD.R")


# --------- Theme, scales, etc ---------------

options(scipen = 999)

# ---- Load Text file, clean up, set options. ----

# http://networkrepository.com/soc-dolphins.php
# Dolphin social network. Unweighted. Undirected.
dolphins.m <- readMM("data/soc-dolphins.mtx")
dolphins.df <- summary(dolphins.m)
colnames(dolphins.df) <- c("from", "to")

# http://networkrepository.com/eco-everglades.php
# everglades.df <- read.csv("data/eco-everglades.edges", header = FALSE, sep = " ")
# colnames(everglades.df) <- c("from", "to", "weight")

# http://networkrepository.com/aves-thornbill-farine.php
# thornbill.df <- read.csv("data/aves-thornbill-farine.edges", header = FALSE, sep = " ")
# colnames(thornbill.df) <- c("from", "to", "weight")

# build igraph object from CSV files
igraph.net <- graph_from_data_frame(dolphins.df, directed = FALSE
                                         , vertices = union(dolphins.df$from, dolphins.df$to))

# igraph.net <- graph_from_data_frame(everglades.df, directed = FALSE
#                                     , vertices = union(everglades.df$from, everglades.df$to))

# igraph.net <- graph_from_data_frame(thornbill.df, directed = FALSE
#                                     , vertices = union(thornbill.df$start, thornbill.df$end))
#                                     
# class(igraph.net)
# E(igraph.net)         # The edges of the "net" object
# V(igraph.net)         # The vertices of the "net" object
# E(igraph.net)$weight  # Edge attribute "weight"
# 
# --~~ Examine weight -----
# Dolphin data set has no weight
# hist(E(igraph.net)$weight, breaks = 25)
# hist(E(igraph.net)$weight)
# mean(E(igraph.net)$weight)
# sd(E(igraph.net)$weight)

# Keep edges with weight > mean
# igraph.net.sp <- delete_edges(igraph.net, E(igraph.net)[weight < mean(E(igraph.net)$weight)])

# Set edge width based on weight
# E(igraph.net)$width <- E(igraph.net)$weight/10
# E(igraph.net)$width <- E(igraph.net)$weight
# E(igraph.net)$width <- rescale(E(igraph.net)$weight)



# Remove loops in the graph
igraph.net <- simplify(igraph.net, remove.multiple = F, remove.loops = T)
# plot(igraph.net)
# Plot with curved edges (edge.curved=.1)
# plot(igraph.net, edge.curved = .1)


# ---- Network plots... loop layouts ----


# Set the network layout:
graph_attr(igraph.net, "layout") <- layout_with_graphopt
plot(igraph.net, vertex.label = NA)

# Let’s take a look at all available layouts in igraph:
layouts <- grep("^layout_", ls("package:igraph"), value = TRUE)[-1]

# Remove layouts that do not apply to our graph.
layouts <-
  layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

# par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(igraph.net))
  plot(
    igraph.net,
    edge.arrow.mode = 0,
    vertex.label = NA,
    layout = l,
    main = layout
  )
}


# -- Community Detection -------

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(igraph.net)
class(clp)


# ------~~ Plot w/ communities class built-in ------
# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp,
     igraph.net,
     layout = layout_with_fr, # layout_with_mds | layout_with_graphopt
     vertex.size = 5,
     vertex.label = NA)


# We can also plot the communities without relying on their built-in plot:
V(igraph.net)$community <- clp$membership
# Make a list to hold vectors for each detected community
groups.l <- list()
for(i in unique(V(igraph.net)$community)){
  groups.l[[i]] <- as.vector(V(igraph.net)[community == i])
}

# colrs <- brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set2")
colrs <- adjustcolor(brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set2"), alpha = .7)
group.colrs <- adjustcolor(brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set2"), alpha = .2)

# plot(
#   igraph.net,
#   mark.groups = groups.l,
#   mark.col = colrs,
#   mark.border = NA,
#   vertex.size = 5,
#   vertex.label = NA
# )

plot(
  igraph.net,
  layout = layout_with_fr,
  # layout = layout_with_graphopt, # Examine settings at: https://igraph.org/r/doc/layout_with_graphopt.html
  charge = 0.0001,
  max.sa.movement = 20,
  spring.length = 50,
  vertex.color = colrs[V(igraph.net)$community],
  vertex.frame.color = colrs[V(igraph.net)$community],
  # edge.curved = 0.2,
  vertex.size = 5,
  vertex.label = NA,
  mark.groups = groups.l,
  mark.col = group.colrs,
  mark.border = NA,
  main = "Network plot - Vertices colored by community",
  sub = "Communities calculated with cluster_optimal()"
)


# as.vector(V(igraph.net)[community == 1])
# unique(V(igraph.net)$community)


# ---- Hive Pre-work ----

# ----~~ Set network attributes ----
# When I put this code before community detection, the community results were
# nuts. Coloring didn't make sense, tiny groups, etc.


# Calculate degree for all nodes (#links)
degAll <- igraph::degree(igraph.net, v = V(igraph.net), mode = "all")
# Calculate betweenness for all nodes
betAll <- igraph::betweenness(igraph.net, v = V(igraph.net), directed = FALSE) / (((vcount(igraph.net) - 1) * (vcount(igraph.net)-2)) / 2)
betAll.norm <- (betAll - min(betAll))/(max(betAll) - min(betAll))
# Set node degree and betweenness
igraph.net <- igraph::set.vertex.attribute(igraph.net, "degree", index = V(igraph.net), value = degAll)
igraph.net <- igraph::set.vertex.attribute(igraph.net, "betweenness", index = V(igraph.net), value = betAll.norm)

# Zero out edge weight and similarity
igraph.net <- igraph::set.edge.attribute(igraph.net, "weight", index = E(igraph.net), value = 0)
igraph.net <- igraph::set.edge.attribute(igraph.net, "similarity", index = E(igraph.net), value = 0)

# Calculate Dice similarities between all pairs of nodes
dsAll <- igraph::similarity.dice(igraph.net, vids = V(igraph.net), mode = "all")

# Calculate edge weight based on the node similarity
F1 <- function(x) {data.frame(V4 = dsAll[which(V(igraph.net)$name == as.character(x$from)), which(V(igraph.net)$name == as.character(x$to))])}
dolphins.ext <- ddply(dolphins.df, .variables=c("from", "to"), function(x) data.frame(F1(x)))

for (i in 1:nrow(dolphins.ext))
{
  # dolphin dataset has no weight
  # E(igraph.net)[as.character(dolphins.ext$from) %--% as.character(dolphins.ext$to)]$weight <- as.numeric(dolphins.ext$V3)
  E(igraph.net)[as.character(dolphins.ext$from) %--% as.character(dolphins.ext$to)]$similarity <- as.numeric(dolphins.ext$V4)
}

rm(degAll, betAll, betAll.norm, F1, dsAll, i)

write.table(dolphins.ext, "dolphin_similarity.csv", row.names = FALSE, 
            col.names = c("from", "to", "similarity"), sep=",")



# ----~~ node/edge color based on the properties ----

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(V(igraph.net)$betweenness)))
nodes_size <- sapply(V(igraph.net)$betweenness, function(x) approxVals$y[which(sort(unique(V(igraph.net)$betweenness)) == x)])
rm(approxVals)

# Define node color
# We'll interpolate node colors based on the node degree using the "colorRampPalette" function from the "grDevices" library
library("grDevices")
# This function returns a function corresponding to a collor palete of "bias" number of elements
F2 <- colorRampPalette(c("#F5DEB3", "#FF0000"), bias = length(unique(V(igraph.net)$degree)), space = "rgb", interpolate = "linear")
# Now we'll create a color for each degree
colCodes <- F2(length(unique(V(igraph.net)$degree)))
# And we will assign a color for each node based on its degree
nodes_col <- sapply(V(igraph.net)$degree, function(x) colCodes[which(sort(unique(V(igraph.net)$degree)) == x)])
rm(F2, colCodes)

# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(E(igraph.net)$similarity)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(igraph.net)$similarity)))
edges_col <- sapply(E(igraph.net)$similarity, function(x) colCodes[which(sort(unique(E(igraph.net)$similarity)) == x)])
rm(F2, colCodes)



# ---- HiveR start ----

# Create a hive plot from the data frame
hive1 <- mod.edge2HPD(edge_df = dolphins.ext)
#sumHPD(hive1)

# Assign nodes to a radius based on their degree (number of edges they are touching)
hive2 <- mod.mineHPD(hive1, option = "rad <- tot.edge.count")

# Assign nodes to axes based on their position in the edge list 
# (this function assumes direct graphs, so it considers the first column to be a source and second column to be a sink )
hive3 <- mod.mineHPD(hive2, option = "axis <- source.man.sink")

# Removing zero edges for better visualization 
hive4 <- mod.mineHPD(hive3, option = "remove zero edge")

# And finally, plotting our graph (Figure 1)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), 
         axLab.gpar = gpar(col = c("red", "blue", "green")), axLab.pos = 10)




# ----~~ node/edge customization ----

# First do nodes
nodes <- hive4$nodes

# Change the node color and size based on node degree and betweenness values
for (i in 1:nrow(nodes))
{
  nodes$color[i] <- nodes_col[which(nodes$lab[i] == V(igraph.net)$name)]
  nodes$size[i] <- nodes_size[which(nodes$lab[i] == V(igraph.net)$name)]
}

# Reassign these nodes to the hive(4) object
hive4$nodes <- nodes

# And plot it (Figure 2)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), 
         axLab.gpar = gpar(col = c("red", "blue", "green")), axLab.pos = 10)

# Now do the edges
edges <- hive4$edges

# Change the edge color based on Dice similarity
for (i in 1:nrow(edges))
{
  index1 <- which(nodes$id == edges$id1[i])
  index2 <- which(nodes$id == edges$id2[i])
  
  edges$color[i] <- edges_col[which(E(igraph.net)[as.character(nodes$lab[index1]) %--% as.character(nodes$lab[index2])] == E(igraph.net))]
}

# Reassign these edges to the hive(4) object
hive4$edges <- edges

# And plot it (Figure 3)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), 
         axLab.gpar = gpar(col = brewer.pal(6, "Dark2")), axLab.pos = 10)

# Some edges are too thick, so we will reduce the edge weight (thickness) by 25%
hive4$edges$weight <- hive4$edges$weight/4

# And plot it (Figure 5)
plotHive(hive4, method = "abs", bkgnd = "white", axLabs = c("source", "hub", "sink"), 
         axLab.gpar = gpar(col = c("red", "blue", "green")), axLab.pos = 10)


##########
