#########
# CIS576 HW5
# Kevin Cullen
# Network Visualization

setwd("~/Projects/cis576/HW5")

# library()  # all statements needed to load libs
# library(tidyverse)
# library(reshape)
library(Matrix)
library(igraph)
library(RColorBrewer)
# library(scales)
# library(ggraph)

# -- Sources --- 
# Tutorial...
# Static and dynamic network visualization with R, Katherine Ognyanova
# https://kateto.net/network-visualization
# 
# Data...
# http://networkrepository.com/soc-dolphins.php
# 

# --------- Theme, scales, etc ---------------

options(scipen = 999)

# --------- Load Text file, clean up, set options. ----------

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
# class(igraph.net)
# E(igraph.net)         # The edges of the "net" object
# V(igraph.net)         # The vertices of the "net" object
# E(igraph.net)$weight  # Edge attribute "weight"

# Remove loops in the graph
igraph.net <- simplify(igraph.net, remove.multiple = F, remove.loops = T)
plot(igraph.net)

# Plot with curved edges (edge.curved=.1)
plot(igraph.net, edge.curved = .1)

# Compute node degrees (#links) and use that to set node size:
# deg <- degree(igraph.net, mode = "all")
# V(igraph.net)$size <- deg / 3

# Set edge width based on weight
# E(igraph.net)$width <- E(igraph.net)$weight/10
# E(igraph.net)$width <- E(igraph.net)$weight
# E(igraph.net)$width <- rescale(E(igraph.net)$weight)

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


# --~~ Examine weight -----
# Dolphin data set has no weight
# hist(E(igraph.net)$weight, breaks = 25)
# hist(E(igraph.net)$weight)
# mean(E(igraph.net)$weight)
# sd(E(igraph.net)$weight)

# Keep edges with weight > mean
# igraph.net.sp <- delete_edges(igraph.net, E(igraph.net)[weight < mean(E(igraph.net)$weight)])


# -- Community Detection -------

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(igraph.net)
class(clp)


# ------~~ Plot w/ communities class built-in ------
# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp,
     igraph.net,
     layout = layout_with_fr, # layout_with_graphopt
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




##########
