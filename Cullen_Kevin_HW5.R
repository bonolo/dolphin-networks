# -- CIS576 HW5 ----
#
# Kevin Cullen
# Network Visualization

setwd("~/Projects/cis576/HW5")


# -- References / Credits---- 
# Tutorial...
# Static and dynamic network visualization with R, Katherine Ognyanova
# https://kateto.net/network-visualization
# 
# Cirlize tutorials at R Graph Gallery
# https://www.r-graph-gallery.com/chord-diagram.html
# 
# Hive Network - based loosely upon sample file provided to class...
# plotNetworkUsingHiveR_Good.R
# 
# Data...
# http://networkrepository.com/soc-dolphins.php
# 
# External / replacement functions
# https://www.vesnam.com/Rblog/viznets3/
# mod.mineHPD, mod.edge2HPD
# https://gist.github.com/Vessy/6054742
# mod.adj2HPD.R
 

# -- Libraries ----
library(Matrix)
library(igraph)
library(RColorBrewer)
library(plyr)
library(HiveR)
library(grid)
library(dplyr)


# -- source()s ----

source("mod.adj2HPD.R")
source("mod.mineHPD.R")
source("mod.edge2HPD.R")


# -- settings, etc ----

options(scipen = 999)


# -- Load Text file, clean up, set options. ----

# http://networkrepository.com/soc-dolphins.php
# Dolphin social network. Unweighted. Undirected.
dolphins.m <- readMM("data/soc-dolphins.mtx")
dolphins.df <- summary(dolphins.m)
colnames(dolphins.df) <- c("from", "to")

# --~~ Build igraph object from CSV files ----
igraph.net <- graph_from_data_frame(dolphins.df, directed = FALSE
                                         , vertices = union(dolphins.df$from, dolphins.df$to))

# class(igraph.net)
# E(igraph.net)         # The edges of the "net" object
# V(igraph.net)         # The vertices of the "net" object
# E(igraph.net)$weight  # Edge attribute "weight" -- Dolphin data set has no weight

# Remove loops in the graph
igraph.net <- simplify(igraph.net, remove.multiple = F, remove.loops = T)


# -- Network plots... loop layouts ----

# Set a network layout and plot:
graph_attr(igraph.net, "layout") <- layout_with_graphopt
plot(igraph.net, vertex.label = NA)
# With curved edges...
# plot(igraph.net, edge.curved = .1)

# Let’s take a look at all available layouts in igraph:
layouts <- grep("^layout_", ls("package:igraph"), value = TRUE)[-1]

# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

# par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
# for (layout in layouts) {
#   print(layout)
#   l <- do.call(layout, list(igraph.net))
#   plot(
#     igraph.net,
#     edge.arrow.mode = 0,
#     vertex.label = NA,
#     layout = l,
#     main = layout
#   )
# }


# -- Community Detection ----

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(igraph.net)
class(clp)


# --~~ Plot w/ communities class built-in ----
# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp,
     igraph.net,
     layout = layout_with_fr, # layout_with_mds | layout_with_graphopt
     vertex.size = 5,
     vertex.label = NA)


# We can also plot the communities without relying on their built-in plot:
V(igraph.net)$community <- clp$membership
# Make a list to hold a separate vector for each detected community
groups.l <- list()
for(i in unique(V(igraph.net)$community)){
  groups.l[[i]] <- as.vector(V(igraph.net)[community == i])
}

# colrs <- brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set2")
colrs <- adjustcolor(brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set1"), alpha = .7)
group.colrs <- adjustcolor(brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set1"), alpha = .2)

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
  layout = layout_with_fr, # layout_with_mds
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
  main = "Dolphin Social Network Colored by Community",
  sub = "Nodes = Dolphins. Communities detected with cluster_optimal()"
)


# as.vector(V(igraph.net)[community == 1])
# unique(V(igraph.net)$community)


# -- Circle Plot (circlize) ----

# https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html

# Load library
library(circlize)

# create dataframe of edges w/ communities & colors
circle.e.df <- data.frame(as_edgelist(igraph.net))
colnames(circle.e.df) <- c("from", "to")

# create data frame of vertices w/ communities & colors
circle.v.df <- data.frame(node.id = as.factor(V(igraph.net))
                       , community = as.factor(V(igraph.net)$community)
                       , community.color = as.character(colrs[V(igraph.net)$community])
                       )
circle.v.df$community.color <- as.character(circle.v.df$community.color) # Twice? Really?
circle.v.df <- circle.v.df[order(circle.v.df$community),]

# join community & color on to the edge list, using from node.id
circle.e.df <- left_join(circle.e.df, circle.v.df, by = c("from" = "node.id"))
gridcolor <- circle.v.df$community.color


# Create circular plot from edge list
chordDiagram(circle.e.df[,1:2]
             , transparency = 0.5
             , order = circle.v.df$node.id # set colors set before sorting data!!!
             , grid.col = gridcolor
             )
title("Circle Plot - Dolphin social networks colored by community")
# union(circle.e.df[[1]], circle.e.df[[2]])

circos.clear()




# -- Hive Pre-work ----

# --~~ Set network attributes ----
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

rm(degAll, betAll, betAll.norm)


# --~~ node/edge color based on the properties ----

# Calculate node size
# We'll interpolate node size based on the node betweenness centrality, using the "approx" function
# And we will assign a node size for each node based on its betweenness centrality
approxVals <- approx(c(0.5, 1.5), n = length(unique(V(igraph.net)$betweenness)))
nodes_size <- sapply(V(igraph.net)$betweenness, function(x) approxVals$y[which(sort(unique(V(igraph.net)$betweenness)) == x)])
rm(approxVals)


# Define node color

V(igraph.net)$degree
V(igraph.net)$community
V(igraph.net)$color
V(igraph.net)$degree

# And we will assign a color for each node based on its community
V(igraph.net)$color <- colrs[V(igraph.net)$community]




# Assign visual attributes to edges using the same approach as we did for nodes
F2 <- colorRampPalette(c("#FFFF00", "#006400"), bias = length(unique(E(igraph.net)$similarity)), space = "rgb", interpolate = "linear")
colCodes <- F2(length(unique(E(igraph.net)$similarity)))
edges_col <- sapply(E(igraph.net)$similarity, function(x) colCodes[which(sort(unique(E(igraph.net)$similarity)) == x)])
rm(F2, colCodes)



# -- HiveR start ----

# Create a hive plot from the data frame
hive1 <- mod.edge2HPD(edge_df = dolphins.df)

hive1 <- mod.edge2HPD(edge_df = circle.e.df[,1:2]
                      , edge.color = circle.e.df$community.color
                      , node.color = circle.v.df[,c(1,3)]
                      )

# mod.edge2HPD <- function(edge_df = NULL, unique.rows = TRUE, axis.cols = NULL, type = "2D", 
#                         desc = NULL, 
#                         edge.weight = NULL, edge.color = NULL, 
#                         node.color = NULL, node.size = NULL, node.radius = NULL, node.axis = NULL) 
  #edge.weight - a list corresponding to edge weights (same order as in edge_df)
  #edge.color - a lis corresponding to edge colors (same order as in edge_df)
  #node.color - a data frame consisting of two columns: column 1 - node labels, column 2 - node color
  #node.size - a data frame consisting of two columns: column 1 - node labels, column 2 - node size
  #node.radius - a data frame consisting of two columns: column 1 - node labels, column 2 - node radius
  #node.axis - a data frame consisting of two columns: column 1 - node labels, column 2 - node axis



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




# --~~ node/edge customization ----

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



##########

