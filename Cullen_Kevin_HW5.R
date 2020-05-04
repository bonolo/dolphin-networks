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
# And the Circlize Vignette
# https://jokergoo.github.io/circlize_book/book/
# 
# Hive Network - based loosely upon sample file provided to class...
# plotNetworkUsingHiveR_Good.R
# 
# Data...
# http://networkrepository.com/soc-dolphins.php
# 
# External / replacement functions
#   https://www.vesnam.com/Rblog/viznets3/: mod.mineHPD(), mod.edge2HPD()
#   https://gist.github.com/Vessy/6054742: mod.adj2HPD.R()
 

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

# Remove loops in the graph
igraph.net <- simplify(igraph.net, remove.multiple = F, remove.loops = T)


# -- Network plots... loop layouts ----

# Set a network layout and plot:
graph_attr(igraph.net, "layout") <- layout_with_graphopt
plot(igraph.net, vertex.label = NA)
# With curved edges...
# plot(igraph.net, edge.curved = .1)

# Let’s take a look at all available layouts in igraph:
# layouts <- grep("^layout_", ls("package:igraph"), value = TRUE)[-1]

# Remove layouts that do not apply to our graph.
# layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

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
     vertex.label = NA,
     main = "Dolphin Social Network Colored by Community",
     sub = "Nodes = Dolphins. Communities detected with cluster_optimal()")



# --~~ Plot via igraph (without relying on community class's built-in plot) ----

# Start by assigning membership to each vertex/node
V(igraph.net)$community <- clp$membership

# Make a list & fill with a separate vector for each detected community
communities.l <- list()
for(i in unique(V(igraph.net)$community)){
  communities.l[[i]] <- as.vector(V(igraph.net)[community == i]$name)
}

# Create palettes. Assign to nodes.
colrs <- adjustcolor(brewer.pal(n = length(unique(V(igraph.net)$community))
                                , name = "Set1")
                     , alpha = .7)
V(igraph.net)$vertex.color <- colrs[V(igraph.net)$community]

# Make a lighter shade for marking areas
community.colrs <- adjustcolor(colrs, alpha = .2)

plot(
  igraph.net,
  layout = layout_with_fr, # layout_with_mds
  # layout = layout_with_graphopt, # Examine settings at: https://igraph.org/r/doc/layout_with_graphopt.html
  charge = 0.0001,
  max.sa.movement = 20,
  spring.length = 50,
  vertex.color = V(igraph.net)$vertex.color,
  vertex.frame.color = V(igraph.net)$vertex.color,
  edge.curved = 0.3,
  vertex.size = 5,
  vertex.label = NA,
  # vertex.label = V(igraph.net)$name, # Show names... useful for debugging
  mark.groups = communities.l,
  mark.col = community.colrs,
  mark.border = NA,
  main = "Dolphin Social Network Colored by Community",
  sub = "Nodes = Dolphins. Communities detected with cluster_optimal()"
)



# -- Circle Plot (circlize) ----

# https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html
# https://jokergoo.github.io/circlize_book/book/advanced-usage-of-chorddiagram.html

# Load library
library(circlize)

# create dataframe of edges w/ communities & colors
circle.e.df <- data.frame(as_edgelist(igraph.net))
colnames(circle.e.df) <- c("from", "to")

circle.e.df$from <- as.integer(as.character(circle.e.df$from))
circle.e.df$to <- as.integer(as.character(circle.e.df$to))

# create data frame of vertices w/ communities & colors
circle.v.df <- data.frame(id = as.integer(as.character(V(igraph.net)$name))
                       , community = as.integer(as.character(V(igraph.net)$community))
                       , vertex.color = as.character(V(igraph.net)$vertex.color)
                       )
circle.v.df$vertex.color <- as.character(circle.v.df$vertex.color) # Twice? Really?

# Sort by group, id so the circle plot makes sense (and looks good... very important, that)
circle.v.df <- circle.v.df[order(circle.v.df$community, circle.v.df$id),]

# join community & color on to the edge list, using from id
circle.e.df <- left_join(circle.e.df, circle.v.df, by = c("from" = "id"))
gridcolor <- circle.v.df$vertex.color

# Create circular plot from edge list
chordDiagram(circle.e.df[, 1:2]
             , transparency = 0.5
             , order = circle.v.df$id # set colors set before sorting data!!!
             , grid.col = as.vector(gridcolor)
             , directional = 0
             , annotationTrack = c("name", "grid")
             )
title("Circle Plot - Dolphin social networks colored by community")


# Chord diagram with rotated labels
chordDiagram(
  circle.e.df[, 1:2],
  transparency = 0.5, 
  order = circle.v.df$id, # set colors set before sorting data!!!
  grid.col = as.vector(gridcolor),
  annotationTrack = "grid",
  annotationTrackHeight = uh(5, "mm"),
  preAllocateTracks = list(track.height = max(strwidth(unlist(
    dimnames(circle.e.df[, 1:2])
  ))))
)
# we go back to the first track and customize sector labels
circos.track(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(
      CELL_META$xcenter,
      CELL_META$ylim[1],
      CELL_META$sector.index,
      facing = "clockwise",
      niceFacing = TRUE,
      adj = c(0, 0.5)
    )
  },
  bg.border = NA
) # here set bg.border to NA is important
title("Circle Plot - Dolphin social networks colored by community")


circos.clear()


# -- HiveR ----

require(rgl)

# Create a hive plot from the data frame
# Set edge color to community.color for start node
# Set node color to its community.color
# Put each community on a separate axis
hive1 <- mod.edge2HPD(edge_df = circle.e.df[, 1:2]
                      , edge.color = circle.e.df$vertex.color
                      , node.color = circle.v.df[, c(1,3)]
                      , node.axis = circle.v.df[, 1:2]
                      )
hive1$axis.cols <- "#888888"

# Make Community Labels
axLabs <- paste('Community', unique(circle.v.df$community), sep = " ")
circle.v.df$community <- as.integer(circle.v.df$community)

# data.frame(lab = hive1[["nodes"]][["lab"]], axis = hive1[["nodes"]][["axis"]])
# data.frame(lab = hive3[["nodes"]][["lab"]], axis = hive3[["nodes"]][["axis"]])
# hive1$edges

# --~~ source / man / sink Hive Plot ----

# Assign nodes to a radius based on their degree (number of edges they are touching)
hive2 <- mod.mineHPD(hive1, option = "rad <- tot.edge.count")

# Make this a source / man /sink plot
hive2 <- mod.mineHPD(hive2, option = "axis <- source.man.sink")

plotHive(hive2, method = "abs", bkgnd = "white"
         , axLabs = c("source", "hub", "sink")
         , axLab.gpar = gpar(col = brewer.pal(3, "Dark2"))
         , axLab.pos = 10
)


# --~~ Hive Plot : 1 axis per community ----

# Plot it
plotHive(hive1, method = "abs", bkgnd = "white"
         , axLabs = axLabs
         , axLab.gpar = gpar(col = unique(circle.v.df$vertex.color))
         , axLab.pos = 10
         )


# --~~ Hive Plot : 3D ----
hive3d <- hive1
hive3d$type = "3D"
plot3dHive(hive3d, method = "abs", bkgnd = "white"
         , axLabs = axLabs
         , axLab.gpar = gpar(col = unique(circle.v.df$vertex.color))
         , axLab.pos = 10
)

rm(hive1, hive2, hive3, hive4, hive3d)

##########

