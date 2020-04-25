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
library(scales)
library(RColorBrewer)
# library(ggraph)

# --------- Theme, scales, etc ---------------

# theme_set(theme_light())
# light.grey <- "#dddddd"
# light.grey.line <- element_line(size = 0.3, color = light.grey)
# theme.base <- theme(panel.border = element_blank()
#                     , panel.grid.major = element_blank()
#                     , panel.grid.minor = element_blank()
#                     , axis.ticks.x = element_blank()
#                     , axis.ticks.y = element_blank()
# )
# palette25 <- colorRampPalette(brewer.pal(8, "Dark2"))(25)
# palette2 <- c("#66C2A5", "#FC8D62")
# palette2rev <- c("#FC8D62", "#66C2A5")

options(scipen = 999)

# --------- Load Text file, clean up, set options. ----------

# http://networkrepository.com/soc-dolphins.php
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
class(igraph.net)
# E(igraph.net)         # The edges of the "net" object
# V(igraph.net)         # The vertices of the "net" object
# E(igraph.net)$weight  # Edge attribute "weight"

plot(igraph.net)
# Remove loops in the graph
igraph.net <- simplify(igraph.net, remove.multiple = F, remove.loops = T)
plot(igraph.net)

# Remove labels by setting them to NA
# plot(igraph.net, vertex.label = NA)

# Plot with curved edges (edge.curved=.1)
plot(igraph.net, edge.curved = .1)

# Set edge color to light gray, the node & border color to orange 
plot(
  igraph.net,
  edge.color = "orange",
  vertex.color = "orange",
  vertex.frame.color = "#ffffff",
  vertex.label.color = "black"
)

# Compute node degrees (#links) and use that to set node size:
# deg <- degree(igraph.net, mode = "all")
# V(igraph.net)$size <- deg / 3

# The labels are currently node IDs.
# Setting them to NA will render no labels:
# V(igraph.net)$label <- NA

# Set edge width based on weight
# E(igraph.net)$width <- E(igraph.net)$weight/10
# E(igraph.net)$width <- E(igraph.net)$weight
E(igraph.net)$width <- rescale(E(igraph.net)$weight)

# change edge color
E(igraph.net)$edge.color <- "gray80"

# Set the network layout:
graph_attr(igraph.net, "layout") <- layout_with_lgl
plot(igraph.net, vertex.label = NA)

plot(igraph.net,
     edge.color = "orange",
     vertex.color = "gray50")

# Random layout
plot(igraph.net, layout = layout_randomly, vertex.label = NA)

# -- Calculate vertex coordinates in advance --

# Circle layout
l <- layout_in_circle(igraph.net)
plot(igraph.net, layout = l, vertex.label = NA)

# 3D sphere layout
l <- layout_on_sphere(igraph.net)
plot(igraph.net, layout = l, vertex.label = NA)

# Fruchterman-Reingold sphere layout
l <- layout_with_fr(igraph.net)
plot(igraph.net, layout = l, vertex.size = 5, vertex.label = NA)

# 3D option with Fruchterman-Reingold
l <- layout_with_fr(igraph.net)
plot(igraph.net, dim = 3, layout = l, vertex.size = 5, vertex.label = NA)

# Layout can interpret edge weights. Set the “weights” parameter which increases
# the attraction forces among nodes connected by heavier edges.
lw <- layout_with_fr(igraph.net, weights = E(igraph.net)$weight)
plot(igraph.net, layout = lw, vertex.size = 5, vertex.label = NA)

# Kamada Kawai
l <- layout_with_kk(igraph.net)
plot(
  igraph.net,
  layout = l,
  vertex.size = 5,
  vertex.label = NA
)

# Graphopt
l <- layout_with_graphopt(igraph.net)
plot(
  igraph.net,
  layout = l,
  vertex.size = 5,
  vertex.label = NA
)

# The charge parameter below changes node repulsion:
l1 <- layout_with_graphopt(igraph.net, charge = 0.02)
l2 <- layout_with_graphopt(igraph.net, charge = 0.000001)

par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
plot(igraph.net, layout = l1, vertex.size = 5, vertex.label = NA)
plot(igraph.net, layout = l2, vertex.size = 5, vertex.label = NA)
dev.off()

# MDS (multidimensional scaling) algorithm
plot(
  igraph.net,
  layout = layout_with_mds,
  vertex.size = 5,
  vertex.label = NA
)

# Let’s take a look at all available layouts in igraph:
layouts <- grep("^layout_", ls("package:igraph"), value = TRUE)[-1]

# Remove layouts that do not apply to our graph.
layouts <-
  layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(igraph.net))
  plot(
    igraph.net,
    edge.arrow.mode = 0,
    layout = l,
    main = layout
  )
}


# hist(E(igraph.net)$weight, breaks = 25)
hist(E(igraph.net)$weight)
mean(E(igraph.net)$weight)
sd(E(igraph.net)$weight)

# Keep edges with weight > mean
igraph.net.sp <-
  # delete_edges(igraph.net, E(igraph.net)[weight < 2])
  delete_edges(igraph.net, E(igraph.net)[weight < mean(E(igraph.net)$weight)])
plot(
  igraph.net.sp,
  layout = layout_with_kk,
  vertex.size = 5,
  vertex.label = NA
)


# -- Community Detection -------

# Community detection (by optimizing modularity over partitions):
clp <- cluster_optimal(igraph.net)
class(clp)

# Community detection returns an object of class "communities"
# which igraph knows how to plot:
plot(clp,
     igraph.net,
     layout = layout_with_fr,
     vertex.size = 5,
     vertex.label = NA)

# We can also plot the communities without relying on their built-in plot:
V(igraph.net)$community <- clp$membership
# colrs <- adjustcolor(c("gray50", "tomato", "gold", "yellowgreen"), alpha = .6)
colrs <- brewer.pal(n = length(unique(V(igraph.net)$community)), name = "Set2")
plot(
  igraph.net,
  # layout = layout_with_fr,
  layout = layout_with_graphopt,
  vertex.color = colrs[V(igraph.net)$community],
  # vertex.size = 5,
  vertex.label = NA
)



# -- 4.4 Highlighting specific nodes or links ----------
# The distances function returns a matrix of shortest paths from nodes listed
# in the v parameter to ones included in the to parameter.
# Thornbill: Node 6 has the highest degree (links), so let's focus on that
# Eco: Node 66 has the highest degree
# Dolphins: Node 15 has the highest degree
dist.from.node <-
  distances(
    igraph.net,
    # v = V(igraph.net)[name == "6"],
    # v = V(igraph.net)[name == "66"],
    v = V(igraph.net)[name == "15"],
    to = V(igraph.net),
    weights = NA
  )

# Set colors to plot the distances:
oranges <- colorRampPalette(c("dark red", "gold"))
col <- oranges(max(dist.from.node) + 1)
col <- col[dist.from.node + 1]

plot(
  igraph.net,
  vertex.color = col,
  vertex.label = dist.from.node,
  vertex.label.color = "white",
  vertex.size = 10
)


# -- Highlight a path in the network ----------
# Thornbill: Nodes 11 & 56 are tied for 2nd-highest degree
# Eco: Node 64 has 2nd-highest degree
# Dolphins: Nodes 38 & 46 are tied for 2nd-highest degree
igraph.path <- shortest_paths(igraph.net
                                 # , from = V(igraph.net)[name == '56']
                                 # , from = V(igraph.net)[name == '64']
                                 , from = V(igraph.net)[name == '38']
                                 # , to = V(igraph.net)[name == '6']
                                 # , to = V(igraph.net)[name == '66']
                                 , to = V(igraph.net)[name == '15']
                                 , output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(igraph.net))
ecol[unlist(igraph.path$epath)] <- "orange"
# Generate edge width variable to plot the path:
ew <- rep(2, ecount(igraph.net))
ew[unlist(igraph.path$epath)] <- 4
# Generate node color variable to plot the path:
vcol <- rep("gray40", vcount(igraph.net))
vcol[unlist(igraph.path$vpath)] <- "gold"

plot(
  igraph.net,
  vertex.color = vcol,
  edge.color = ecol,
  edge.width = ew,
  edge.arrow.mode = 0,
  vertex.size = 5#, vertex.label = NA
)

# We can highlight the edges going into or out of a vertex.
# For a single node, use incident(), for multiple nodes use incident_edges()
# vertices.v = c(6, 11, 56) # Thornbill
# vertices.v = c(66) # Eco
vertices.v = c(15, 38, 46) # Dolphins
inc.edges <-
  # incident(igraph.net, V(igraph.net)[name == "6"], mode = "all")
  incident_edges(igraph.net, V(igraph.net)[name %in% vertices.v], mode = "all")

# Set colors to plot the selected edges.
ecol <- rep("gray80", ecount(igraph.net))
ecol[names(inc.edges)] <- "orange"
vcol <- rep("grey40", vcount(igraph.net))
# vcol[V(igraph.net)$name == "6"] <- "gold"
vcol[V(igraph.net)[name %in% vertices.v]] <- "gold"
plot(
  igraph.net,
  vertex.color = vcol,
  edge.color = ecol,
  edge.width = 1,
  vertex.size = 7
)


# Point to immediate neighbors of a vertex.
# neighbors function finds all nodes one step out from focal actor.
# To find the neighbors for multiple nodes, use adjacent_vertices().
neigh.nodes <-
  # neighbors(igraph.net, V(igraph.net)[name == "6"], mode = "out") # Thornbill
  # neighbors(igraph.net, V(igraph.net)[name == "66"], mode = "out") # Eco
  neighbors(igraph.net, V(igraph.net)[name == "15"], mode = "out") # Dolphins

# Set colors to plot the neighbors:
vcol[neigh.nodes] <- "#ff9d00"
plot(
  igraph.net,
  # layout = layout_in_circle,
  vertex.color = vcol,
  vertex.size = 5,
  vertex.label = NA
)





max(degree(igraph.net, mode = "all"))
V(igraph.net)[name == "6"]$degree

E(igraph.net)$weight
V(igraph.net)$size




# Graphviz at data source site...
# http://networkrepository.com/graphvis.php?d=./data/gsm50/soc/soc-dolphins.mtx#panel-body
# ^^^^^^^^^ try this again ^^^^^^^

# -- ggraph visualizations ---------
set.seed(2020)
ggraph(dolphin_network, layout = "fr") +
  # geom_edge_link(alpha = 0.2, aes(width = value)) +
  geom_edge_link(alpha = 0.2) +
  # geom_node_point(aes(color = as.factor(group), size = 10 * nodesize)) +
  geom_node_point(aes(size = 10)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none")


ggraph(subset(everglades.df, value >= 0.05), layout = "fr") +
  geom_edge_link(alpha = 0.2, aes(width = value)) +
  # geom_edge_link(alpha = 0.2) +
  # geom_node_point(aes(color = as.factor(group), size = 10 * nodesize)) +
  geom_node_point(aes(size = 10)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none")

ggraph(subset(thornbill.df, value > 4), layout = "fr") +
  geom_edge_link() +
  geom_node_point(aes(size = 5)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position = "none")




##########
