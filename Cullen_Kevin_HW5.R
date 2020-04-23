#########
# CIS576 HW5
# Kevin Cullen
# Network Visualization
setwd("~/Projects/cis576/HW5")

# library()  # all statements needed to load libs
# library(tidyverse)
# library(RColorBrewer)
# library(reshape)
library(Matrix)

# --------- Theme, scales, etc

theme_set(theme_light())
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

# http://networkrepository.com/eco-everglades.php
everglades.df <- read.csv("data/eco-everglades.edges", header = FALSE, sep = " ")
colnames(everglades.df) <- c("start", "end", "value")

# build igraph object from CSV files
dolphin_network <- graph_from_data_frame(dolphins.df, directed = FALSE
                                         , vertices = union(dolphins.df$i, dolphins.df$j))

everglades.net <- graph_from_data_frame(everglades.df
                                        , vertices = union(everglades.df$start, everglades.df$end))

dolphins.df %>% group_by(i, j) %>% tally()
  # Find totals by CMTE_ID
  # san_check.df <- donate.df %>%
  #   group_by(CMTE_ID, CAND_ID, CAND_NAME) %>%
  #   summarise(CASH_FOR_VOTES = sum(TRANSACTION_AMT))
  # san_check.df$CASH_FOR_VOTES = scales::dollar(san_check.df$CASH_FOR_VOTES)

# visualize graph
set.seed(2020)
ggraph(dolphin_network, layout = "fr") +
  # geom_edge_link(alpha = 0.2, aes(width = value)) +
  geom_edge_link(alpha = 0.2) +
  # geom_node_point(aes(color = as.factor(group), size = 10 * nodesize)) +
  geom_node_point(aes(size = 10)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position="none")


ggraph(everglades.net, layout = "fr") +
  geom_edge_link(alpha = 0.2, aes(width = value)) +
  # geom_edge_link(alpha = 0.2) +
  # geom_node_point(aes(color = as.factor(group), size = 10 * nodesize)) +
  geom_node_point(aes(size = 10)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  theme(legend.position="none")


# -- Question --------------------------


# -- Question -------------------------


# -- Generate Tableau File(s) --------------------------
# write.csv(frequencies.df[1:100,], file = "frequencies.csv", row.names = FALSE)


##########
