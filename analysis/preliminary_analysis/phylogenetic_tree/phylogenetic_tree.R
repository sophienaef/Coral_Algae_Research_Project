# install.packages(c("ape", "data.tree"))

library(ape)
library(data.tree)
library(dplyr)

taxonomy_list <- read.csv("taxonomy_list.csv")

taxonomy_list <- taxonomy_list %>%
  mutate(pathString = paste("Cnidaria", Subphylum, Class, Order, Family, Genus, sep = "/"))

###

tree_dt <- as.Node(taxonomy_list, pathName = "pathString")

phy <- as.phylo.Node(tree_dt)

pdf("phylogenetic_tree.pdf", width = 15, height = 12)

plot(
  phy,
  edge.width = 2,
  cex = 1.2,
  tip.color = "black",
  font = 3,
  show.node.label = FALSE,
  label.offset = 0.5
)

# Extract node coordinates from plot
last_plot <- get("last_plot.phylo", envir = .PlotPhyloEnv)
node_x <- last_plot$xx[(Ntip(phy) + 1):(Ntip(phy) + Nnode(phy))]
node_y <- last_plot$yy[(Ntip(phy) + 1):(Ntip(phy) + Nnode(phy))]

if(!is.null(phy$node.label)) {
  # Number of internal nodes
  n_nodes <- Nnode(phy)
  
  root_index <- 1
  
  x_positions <- node_x - 19 # shift internal nodes
  x_positions[root_index] <- node_x[root_index] - 8  # shift root label
  
  text(
    x = x_positions,
    y = node_y,
    labels = phy$node.label,
    cex = 1.2,
    adj = c(0, -0.3), 
    xpd = TRUE
  )
}

dev.off()
