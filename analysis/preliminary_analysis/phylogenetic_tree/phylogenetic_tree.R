# install.packages(c("ape", "data.tree"))

library(ape)
library(data.tree)
library(dplyr)

# Read taxonomy list CSV
taxonomy_list <- read.csv("taxonomy_list.csv")

# Combine all taxonomic levels into a path string
taxonomy_list <- taxonomy_list %>%
  mutate(pathString = paste("Cnidaria", Subphylum, Class, Order, Family, Genus, sep = "/"))

# Build data.tree structure
tree_dt <- as.Node(taxonomy_list, pathName = "pathString")

# Convert to phylo object
phy <- as.phylo.Node(tree_dt)

# Save tree to PDF
pdf("phylogenetic_tree.pdf", width = 15, height = 10)

# Plot base tree without node labels
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
  
  # Root node is usually the first internal node
  root_index <- 1
  
  # Prepare x positions: shift root less far, others more
  x_positions <- node_x - 19         # shift all internal nodes by 18 units left
  x_positions[root_index] <- node_x[root_index] - 7  # shift root label only 6 units left
  
  # Add internal node labels with custom shifts
  text(
    x = x_positions,
    y = node_y,
    labels = phy$node.label,
    cex = 1.2,
    adj = c(0, -0.4),        # left-align horizontally, slightly below vertical center
    xpd = TRUE
  )
}

dev.off()
