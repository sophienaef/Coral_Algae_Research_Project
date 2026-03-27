b04_lsc <- read.csv("Baker2004_Red_Sea.csv")
ctd_lsc <- read.csv("coral_trait_database_red_sea.csv")
h20_lsc <- read.csv("Hume2020_combined_data.csv")
z17_lsc <- read.csv("Ziegler2017_cleaned_joined_data_red_sea.csv")

###

# add IDs 
b04_lsc$ID <- "b04"
ctd_lsc$ID <- "ctd"
h20_lsc$ID <- "h20"
z17_lsc$ID <- "z17"

b04_lsc <- b04_lsc[, c("ID", setdiff(names(b04_lsc), "ID"))]
ctd_lsc <- ctd_lsc[, c("ID", setdiff(names(ctd_lsc), "ID"))]
h20_lsc <- h20_lsc[, c("ID", setdiff(names(h20_lsc), "ID"))]
z17_lsc <- z17_lsc[, c("ID", setdiff(names(z17_lsc), "ID"))]

### cleaning 1.0

# b04 expand data frame and remove data with unknown genera
expand_rows_by_count <- function(df, count_col) {
  if (!count_col %in% colnames(df)) {
    stop(paste("Column", count_col, "not found in data frame"))
  }
  if (!is.numeric(df[[count_col]])) {
    stop(paste("Column", count_col, "must be numeric"))
  }
  expanded_df <- df[rep(1:nrow(df), df[[count_col]]), ]
  expanded_df[[count_col]] <- NULL
  return(expanded_df)
}
b04_lsc <- expand_rows_by_count(b04_lsc, "Number_of_Sampled_Colonies")
rownames(b04_lsc) <- NULL
b04_lsc <- b04_lsc[-c(25, 26, 32, 33), ]

# ctd remove subclade data, remove species, and rename columns
ctd_lsc <- ctd_lsc[-2, ]
ctd_lsc$specie_name <- sub(" .*", "", ctd_lsc$specie_name)
colnames(ctd_lsc)[colnames(ctd_lsc) == "specie_name"] <- "Genus"
colnames(ctd_lsc)[colnames(ctd_lsc) == "value"] <- "Clade"
colnames(ctd_lsc)[colnames(ctd_lsc) == "location_name"] <- "Location"

# h20 rename columns
colnames(h20_lsc)[colnames(h20_lsc) == "species"] <- "Genus"
colnames(h20_lsc)[colnames(h20_lsc) == "clade"] <- "Clade"
colnames(h20_lsc)[colnames(h20_lsc) == "reef"] <- "Location"

# z17 remove data with no clade, rename leather coral, and rename columns
z17_lsc <- z17_lsc[-c(193), ]
z17_lsc$Genus[z17_lsc$Genus == "Leather Coral"] <- "Sarcophyton"
z17_lsc$Genus[z17_lsc$Genus == "Symphyllia"] <- "Lobophyllia"
z17_lsc$Genus[z17_lsc$Genus == "Tubinaria"] <- "Turbinaria"
colnames(z17_lsc)[colnames(z17_lsc) == "Major_Clade"] <- "Clade"
colnames(z17_lsc)[colnames(z17_lsc) == "Reef"] <- "Location"


### 

# select rows of each data frame
b04_lsc <- b04_lsc[, c("ID", "Location", "Genus", "Clade")]
ctd_lsc <- ctd_lsc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]
h20_lsc <- h20_lsc[, c("ID", "Location","Genus", "Clade")]
z17_lsc <- z17_lsc[, c("ID", "Location", "GPS", "Genus", "Clade")]

### cleaning 2.0

# b04 add long and lat
coords_lookup_b04 <- data.frame(
  Location = c("Ras Suwaiyil", "Ras Baridi", "Little Barrier Reef"),
  latitude = c(28.671337, 24.258000, 25.580000),
  longitude = c(34.769468, 37.571000, 36.750000),
  stringsAsFactors = FALSE
)
b04_lsc <- merge(b04_lsc, coords_lookup_b04, by = "Location", all.x = TRUE)
b04_lsc <- b04_lsc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]

# ctd rename locations
ctd_lsc$Location <- ifelse(
  ctd_lsc$Location == "Gulf of Eliat, Red Sea",
  "Gulf of Aqaba",
  ctd_lsc$Location
)
ctd_lsc[1, 2:4] <- ctd_lsc[3, 2:4]


# h20 add long and lat
coords_lookup_h20 <- data.frame(
  Location = c("Fsar", "Al Fahal", "Tahla", "Qita al Kirsh", "Shib Nazar", "Abu Madafi"),
  latitude = c(22.232620, 22.306233, 22.290333, 22.430717, 22.322533, 22.086817),
  longitude = c(39.030270, 38.960533, 39.054517, 38.992733, 38.854283, 38.778333),
  stringsAsFactors = FALSE
)

h20_lsc <- merge(h20_lsc, coords_lookup_h20, by = "Location", all.x = TRUE)
h20_lsc <- h20_lsc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]

# z17 convert GPS and clean columns
library(stringr)
dms_to_decimal <- function(dms) {
  dms <- gsub(" ", "", dms)  # Remove spaces
  pattern <- "^([0-9]{1,3})°([0-9]{1,2})'([0-9.]+)\"?([NSEW])$"
  matches <- regmatches(dms, regexec(pattern, dms))
  
  if(length(matches[[1]]) == 5) {
    degrees <- as.numeric(matches[[1]][2])
    minutes <- as.numeric(matches[[1]][3])
    seconds <- as.numeric(matches[[1]][4])
    direction <- matches[[1]][5]
    
    decimal <- degrees + minutes / 60 + seconds / 3600
    
    if(direction %in% c("S", "W")) {
      decimal <- -decimal
    }
    return(decimal)
  } else {
    warning(paste("Failed to parse:", dms))
    return(NA)
  }
}
lat_lon_all <- str_split_fixed(str_squish(z17_lsc$GPS), " ", 2)
z17_lsc$latitude <- sapply(lat_lon_all[,1], dms_to_decimal)
z17_lsc$longitude <- sapply(lat_lon_all[,2], dms_to_decimal)

z17_lsc <- z17_lsc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]


###

combined_lsc <- rbind(b04_lsc, ctd_lsc, h20_lsc, z17_lsc)

# split data by genus
genus_list <- split(combined_lsc, combined_lsc$Genus)

# filter genus with too little data
filtered_data <- combined_lsc %>%
  filter(!Genus %in% c("Sclerophytum", "Milleporidae")) %>%  # exclude unwanted genera
  group_by(Genus) %>%
  filter(n() > 2) %>%  # keep genera with more than 2 rows
  ungroup()
genus_list <- split(filtered_data, filtered_data$Genus)

### mao creation

# install.packages("patchwork")  

library(sf)
library(dplyr)
library(igraph)
library(ggplot2)
library(ggforce)
library(maps)
library(patchwork)

world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Custom clade colors
clade_colors <- c(
  "A" = "lightpink1",
  "C" = "lightgoldenrod2",
  "D" = "lightblue2"
)

# Function to plot map per genus with clustered pie charts
plot_genus_map <- function(genus_data, genus_name) {
  
  points_sf <- st_as_sf(genus_data, coords = c("longitude", "latitude"), crs = 4326)
  points_sf_proj <- st_transform(points_sf, 32636)
  distance_threshold <- 60000
  neighbors <- st_is_within_distance(points_sf_proj, dist = distance_threshold)
  
  # Build edge list for igraph clustering
  edges <- do.call(rbind, lapply(seq_along(neighbors), function(i) {
    if(length(neighbors[[i]]) > 0) {
      cbind(i, neighbors[[i]])
    }
  }))
  
  graph <- igraph::graph_from_edgelist(edges, directed = FALSE)
  clusters <- igraph::clusters(graph)$membership
  points_sf$cluster_id <- clusters
  
  # Aggregate clade counts per cluster (no geometry here)
  cluster_summary <- points_sf %>%
    st_drop_geometry() %>%
    group_by(cluster_id, Clade) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(cluster_id) %>%
    mutate(prop = count / sum(count)) %>%
    ungroup()
  
  # Calculate cluster centroids (sf with geometry and cluster_id)
  cluster_centroids <- points_sf %>%
    group_by(cluster_id) %>%
    summarise(geometry = st_centroid(st_union(geometry))) %>%
    ungroup()
  
  # Join cluster_summary (attributes) to cluster_centroids (sf with geometry)
  cluster_data <- cluster_centroids %>%
    left_join(cluster_summary, by = "cluster_id")
  
  # Prepare pie chart angles (group by cluster_id)
  cluster_data <- cluster_data %>%
    arrange(Clade) %>%
    group_by(cluster_id) %>%
    mutate(
      start = cumsum(lag(prop, default = 0)) * 2 * pi,
      end = cumsum(prop) * 2 * pi
    ) %>%
    ungroup()
  
  # Extract coordinates for plotting pies
  coords <- st_coordinates(cluster_data)
  cluster_data$x <- coords[,1]
  cluster_data$y <- coords[,2]
  
  # Plot map with pie charts
  p <- ggplot() +
    geom_sf(data = world, fill = "antiquewhite") +
    coord_sf(xlim = c(32, 44), ylim = c(18, 30)) +
    geom_arc_bar(
      data = cluster_data,
      aes(
        x0 = x, y0 = y,
        r0 = 0, r = 0.5,
        start = start, end = end,
        fill = Clade
      ),
      color = "black", size = 0.2
    ) +
    scale_fill_manual(values = clade_colors) +
    theme_minimal() +
    labs(title = paste("Clade Distribution for Genus:", genus_name)) +
    theme(legend.position = "right")
  
  return(p)
}

# Your desired order and page setup
desired_order <- c(
  "Favia", "Fungia", "Lobophytum", "Pavona", "Sinularia", "Turbinaria",
  "Astreopora", "Diploastrea", "Leptoria", "Porites", "Xenia",
  "Acropora", "Echinopora", "Goniastrea", "Sarcophyton",
  "Galaxea", "Gardineroseris", "Montipora", "Pocillopora", "Seriatopora", "Stylophora"
)

maps_per_page_vec <- c(6, 5, 4, 6)

page_filenames <- c("Map_No_Change", "Map_C_Increase", "Map_C_D_Random", "Map_A_C_D_Random")

page_titles <- c(
  "No change in clade distributions",
  "Increase in D clade toward South",
  "C and D Clade no pattern",
  "All three Clades no pattern"
)

# Build map list in desired order
map_list <- list()
for (genus_name in desired_order) {
  if (genus_name %in% names(genus_list)) {
    genus_data <- genus_list[[genus_name]]
    p <- plot_genus_map(genus_data, genus_name)
    map_list[[genus_name]] <- p
  } else {
    warning(paste("Genus", genus_name, "not found in genus_list"))
  }
}

# Save maps per page with custom layout and titles
start_idx <- 1

for (page in seq_along(maps_per_page_vec)) {
  n_maps <- maps_per_page_vec[page]
  end_idx <- start_idx + n_maps - 1
  
  maps_to_plot <- map_list[start_idx:end_idx]
  
  nrow <- 2
  ncol <- ceiling(n_maps / nrow)
  
  combined_plot <- wrap_plots(maps_to_plot, nrow = nrow, ncol = ncol) +
    plot_annotation(title = page_titles[page])
  
  ggsave(
    filename = paste0(page_filenames[page], ".png"),
    plot = combined_plot,
    width = 5 * ncol,
    height = 5 * nrow,
    dpi = 300
  )
  
  start_idx <- end_idx + 1
}

