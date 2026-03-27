b04_lc <- read.csv("Baker2004_Red_Sea.csv")
ctd_lc <- read.csv("coral_trait_database_red_sea.csv")
h20_lc <- read.csv("Hume2020_combined_data.csv")
z17_lc <- read.csv("Ziegler2017_cleaned_joined_data_red_sea.csv")

###

# add IDs 
b04_lc$ID <- "b04"
ctd_lc$ID <- "ctd"
h20_lc$ID <- "h20"
z17_lc$ID <- "z17"

b04_lc <- b04_lc[, c("ID", setdiff(names(b04_lc), "ID"))]
ctd_lc <- ctd_lc[, c("ID", setdiff(names(ctd_lc), "ID"))]
h20_lc <- h20_lc[, c("ID", setdiff(names(h20_lc), "ID"))]
z17_lc <- z17_lc[, c("ID", setdiff(names(z17_lc), "ID"))]

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
b04_lc <- expand_rows_by_count(b04_lc, "Number_of_Sampled_Colonies")
rownames(b04_lc) <- NULL
b04_lc <- b04_lc[-c(25, 26, 32, 33), ]

# ctd remove subclade data, remove species, and rename columns
ctd_lc <- ctd_lc[-2, ]
ctd_lc$specie_name <- sub(" .*", "", ctd_lc$specie_name)
colnames(ctd_lc)[colnames(ctd_lc) == "specie_name"] <- "Genus"
colnames(ctd_lc)[colnames(ctd_lc) == "value"] <- "Clade"
colnames(ctd_lc)[colnames(ctd_lc) == "location_name"] <- "Location"

# h20 rename columns
colnames(h20_lc)[colnames(h20_lc) == "species"] <- "Genus"
colnames(h20_lc)[colnames(h20_lc) == "clade"] <- "Clade"
colnames(h20_lc)[colnames(h20_lc) == "reef"] <- "Location"

# z17 remove data with no clade, rename leather coral, and rename columns
z17_lc <- z17_lc[-c(193), ]
z17_lc$Genus[z17_lc$Genus == "Leather Coral"] <- "Sarcophyton"
z17_lc$Genus[z17_lc$Genus == "Symphyllia"] <- "Lobophyllia"
z17_lc$Genus[z17_lc$Genus == "Tubinaria"] <- "Turbinaria"
colnames(z17_lc)[colnames(z17_lc) == "Major_Clade"] <- "Clade"
colnames(z17_lc)[colnames(z17_lc) == "Reef"] <- "Location"


### 

# select rows of each data frame
b04_lc <- b04_lc[, c("ID", "Location", "Genus", "Clade")]
ctd_lc <- ctd_lc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]
h20_lc <- h20_lc[, c("ID", "Location","Genus", "Clade")]
z17_lc <- z17_lc[, c("ID", "Location", "GPS", "Genus", "Clade")]

# list all locations (- ctd_lc)
b04_unique_locations <- unique(b04_lc$Location)
writeLines(b04_unique_locations, "b04_lc_unique_locations.txt")

h20_unique_locations <- unique(h20_lc$Location)
writeLines(h20_unique_locations, "h20_lc_unique_locations.txt")

z17_unique_locations <- unique(z17_lc$Location)
writeLines(z17_unique_locations, "z17_lc_unique_locations.txt")

### cleaning 2.0

# b04 add long and lat
coords_lookup_b04 <- data.frame(
  Location = c("Ras Suwaiyil", "Ras Baridi", "Little Barrier Reef"),
  latitude = c(28.671337, 24.258000, 25.580000),
  longitude = c(34.769468, 37.571000, 36.750000),
  stringsAsFactors = FALSE
)
b04_lc <- merge(b04_lc, coords_lookup_b04, by = "Location", all.x = TRUE)
b04_lc <- b04_lc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]

# ctd rename locations
ctd_lc$Location <- ifelse(
  ctd_lc$Location == "Gulf of Eliat, Red Sea",
  "Gulf of Aqaba",
  ctd_lc$Location
)
ctd_lc[1, 2:4] <- ctd_lc[3, 2:4]


# h20 add long and lat
coords_lookup_h20 <- data.frame(
  Location = c("Fsar", "Al Fahal", "Tahla", "Qita al Kirsh", "Shib Nazar", "Abu Madafi"),
  latitude = c(22.232620, 22.306233, 22.290333, 22.430717, 22.322533, 22.086817),
  longitude = c(39.030270, 38.960533, 39.054517, 38.992733, 38.854283, 38.778333),
  stringsAsFactors = FALSE
)

h20_lc <- merge(h20_lc, coords_lookup_h20, by = "Location", all.x = TRUE)
h20_lc <- h20_lc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]

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
lat_lon_all <- str_split_fixed(str_squish(z17_lc$GPS), " ", 2)
z17_lc$latitude <- sapply(lat_lon_all[,1], dms_to_decimal)
z17_lc$longitude <- sapply(lat_lon_all[,2], dms_to_decimal)

z17_lc <- z17_lc[, c("ID", "Location", "latitude", "longitude", "Genus", "Clade")]


###

combined_lc <- rbind(b04_lc, ctd_lc, h20_lc, z17_lc)

###

# map creation

# install.packages(c("ggplot2", "sf", "maps"))
library(ggplot2)
library(sf)
library(maps)

# Convert your data frame to an sf object
lc_sf <- st_as_sf(combined_lc, coords = c("longitude", "latitude"), crs = 4326)

# Get world map
world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# Plot
ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_sf(data = lc_sf, color = "blue", size = 2, alpha = 0.7) +
  theme_minimal() +
  labs(title = "Sample Locations in the Red Sea") +
  #coord_sf(
    #xlim = c(min(combined_lc$longitude) - 1, max(combined_lc$longitude) + 1),
    #ylim = c(min(combined_lc$latitude) - 1, max(combined_lc$latitude) + 1)
  #)
  coord_sf(
    xlim = c(32, 44),   
    ylim = c(12, 30)
  )

# pie map 

install.packages(c("ggplot2", "sf", "scatterpie", "maps"))

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(scatterpie)

# Aggregate Clade counts per location
clade_summary <- combined_lc %>%
  group_by(longitude, latitude) %>%
  count(Clade) %>%
  pivot_wider(names_from = Clade, values_from = n, values_fill = 0) %>%
  ungroup()

# Add coordinate columns explicitly (needed for scatterpie)
clade_summary <- clade_summary %>%
  mutate(x = longitude, y = latitude)

# Plot world map base
world <- st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

ggplot() +
  geom_sf(data = world, fill = "antiquewhite") +
  geom_scatterpie(
    aes(x = x, y = y),
    data = clade_summary,
    cols = c("A", "C", "D"),
    color = NA,
    alpha = 0.8,
    pie_scale = 4
  ) +
  coord_sf(xlim = c(32, 44), ylim = c(12, 30)) +
  theme_minimal() +
  labs(title = "Clade Composition by Location in the Red Sea") +
  theme(legend.position = "right")

# interactive pie map 

library(leaflet)
library(dplyr)
library(tidyr)

# Prepare summarized clade data per location
clade_summary <- combined_lc %>%
  group_by(longitude, latitude, Clade) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Clade, values_from = count, values_fill = 0)

# Create popup text for each clade (optional, can be customized)
clade_summary <- clade_summary %>%
  mutate(
    popup_A = paste0("<b>Location:</b> ", longitude, ", ", latitude, "<br><b>Clade A:</b> ", A),
    popup_C = paste0("<b>Location:</b> ", longitude, ", ", latitude, "<br><b>Clade C:</b> ", C),
    popup_D = paste0("<b>Location:</b> ", longitude, ", ", latitude, "<br><b>Clade D:</b> ", D)
  )

n <- leaflet(clade_summary) %>%
  addTiles() %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(A) * 3,
    popup = ~popup_A,
    group = "Clade A"
  ) %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(C) * 3,
    popup = ~popup_C,
    group = "Clade C"
  ) %>%
  
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = ~sqrt(D) * 3,
    popup = ~popup_D,
    group = "Clade D"
  ) %>%
  
  addLayersControl(
    overlayGroups = c("Clade A", "Clade C", "Clade D"),
    options = layersControlOptions(collapsed = FALSE)
  )

library(htmlwidgets)

saveWidget(n, "C:/Users/sophi/SynologyDrive/Documents/GitHub/coral_popgen/project_sophie/analysis/locations_clade/leaflet_map_locations_clades.html", selfcontained = TRUE)

