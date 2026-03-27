b04_sl <- read.csv("Baker2004_Red_Sea.csv")
ctd_sl <- read.csv("coral_trait_database_red_sea.csv")
h20_sl <- read.csv("Hume2020_combined_data.csv")
z17_sl <- read.csv("Ziegler2017_cleaned_joined_data_red_sea.csv")

###

# add IDs 
b04_sl$ID <- "b04"
ctd_sl$ID <- "ctd"
h20_sl$ID <- "h20"
z17_sl$ID <- "z17"

b04_sl <- b04_sl[, c("ID", setdiff(names(b04_sl), "ID"))]
ctd_sl <- ctd_sl[, c("ID", setdiff(names(ctd_sl), "ID"))]
h20_sl <- h20_sl[, c("ID", setdiff(names(h20_sl), "ID"))]
z17_sl <- z17_sl[, c("ID", setdiff(names(z17_sl), "ID"))]

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
b04_sl <- expand_rows_by_count(b04_sl, "Number_of_Sampled_Colonies")
rownames(b04_sl) <- NULL
b04_sl <- b04_sl[-c(25, 26, 32, 33), ]

b04_sl$Date <- as.character(b04_sl$Date)
b04_sl$Date[b04_sl$Date == "2.2"] <- "2000"
b04_sl$Date[b04_sl$Date == "7.2001"] <- "2001"

# ctd remove subclade data, remove species, and rename columns, add date
ctd_sl <- ctd_sl[-c(1,2), ]
ctd_sl$specie_name <- sub(" .*", "", ctd_sl$specie_name)
colnames(ctd_sl)[colnames(ctd_sl) == "specie_name"] <- "Genus"
colnames(ctd_sl)[colnames(ctd_sl) == "value"] <- "Clade"
colnames(ctd_sl)[colnames(ctd_sl) == "location_name"] <- "Location"
ctd_sl$Date <- "2003"

# h20 rename columns and add date
colnames(h20_sl)[colnames(h20_sl) == "species"] <- "Genus"
colnames(h20_sl)[colnames(h20_sl) == "clade"] <- "Clade"
colnames(h20_sl)[colnames(h20_sl) == "reef"] <- "Location"
h20_sl$Date <- "2017"
h20_sl$Location[h20_sl$Location == "Shib Nazar"] <- "Shib Nazar S"
h20_sl$Location[h20_sl$Location == "Al Fahal"] <- "Al Fahal N"

# z17 remove data with no clade, rename leather coral, and rename columns, add dates
z17_sl <- z17_sl[-c(193), ]
z17_sl$Genus[z17_sl$Genus == "Leather Coral"] <- "Sarcophyton"
z17_sl$Genus[z17_sl$Genus == "Symphyllia"] <- "Lobophyllia"
z17_sl$Genus[z17_sl$Genus == "Tubinaria"] <- "Turbinaria"
colnames(z17_sl)[colnames(z17_sl) == "Major_Clade"] <- "Clade"
colnames(z17_sl)[colnames(z17_sl) == "Reef"] <- "Location"
colnames(z17_sl)[colnames(z17_sl) == "Date_of_collection"] <- "Date"
z17_sl$Date <- as.character(z17_sl$Date)
z17_sl$Date <- sub(".*\\.(\\d{4})$", "\\1", z17_sl$Date)
z17_sl$Date <- as.numeric(z17_sl$Date)
z17_sl$Location[z17_sl$Location == "Shib Nazar"] <- "Shib Nazar N"
z17_sl$Location[z17_sl$Location == "Al Fahal"] <- "Al Fahal S"
z17_sl$Location[z17_sl$Location == "Abu lath shallow reef"] <- "Abu Lath Shallow Reef"
z17_sl$Location[z17_sl$Location == "Al lith South Reef"] <- "Al Lith South Reef"



### 

# select rows of each data frame
b04_sl <- b04_sl[, c("ID", "Location", "Date", "Genus", "Clade")]
ctd_sl <- ctd_sl[, c("ID", "Location", "Date", "latitude", "longitude", "Genus", "Clade")]
h20_sl <- h20_sl[, c("ID", "Location", "Date", "Genus", "Clade")]
z17_sl <- z17_sl[, c("ID", "Location", "Date", "GPS", "Genus", "Clade")]

### cleaning 2.0

# b04 add long and lat
coords_lookup_b04 <- data.frame(
  Location = c("Ras Suwaiyil", "Ras Baridi", "Little Barrier Reef"),
  latitude = c(28.671337, 24.258000, 25.580000),
  longitude = c(34.769468, 37.571000, 36.750000),
  stringsAsFactors = FALSE
)
b04_sl <- merge(b04_sl, coords_lookup_b04, by = "Location", all.x = TRUE)
b04_sl <- b04_sl[, c("ID", "Location", "Date", "latitude", "longitude", "Genus", "Clade")]

# ctd rename locations
ctd_sl$Location <- ifelse(
  ctd_sl$Location == "Gulf of Eliat, Red Sea",
  "Gulf of Aqaba",
  ctd_sl$Location
)
ctd_sl[1, 2:4] <- ctd_sl[3, 2:4]


# h20 add long and lat
coords_lookup_h20 <- data.frame(
  Location = c("Fsar", "Al Fahal N", "Tahla", "Qita al Kirsh", "Shib Nazar S", "Abu Madafi"),
  latitude = c(22.232620, 22.306233, 22.290333, 22.430717, 22.322533, 22.086817),
  longitude = c(39.030270, 38.960533, 39.054517, 38.992733, 38.854283, 38.778333),
  stringsAsFactors = FALSE
)

h20_sl <- merge(h20_sl, coords_lookup_h20, by = "Location", all.x = TRUE)
h20_sl <- h20_sl[, c("ID", "Location", "Date", "latitude", "longitude", "Genus", "Clade")]

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
lat_lon_all <- str_split_fixed(str_squish(z17_sl$GPS), " ", 2)
z17_sl$latitude <- sapply(lat_lon_all[,1], dms_to_decimal)
z17_sl$longitude <- sapply(lat_lon_all[,2], dms_to_decimal)

z17_sl <- z17_sl[, c("ID", "Location", "Date", "latitude", "longitude", "Genus", "Clade")]


###

combined_sl <- rbind(b04_sl, ctd_sl, h20_sl, z17_sl)
colnames(combined_sl)[colnames(combined_sl) == "latitude"] <- "Latitude"
colnames(combined_sl)[colnames(combined_sl) == "longitude"] <- "Longitude"

### point overview 

most_northern_point <- combined_sl[which.max(combined_sl$Latitude), ]
most_southern_point <- combined_sl[which.min(combined_sl$Latitude), ]
print(most_northern_point)
print(most_southern_point)

# all unique locations
all_unique_locations <- combined_sl[!duplicated(combined_sl[, c("Latitude", "Longitude")]), ]
all_unique_locations <- all_unique_locations[order(-all_unique_locations$Latitude), ]
cols_to_remove <- c("Clade", "Genus")
existing_cols <- intersect(cols_to_remove, colnames(all_unique_locations))
all_unique_locations <- all_unique_locations[, !(colnames(all_unique_locations) %in% existing_cols)]
write.csv(all_unique_locations, "all_unique_locations.csv", row.names = FALSE)


# map just to see spread visually
library(ggplot2)
library(maps)       
world <- map_data("world")
red_sea_bbox <- list(
  lon_min = 32,
  lon_max = 44,
  lat_min = 12,
  lat_max = 30
)
red_sea_map <- subset(world,
                      long >= red_sea_bbox$lon_min & long <= red_sea_bbox$lon_max &
                      lat >= red_sea_bbox$lat_min & lat <= red_sea_bbox$lat_max)
p1 <- ggplot() +
  geom_polygon(data = red_sea_map, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "black") +
  geom_point(data = all_unique_locations, aes(x = Longitude, y = Latitude),
             color = "red", size = 2) +
  coord_quickmap(xlim = c(red_sea_bbox$lon_min, red_sea_bbox$lon_max),
                 ylim = c(red_sea_bbox$lat_min, red_sea_bbox$lat_max)) +
  labs(title = "Points near the Red Sea",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
print(p1)


### sst 

# import

avg_sst_2000 <- read.csv("../import_sst_data/avg_sst_per_year_red_sea_2000.csv")
avg_sst_2001 <- read.csv("../import_sst_data/avg_sst_per_year_red_sea_2001.csv")
avg_sst_2003 <- read.csv("../import_sst_data/avg_sst_per_year_red_sea_2003.csv")
avg_sst_2014 <- read.csv("../import_sst_data/avg_sst_per_year_red_sea_2014.csv")
avg_sst_2017 <- read.csv("../import_sst_data/avg_sst_per_year_red_sea_2017.csv")

max_sst_2000 <- read.csv("../import_sst_data/max_sst_per_year_red_sea_2000.csv")
max_sst_2001 <- read.csv("../import_sst_data/max_sst_per_year_red_sea_2001.csv")
max_sst_2003 <- read.csv("../import_sst_data/max_sst_per_year_red_sea_2003.csv")
max_sst_2014 <- read.csv("../import_sst_data/max_sst_per_year_red_sea_2014.csv")
max_sst_2017 <- read.csv("../import_sst_data/max_sst_per_year_red_sea_2017.csv")

# enrich with sst data
# install.packages("FNN")
library(dplyr)
library(FNN)  

# Helper function to find nearest SST for one row
get_nearest_sst <- function(lat, lon, year, avg_list, max_list) {
  avg_df <- avg_list[[as.character(year)]]
  max_df <- max_list[[as.character(year)]]
  
  if (is.null(avg_df) | is.null(max_df)) {
    return(c(NA, NA))
  }
  
  # Prepare matrix of SST points (lon, lat)
  points_avg <- as.matrix(avg_df[, c("lon", "lat")])
  points_max <- as.matrix(max_df[, c("lon", "lat")])
  
  # Query point
  query_point <- matrix(c(lon, lat), nrow = 1)
  
  # Find nearest neighbor index for avg_sst
  nn_avg <- get.knnx(points_avg, query_point, k = 1)$nn.index[1,1]
  nearest_avg_sst <- avg_df$avg_sst[nn_avg]
  
  # Find nearest neighbor index for max_sst
  nn_max <- get.knnx(points_max, query_point, k = 1)$nn.index[1,1]
  nearest_max_sst <- max_df$max_sst[nn_max]
  
  return(c(nearest_avg_sst, nearest_max_sst))
}

# Put your yearly SST datasets into named lists for easy lookup
avg_sst_list <- list(
  "2000" = avg_sst_2000,
  "2001" = avg_sst_2001,
  "2003" = avg_sst_2003,
  "2014" = avg_sst_2014,
  "2017" = avg_sst_2017
)

max_sst_list <- list(
  "2000" = max_sst_2000,
  "2001" = max_sst_2001,
  "2003" = max_sst_2003,
  "2014" = max_sst_2014,
  "2017" = max_sst_2017
)

# Initialize new columns
all_unique_locations$avg_sst <- NA_real_
all_unique_locations$max_sst <- NA_real_

# Loop through all rows to find nearest SST values
for (i in seq_len(nrow(all_unique_locations))) {
  row <- all_unique_locations[i, ]
  lat <- row$Latitude
  lon <- row$Longitude
  year <- as.character(row$Date)  # assuming Date is year as numeric or string
  
  sst_values <- get_nearest_sst(lat, lon, year, avg_sst_list, max_sst_list)
  
  all_unique_locations$avg_sst[i] <- sst_values[1]
  all_unique_locations$max_sst[i] <- sst_values[2]
}

write.csv(all_unique_locations, "all_unique_locations_with_sst.csv", row.names = FALSE)


# add to combined_sl
combined_sl_enriched <- combined_sl %>%
  left_join(
    all_unique_locations %>% 
      select(Date, Latitude, Longitude, avg_sst, max_sst),
    by = c("Date", "Latitude", "Longitude")
  )

# divide data ready for rarefaction
combined_sl_enriched <- combined_sl_enriched %>%
  mutate(ID_Location = paste0(ID, "_", Location))
combined_sl_enriched$ID_Location <- gsub("[^A-Za-z0-9]+", "_", combined_sl_enriched$ID_Location)
split_dfs <- split(combined_sl_enriched, combined_sl_enriched$ID_Location)
for(name in names(split_dfs)) {
  safe_name <- paste0("sample_", gsub("[^A-Za-z0-9]+", "_", name))
  var_name <- make.names(safe_name)
  assign(var_name, split_dfs[[name]], envir = .GlobalEnv)
  csv_file <- paste0(safe_name, ".csv")
  write.csv(split_dfs[[name]], csv_file, row.names = FALSE)
  cat("Created dataset:", var_name, "and saved file:", csv_file, "\n")
}

