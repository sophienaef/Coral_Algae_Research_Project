
# Load required libraries
library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

# Define the processing function
process_sst_for_years <- function(years) {
  # Define Red Sea bounding box
  lat_min <- 12.5
  lat_max <- 30.0
  lon_min <- 32.0
  lon_max <- 45.0
  
  for (yr in years) {
    cat("Processing year:", yr, "\n")
    
    # Construct file name
    file_name <- paste0("sst.day.mean.", yr, ".nc")
    
    # Check if file exists
    if (!file.exists(file_name)) {
      cat("File not found:", file_name, "- skipping this year.\n\n")
      next
    }
    
    # Open NetCDF file
    nc_data <- nc_open(file_name)
    
    # Extract coordinates
    lat <- ncvar_get(nc_data, "lat")
    lon <- ncvar_get(nc_data, "lon")
    time <- ncvar_get(nc_data, "time")
    
    # Find indices within Red Sea bounds
    lat_idx <- which(lat >= lat_min & lat <= lat_max)
    lon_idx <- which(lon >= lon_min & lon <= lon_max)
    
    # Extract sst variable
    sst <- ncvar_get(nc_data, "sst")
    
    # Confirm dimension order
    dims <- dim(sst)
    cat("Dimensions of sst variable:", paste(dims, collapse = " x "), "\n")
    
    # Subset sst by lon and lat indices, keep all time
    # Assuming sst is [lon, lat, time]
    sst_subset <- sst[lon_idx, lat_idx, ]
    
    # Subset coordinates
    lat_subset <- lat[lat_idx]
    lon_subset <- lon[lon_idx]
    
    # Convert time units to dates
    time_units <- ncatt_get(nc_data, "time", "units")$value
    origin_date <- sub("days since ", "", time_units)
    dates <- as.Date(time, origin = origin_date)
    
    nc_close(nc_data)
    
    # Create long-format data frame
    df <- expand.grid(lon = lon_subset, lat = lat_subset, time = dates)
    df$sst <- as.vector(sst_subset)
    
    # Remove NA values
    df_clean <- df[!is.na(df$sst), ]
    
    # Add year column extracted from time
    df_clean$year <- year(df_clean$time)
    
    # Calculate average sst per lon, lat, year
    avg_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(avg_sst = mean(sst, na.rm = TRUE), .groups = "drop")
    
    # Calculate max sst per lon, lat, year
    max_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(max_sst = max(sst, na.rm = TRUE), .groups = "drop")
    
    # Save CSV files in working directory
    avg_file <- paste0("avg_sst_per_year_red_sea_", yr, ".csv")
    max_file <- paste0("max_sst_per_year_red_sea_", yr, ".csv")
    
    write.csv(avg_sst_yearly, avg_file, row.names = FALSE)
    write.csv(max_sst_yearly, max_file, row.names = FALSE)
    
    cat("Saved average SST to:", avg_file, "\n")
    cat("Saved max SST to:", max_file, "\n\n")
  }
}

# Specify the years you want to process
years_to_process <- c(2000, 2001, 2003, 2014, 2017)

# Run the processing function
process_sst_for_years(years_to_process)

###

# plot map to visualise temps

library(ggplot2)
library(dplyr)
library(maps)

sst_2017 <- read.csv("avg_sst_per_year_red_sea_2017.csv")
all_unique_locations <- read.csv("all_unique_locations_with_sst.csv")

lon_limits <- c(32, 44.375)
lat_limits <- c(13.325, 30)

# Get world map data
world <- map_data("world")

p <- ggplot() +
  # SST layer
  geom_raster(data = sst_2017, aes(x = lon, y = lat, fill = avg_sst), interpolate = TRUE) +
  
  # Coastlines overlay
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "black",
    size = 0.2
  ) +
  
  # Points layer from all_unique_locations dataset
  geom_point(
    data = all_unique_locations,
    aes(x = Longitude, y = Latitude),
    shape = 18,
    color = "black",      # You can choose any color
    size = 8,           # Adjust size as needed
    alpha = 0.6         # Optional transparency
  ) +
  
  # Color scale (your flipped version)
  scale_fill_gradientn(
    name = "Avgerage SST °C",
    # colors = c( "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#fff5cc", "#ffd700", "#ff8c00", "#ff4500"),
    colors = c(
      "#477a85", "#679aa1", "#86babd", "#bddad7", "#f4faf0",
      "#f3e6d9", "#f0bdb3", "#e69591", "#c76b6c"
    ),
    values = scales::rescale(c(0.00, 0.15, 0.275, 0.375, 0.525, 0.65, 0.8, 0.9, 1.00))
  ) +
  
  coord_fixed(xlim = lon_limits, ylim = lat_limits, ratio = 1.3) +
  
  labs(
    x = "Longitude",
    y = "Latitude"
  ) +
  
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),          # axis tick labels
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),       # legend title size
    legend.text = element_text(size = 16),        # legend labels size
    legend.position = "bottom",
    legend.key.width = unit(6, "cm"),
    legend.key.height = unit(1, "cm")
  ) +
  
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = 12,
      barheight = 0.8
    )
  )

print(p)

ggsave("heat_map.pdf", plot = p, width = 6, height = 10)

