library(ncdf4)
library(reshape2)
library(dplyr)
library(lubridate)

process_sst_for_years <- function(years) {
  lat_min <- 12.5
  lat_max <- 30.0
  lon_min <- 32.0
  lon_max <- 45.0
  
  for (yr in years) {
    cat("Processing year:", yr, "\n")
    
    file_name <- paste0("sst.day.mean.", yr, ".nc")
    
    if (!file.exists(file_name)) {
      cat("File not found:", file_name, "- skipping this year.\n\n")
      next
    }
    
    nc_data <- nc_open(file_name)
    
    lat <- ncvar_get(nc_data, "lat")
    lon <- ncvar_get(nc_data, "lon")
    time <- ncvar_get(nc_data, "time")
    
    lat_idx <- which(lat >= lat_min & lat <= lat_max)
    lon_idx <- which(lon >= lon_min & lon <= lon_max)

    sst <- ncvar_get(nc_data, "sst")
    
    dims <- dim(sst)
    cat("Dimensions of sst variable:", paste(dims, collapse = " x "), "\n")
    
    sst_subset <- sst[lon_idx, lat_idx, ]
    lat_subset <- lat[lat_idx]
    lon_subset <- lon[lon_idx]
    
    time_units <- ncatt_get(nc_data, "time", "units")$value
    origin_date <- sub("days since ", "", time_units)
    dates <- as.Date(time, origin = origin_date)
    
    nc_close(nc_data)
    
    df <- expand.grid(lon = lon_subset, lat = lat_subset, time = dates)
    df$sst <- as.vector(sst_subset)

    df_clean <- df[!is.na(df$sst), ]
    
    df_clean$year <- year(df_clean$time)
    
    avg_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(avg_sst = mean(sst, na.rm = TRUE), .groups = "drop")

    max_sst_yearly <- df_clean %>%
      group_by(lon, lat, year) %>%
      summarise(max_sst = max(sst, na.rm = TRUE), .groups = "drop")
    
    avg_file <- paste0("avg_sst_per_year_red_sea_", yr, ".csv")
    max_file <- paste0("max_sst_per_year_red_sea_", yr, ".csv")
    
    write.csv(avg_sst_yearly, avg_file, row.names = FALSE)
    write.csv(max_sst_yearly, max_file, row.names = FALSE)
    
    cat("Saved average SST to:", avg_file, "\n")
    cat("Saved max SST to:", max_file, "\n\n")
  }
}

years_to_process <- c(2000, 2001, 2003, 2014, 2017)

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

world <- map_data("world")

p <- ggplot() +
  geom_raster(data = sst_2017, aes(x = lon, y = lat, fill = avg_sst), interpolate = TRUE) +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "grey95",
    color = "black",
    size = 0.2
  ) +
  
  geom_point(
    data = all_unique_locations,
    aes(x = Longitude, y = Latitude),
    shape = 18,
    color = "black",     
    size = 8,           
    alpha = 0.6        
  ) +
  
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
    axis.text = element_text(size = 16),         
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16),       
    legend.text = element_text(size = 16),      
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

