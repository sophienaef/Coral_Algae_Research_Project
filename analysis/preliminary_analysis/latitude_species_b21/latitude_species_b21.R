b21_ls <- read.csv("Barshis21_combined_physio_and_bleaching.csv")

### cleaning 1.0

# rename columns
colnames(b21_ls)[colnames(b21_ls) == "Site_Latitude"] <- "latitude"
colnames(b21_ls)[colnames(b21_ls) == "Site_Longitude"] <- "longitude"

### plots

library(dplyr)
library(ggplot2)
library(patchwork)

# graph for av sym density per species vs temp
avg_data <- b21_ls %>%
  group_by(Species, Temperature) %>%
  summarise(mean_Sym_Density = mean(Sym_Density, na.rm = TRUE)) %>%
  ungroup()
p_sd <- ggplot(avg_data, aes(x = Temperature, y = mean_Sym_Density, color = Species, group = Species)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 3) +
  labs(title = "Average Symiont Density by Species Across Temperatures",
       x = "Temperature",
       y = "Average Symiont Density") +
  theme_minimal()

# graph for av chla_cm2 per species vs temp
avg_data <- b21_ls %>%
  group_by(Species, Temperature) %>%
  summarise(mean_Chla_cm2 = mean(Chla_cm2, na.rm = TRUE)) %>%
  ungroup()
p_ch <- ggplot(avg_data, aes(x = Temperature, y = mean_Chla_cm2, color = Species, group = Species)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 3) +
  labs(title = "Average Chla cm2 by Species Across Temperatures",
       x = "Temperature",
       y = "Average Chla cm2") +
  theme_minimal()

# graph for av bleaching per species vs temp
avg_data <- b21_ls %>%
  group_by(Species, Temperature) %>%
  summarise(mean_Visual_Bleaching = mean(Visual_Bleaching, na.rm = TRUE)) %>%
  ungroup()
p_vb <- ggplot(avg_data, aes(x = Temperature, y = mean_Visual_Bleaching, color = Species, group = Species)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 3) +
  labs(title = "Average Visual Bleaching by Species Across Temperatures",
       x = "Temperature",
       y = "Average Visual Bleaching") +
  theme_minimal()

# print plots
combined_plot <- p_sd / p_ch / p_vb
print(combined_plot)
ggsave("combined_plot.pdf", combined_plot, width = 8, height = 12)