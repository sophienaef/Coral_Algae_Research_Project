physio_data <- read.csv("Red_Sea_Physio.csv")
bleaching_data <- read.csv("Red_Sea_Visual_Bleaching.csv")

# reorder columns
print(colnames(physio_data))
physio_data <- physio_data[, c("Label", "Collection_Date", "Site_Latitude", 
                               "Site_Longitude", "Site", "Temperature", 
                               "Species", "Genotype", "Replicate",
                               "Sym_Density", "Chla_cm2")]

print(colnames(bleaching_data))
bleaching_data <- bleaching_data[, c("Label", "Collection_Date", "Site_Latitude", 
                                     "Site_Longitude", "Site", "Temperature",
                                     "Species", "Genotype", "Replicate",
                                     "Visual_Bleaching")]

# combine files
library(dplyr)

leftover_bleaching <- anti_join(bleaching_data, physio_data, by = "Label")
write.csv(leftover_bleaching, "leftover_bleaching.csv", row.names = FALSE)

common_cols <- intersect(names(physio_data), names(bleaching_data))
common_cols <- setdiff(common_cols, "Label")
physio_unique <- setdiff(names(physio_data), c(common_cols, "Label"))
bleaching_unique <- setdiff(names(bleaching_data), c(common_cols, "Label"))
physio_part <- physio_data %>% select(Label, all_of(common_cols), all_of(physio_unique))
bleaching_part <- bleaching_data %>% select(Label, all_of(bleaching_unique))
combined_physio_and_bleaching <- inner_join(physio_part, bleaching_part, by = "Label")
write.csv(combined_physio_and_bleaching, "combined_physio_and_bleaching.csv", row.names = FALSE)
