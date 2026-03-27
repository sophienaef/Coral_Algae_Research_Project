file_a <- read.csv("supp_23_sample_meta_info_A_unifrac_no_sqrt.csv")
file_c <- read.csv("supp_22_sample_meta_info_C_unifrac_no_sqrt.csv")
file_d <- read.csv("supp_21_sample_meta_info_D_unifrac_no_sqrt.csv")

# combine files
file_a$clade <- "A"
file_c$clade <- "C"
file_d$clade <- "D"
combined_data <- rbind(file_a, file_c, file_d)
combined_data <- combined_data[, c("clade", setdiff(names(combined_data), "clade"))]
write.csv(combined_data, "combined_data.csv", row.names = FALSE)

# list species names
unique_species <- unique(combined_data$species)
writeLines(unique_species, "unique_species.txt")

# renames species
species_map <- c(
  PC = "Pocillopora",
  ST = "Stylophora",
  SE = "Seriatopora",
  GX = "Galaxea",
  G  = "Gardineroseris",
  P  = "Porites",
  M  = "Milleporidae"
)
combined_data$species <- species_map[combined_data$species]

