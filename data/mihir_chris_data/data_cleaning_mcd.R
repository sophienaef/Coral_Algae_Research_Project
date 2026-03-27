## mihir_chris_data.csv

mihir_chris_data <- read.csv("mihir_chris_data.csv", stringsAsFactors = FALSE)

# removed extra space before all Isla Enmedio
mihir_chris_data$location_name[45335:45338] <- trimws(mihir_chris_data$location_name[45335:45338]) 

# renaming locations in the RedSea
library(dplyr)
mihir_chris_data <- mihir_chris_data %>%
  mutate(location_name = recode(location_name,
                                "Abu Qalawa" = "Abu Qalawa, Jeddah, Red Sea",
                                "Al-Ahyaa" = "Al-Ahyaa, Egypt, Red Sea",
                                "Beer Odeeb, Northern Gulf of Suez, Red Sea" = "Beer Odeeb, Northern Gulf of Suez, Red Sea",  # unchanged
                                "Coast of Jeddah, Saudi Arabia, Central Red Sea" = "Coast of Jeddah, Saudi Arabia, Red Sea",
                                "Eilat, Israel" = "Eilat, Israel, Red Sea",
                                "El-Fanadir" = "El-Fanadir, Egypt, Red Sea",
                                "Gulf of Suez" = "Gulf of Suez, Red Sea",
                                "Na'ama Bay, Egypt" = "Na'ama Bay, Egypt, Red Sea",
                                "Yanbu" = "Yanbu, Saudi Arabia, Red Sea"
  ))


###

# list of all location in alphabetical order
list_all_location <- mihir_chris_data$location_name
list_all_location_sorted <- sort(unique(list_all_location))
writeLines(list_all_location_sorted, "list_all_location_sorted.txt")

# list of all species in alphabetical order
list_all_species <- mihir_chris_data$specie_name
list_all_species_sorted <- sort(unique(list_all_species))
writeLines(list_all_species_sorted, "list_all_species_sorted.txt")

###

# data of only red sea
mihir_chris_data_red_sea <- subset(mihir_chris_data, grepl("Red Sea", location_name, ignore.case = TRUE))
write.csv(mihir_chris_data_red_sea , "mihir_chris_data_red_sea.csv", row.names = FALSE)

# list of all red sea location in alphabetical order
list_location_red_sea <- mihir_chris_data_red_sea$location_name
list_location_sorted_red_sea <- sort(unique(list_location_red_sea))
writeLines(list_location_sorted_red_sea, "list_location_sorted_red_sea.txt")

# list of all red sea species in alphabetical order
list_species_red_sea <- mihir_chris_data_red_sea$specie_name
list_species_sorted_red_sea <- sort(unique(list_species_red_sea))
writeLines(list_species_sorted_red_sea, "list_species_sorted_red_sea.txt")

###

# makes csv only with locations that have both symbiont clade and subclade in red sea -> only 2, useless
traits_to_check <- c("Symbiodinium clade", "Symbiodinium subclade")
locations_with_both <- mihir_chris_data_red_sea %>%
  filter(trait_name %in% traits_to_check) %>%
  distinct(location_name, trait_name) %>%
  group_by(location_name) %>%
  summarize(traits_present = n_distinct(trait_name)) %>%
  filter(traits_present == length(traits_to_check)) %>%
  pull(location_name)
clade_subclade_red_sea_filtered_data <- mihir_chris_data_red_sea %>%
  filter(location_name %in% locations_with_both, trait_name %in% traits_to_check)
write.csv(clade_subclade_red_sea_filtered_data, "clade_subclade_red_sea_filtered_data.csv", row.names = FALSE)


