## coral_trait_database.csv

coral_trait_database <- read.csv("coral_trait_database.csv", stringsAsFactors = FALSE)

###

# cleaning & formatting

###

# list of all location in alphabetical order
list_all_locations <- sort(unique(coral_trait_database$location_name))
writeLines(list_all_locations, "list_all_locations.txt")

# list of all species in alphabetical order
list_all_species <- sort(unique(coral_trait_database$specie_name))
writeLines(list_all_species, "list_all_species.txt")

# lists all trait names in alphabetical order
list_all_traits <- sort(unique(coral_trait_database$trait_name))
writeLines(list_all_traits, "list_all_traits.txt")

###

# shows all location with relevant traits -> few red sea found
keywords <- c("Symbiodinium clade", "Symbiodinium density", "Symbiodinium subclade",
              "Symbiont density", "Zooxanthellate")
pattern <- paste(keywords, collapse = "|")
filtered_coral_trait_database <- coral_trait_database[grepl(pattern, coral_trait_database$trait_name, ignore.case = TRUE), ]
unique_locations <- unique(filtered_coral_trait_database$location_name)
writeLines(unique_locations, "filtered_locations.txt")

# isolates red sea locations with relevant traits
red_sea <- coral_trait_database
keywords <- c("Symbiodinium clade", "Symbiodinium density", "Symbiodinium subclade",
              "Symbiont density")
pattern <- paste(keywords, collapse = "|")
selected_locations <- c("Eilat, Israel", "Gulf of Eliat, Red Sea", "Sinai peninsula, Red Sea")
coral_trait_database_red_sea <- red_sea[
  grepl(pattern, red_sea$trait_name, ignore.case = TRUE) & 
    red_sea$location_name %in% selected_locations,
]
write.csv(coral_trait_database_red_sea, "coral_trait_database_red_sea.csv", row.names = FALSE)

# shows all traits existent for Gulf of Eliat -> no symbionts
filtered_traits <- coral_trait_database[coral_trait_database$location_name == "Gulf of Eliat, Red Sea", ]
unique_traits <- unique(filtered_traits$trait_name)
writeLines(unique_traits, "traits_gulf_of_eliat.txt")

###

# gives locations that have both symbiont clade and temperature
traits_to_check <- c("Water temperature", "Symbiodinium clade")
locations_with_both <- coral_trait_database %>%
  filter(trait_name %in% traits_to_check) %>%
  group_by(location_name) %>%
  summarize(traits_found = list(unique(trait_name))) %>%
  filter(all(traits_to_check %in% traits_found)) %>%
  pull(location_name)
writeLines(locations_with_both, "locations_with_both_traits.txt")

# makes csv only with locations that have both symbiont clade and temperature
traits_to_check <- c("Water temperature", "Symbiodinium clade")
locations_with_both <- coral_trait_database %>%
  filter(trait_name %in% traits_to_check) %>%
  distinct(location_name, trait_name) %>%
  group_by(location_name) %>%
  summarize(traits_present = n_distinct(trait_name)) %>%
  filter(traits_present == length(traits_to_check)) %>%
  pull(location_name)
clade_filtered_data <- coral_trait_database %>%
  filter(location_name %in% locations_with_both, trait_name %in% traits_to_check)
write.csv(clade_filtered_data, "clade_filtered_locations_with_both_traits.csv", row.names = FALSE)

### 

# gives locations that have both symbiont subclade and temperature
traits_to_check <- c("Water temperature", "Symbiodinium subclade")
locations_with_both_subclade <- coral_trait_database %>%
  filter(trait_name %in% traits_to_check) %>%
  group_by(location_name) %>%
  summarize(traits_found = list(unique(trait_name))) %>%
  filter(all(traits_to_check %in% traits_found)) %>%
  pull(location_name)
writeLines(locations_with_both_subclade, "locations_with_both_traits_subclade.txt")

# csv with eilat only and subclade -> only one lcoation
rows_eilat_only <- read.csv("coral_trait_database.csv", stringsAsFactors = FALSE)
filtered_rows_eilat_only <- rows_eilat_only[rows_eilat_only$location_name == "Eilat, Israel" & rows_eilat_only$trait_name == "Symbiodinium subclade", ]
write.csv(filtered_rows_eilat_only, "filtered_rows_eilat_only.csv", row.names = FALSE)


# makes csv only with locations that have both symbiont subclade and temperature
library(dplyr)
traits_to_check <- c("Water temperature", "Symbiodinium subclade")
locations_with_both <- coral_trait_database %>%
  filter(trait_name %in% traits_to_check) %>%
  distinct(location_name, trait_name) %>%
  group_by(location_name) %>%
  summarize(traits_present = n_distinct(trait_name)) %>%
  filter(traits_present == length(traits_to_check)) %>%
  pull(location_name)
subclade_filtered_data <- coral_trait_database %>%
  filter(location_name %in% locations_with_both, trait_name %in% traits_to_check)
write.csv(subclade_filtered_data, "subclade_filtered_locations_with_both_traits.csv", row.names = FALSE)


###

# makes csv only with locations that have both symbiont clade and subclade 
traits_to_check <- c("Symbiodinium clade", "Symbiodinium subclade")
locations_with_both <- coral_trait_database %>%
  filter(trait_name %in% traits_to_check) %>%
  distinct(location_name, trait_name) %>%
  group_by(location_name) %>%
  summarize(traits_present = n_distinct(trait_name)) %>%
  filter(traits_present == length(traits_to_check)) %>%
  pull(location_name)
clade_subclade_red_sea_filtered_data <- coral_trait_database %>%
  filter(location_name %in% locations_with_both, trait_name %in% traits_to_check)
write.csv(clade_subclade_red_sea_filtered_data, "clade_subclade_red_sea_filtered_data.csv", row.names = FALSE)



