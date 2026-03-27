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
library(dplyr)
filtered_data <- combined_lsc %>%
  filter(!Genus %in% c("Sclerophytum", "Milleporidae")) %>%  # exclude unwanted genera
  group_by(Genus) %>%
  filter(n() > 2) %>%  # keep genera with more than 2 rows
  ungroup()
genus_list <- split(filtered_data, filtered_data$Genus)

### stat tests

# 1 - Multinomial logistic regression (overall)
library(nnet)
filtered_data$Clade <- factor(filtered_data$Clade)
model_1 <- multinom(Clade ~ latitude, data = filtered_data)
summary(model_1)

capture.output({
  cat("Multinomial logistic regression model_1 (Clade ~ latitude)\n\n")
  print(summary(model_1))
}, file = "multinomial_logistic_regression_1.txt")


# 2 - Logistic regression per species (binary clade - one clade vs others per species)
# A
library(dplyr)
library(broom)
results_2A <- lapply(genus_list, function(df) {
  df$Clade_binary <- ifelse(df$Clade == "A", 1, 0)
  if(length(unique(df$Clade_binary)) > 1) {
    glm_fit <- glm(Clade_binary ~ latitude, data = df, family = binomial)
    tidy(glm_fit)
  } else {
    NULL
  }
})
names(results_2A) <- names(genus_list)
str(results_2A)

# C
library(dplyr)
library(broom)
results_2C <- lapply(genus_list, function(df) {
  df$Clade_binary <- ifelse(df$Clade == "C", 1, 0)
  if(length(unique(df$Clade_binary)) > 1) {
    glm_fit <- glm(Clade_binary ~ latitude, data = df, family = binomial)
    tidy(glm_fit)
  } else {
    NULL
  }
})
names(results_2C) <- names(genus_list)
str(results_2C)

# D
library(dplyr)
library(broom)
results_2D <- lapply(genus_list, function(df) {
  df$Clade_binary <- ifelse(df$Clade == "D", 1, 0)
  if(length(unique(df$Clade_binary)) > 1) {
    glm_fit <- glm(Clade_binary ~ latitude, data = df, family = binomial)
    tidy(glm_fit)
  } else {
    NULL
  }
})
names(results_2D) <- names(genus_list)
str(results_2D)

write_results_to_txt <- function(results_list, filename) {
  sink(filename)
  for (genus in names(results_list)) {
    cat("Genus:", genus, "\n")
    if (is.null(results_list[[genus]])) {
      cat("No results (NULL)\n\n")
    } else {
      print(results_list[[genus]])
      cat("\n")
    }
  }
  sink()
}
write_results_to_txt(results_2A, "logistic_regression_summary_2A.txt")
write_results_to_txt(results_2C, "logistic_regression_summary_2C.txt")
write_results_to_txt(results_2D, "logistic_regression_summary_2D.txt")



# 3 - Beta regression on clade proportions per location

# A
library(betareg)
prop_data <- filtered_data %>%
  group_by(longitude, latitude, Clade) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(longitude, latitude) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()
cladeA_data <- prop_data %>% filter(Clade == "A")
beta_model_3A <- betareg(prop ~ latitude, data = cladeA_data)

summary(beta_model_3A)

# C
library(betareg)
prop_data <- filtered_data %>%
  group_by(longitude, latitude, Clade) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(longitude, latitude) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()
cladeC_data <- prop_data %>% filter(Clade == "C")
beta_model_3C <- betareg(prop ~ latitude, data = cladeC_data)

summary(beta_model_3C)

# D
library(betareg)
prop_data <- filtered_data %>%
  group_by(longitude, latitude, Clade) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(longitude, latitude) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()
cladeD_data <- prop_data %>% filter(Clade == "D")
beta_model_3D <- betareg(prop ~ latitude, data = cladeD_data)

summary(beta_model_3D)

sink("beta_models_summary_3.txt")
cat("Summary of beta_model_3A\n")
print(summary(beta_model_3A))
cat("\n\n")
cat("Summary of beta_model_3C\n")
print(summary(beta_model_3C))
cat("\n\n")
cat("Summary of beta_model_3D\n")
print(summary(beta_model_3D))
cat("\n\n")
sink()

