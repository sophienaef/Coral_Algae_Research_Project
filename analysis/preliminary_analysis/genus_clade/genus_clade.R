b04_gc <- read.csv("Baker2004_Red_Sea.csv")
ctd_gc <- read.csv("coral_trait_database_red_sea.csv")
h20_gc <- read.csv("Hume2020_combined_data.csv")
z17_gc <- read.csv("Ziegler2017_cleaned_joined_data_red_sea.csv")

###

# add IDs 
b04_gc$ID <- "b04"
ctd_gc$ID <- "ctd"
h20_gc$ID <- "h20"
z17_gc$ID <- "z17"

b04_gc <- b04_gc[, c("ID", setdiff(names(b04_gc), "ID"))]
ctd_gc <- ctd_gc[, c("ID", setdiff(names(ctd_gc), "ID"))]
h20_gc <- h20_gc[, c("ID", setdiff(names(h20_gc), "ID"))]
z17_gc <- z17_gc[, c("ID", setdiff(names(z17_gc), "ID"))]

### cleaning 1.0

# b04 expand data frame
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
b04_gc <- expand_rows_by_count(b04_gc, "Number_of_Sampled_Colonies")
rownames(b04_gc) <- NULL

### 

# select rows of each data frame
b04_gc <- b04_gc[, c("ID", "Genus", "Clade")]
ctd_gc <- ctd_gc[, c("ID", "specie_name", "value")]
h20_gc <- h20_gc[, c("ID", "species", "clade")]
z17_gc <- z17_gc[, c("ID", "Genus", "Major_Clade")]

### cleaning 2.0

# b04 remove data with unknown genera
b04_gc <- b04_gc[-c(25, 26, 32, 33), ]

# ctd remove subclade data, remove species, and rename columns
ctd_gc <- ctd_gc[-2, ]
ctd_gc$specie_name <- sub(" .*", "", ctd_gc$specie_name)
colnames(ctd_gc)[colnames(ctd_gc) == "specie_name"] <- "Genus"
colnames(ctd_gc)[colnames(ctd_gc) == "value"] <- "Clade"

# h20 rename columns
colnames(h20_gc)[colnames(h20_gc) == "species"] <- "Genus"
colnames(h20_gc)[colnames(h20_gc) == "clade"] <- "Clade"

# z17 remove data with no clade, rename leather coral, and rename columns
z17_gc <- z17_gc[-c(193), ]
z17_gc$Genus[z17_gc$Genus == "Leather Coral"] <- "Sarcophyton"
z17_gc$Genus[z17_gc$Genus == "Symphyllia"] <- "Lobophyllia"
z17_gc$Genus[z17_gc$Genus == "Tubinaria"] <- "Turbinaria"
colnames(z17_gc)[colnames(z17_gc) == "Major_Clade"] <- "Clade"

###

# combine data
combined_gc <- rbind(b04_gc,ctd_gc, h20_gc, z17_gc)

### pie chart

library(dplyr)
library(ggplot2)

# count occurrences 
clade_counts <- combined_gc %>%
  group_by(Genus, Clade) %>%
  summarise(count = n(), .groups = "drop")

# proportion
clade_props <- clade_counts %>%
  group_by(Genus) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# make chart
clade_colors <- c(
  "A" = "lightpink1",
  "C" = "lightgoldenrod2",
  "D" = "lightblue2"
)

pie_chart <- ggplot(clade_props, aes(x = "", y = proportion, fill = Clade)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ Genus) +
  theme_void() +
  scale_fill_manual(values = clade_colors) +
  labs(title = "Proportion of Clades per Coral Genus") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

ggsave("genus_clade_pie_charts.pdf", plot = pie_chart, width = 10, height = 8)

