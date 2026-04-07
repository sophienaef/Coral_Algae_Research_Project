csv_files <- list.files(pattern = "\\.csv$")
for (file in csv_files) {
  df_name <- tools::file_path_sans_ext(file)
  assign(df_name, read.csv(file))
}

# adding taxonomy

library(dplyr)

# Load your taxonomy list
taxonomy_list <- read.csv("taxonomy_list.csv")

# Get all object names starting with "sample_"
sample_names <- ls(pattern = "^sample_")

for (name in sample_names) {
  obj <- get(name)
  
  # Process only if the object is a data frame
  if (is.data.frame(obj)) {
    enriched_df <- obj %>%
      left_join(taxonomy_list %>% select(Genus, Family), by = "Genus")
    
    # Overwrite the original data frame
    assign(name, enriched_df)
  } else {
    message(paste(name, "is not a data frame and was skipped."))
  }
}

# bayseain - division by family

source("func_bb.r")
library(dplyr)

# Collect all sample data frames starting with "sample_"
sample_names <- ls(pattern = "^sample_")

# Remove any non-data frame objects if needed
sample_names <- setdiff(sample_names, "sample_names")

samples_list <- mget(sample_names)

set.seed(123)
n_iterations <- 1000
all_results <- list()

clade_levels <- c("A", "C", "D")  # your actual clade labels

# Extract all unique genera across samples to loop over
all_family <- unique(unlist(lapply(samples_list, function(df) {
  if (is.data.frame(df) && "Family" %in% names(df)) {
    unique(df$Family)
  } else {
    NULL  # skip non-data frames or those without Family column
  }
})))

for (iter in 1:n_iterations) {
  
  # Rarefy each sample to 4 points
  rarefied_samples <- lapply(samples_list, function(sample_df) {
    if (nrow(sample_df) > 4) {
      sample_df %>% sample_n(4)
    } else {
      sample_df
    }
  })
  
  combined_data <- bind_rows(rarefied_samples)
  
  # Run Bayesian Blocks segmentation
  edges <- bayesian_blocks(combined_data$avg_sst, p0 = 0.01)
  
  # Add min and max to edges to cover full range
  edges <- sort(unique(c(min(combined_data$avg_sst), edges, max(combined_data$avg_sst))))
  
  results <- list()
  
  for (i in 1:(length(edges) - 1)) {
    t_start <- edges[i]
    t_end <- edges[i + 1]
    
    block_data <- combined_data %>%
      filter(avg_sst >= t_start & avg_sst < t_end)
    
    # For each Family, calculate clade proportions within this block
    for (family in all_family) {
      family_data <- block_data %>% filter(Family == family)
      
      counts <- prop.table(table(factor(family_data$Clade, levels = clade_levels)))
      
      results[[length(results) + 1]] <- data.frame(
        iteration = iter,
        block_start = t_start,
        block_end = t_end,
        Family = family,
        prob_A = ifelse(!is.na(counts["A"]), counts["A"], 0),
        prob_C = ifelse(!is.na(counts["C"]), counts["C"], 0),
        prob_D = ifelse(!is.na(counts["D"]), counts["D"], 0)
      )
    }
  }
  
  all_results[[iter]] <- do.call(rbind, results)
}

final_results <- do.call(rbind, all_results)

summary_df <- final_results %>%
  group_by(block_start, block_end, Family) %>%
  summarise(
    mean_prob_A = mean(prob_A),
    mean_prob_C = mean(prob_C),
    mean_prob_D = mean(prob_D),
    .groups = "drop"
  ) %>%
  filter(
    ( (mean_prob_A == 0) + (mean_prob_C == 0) + (mean_prob_D == 0) ) <= 1
  )

print(summary_df)

write.csv(summary_df, "summary_clade_probabilities_by_family.csv", row.names = FALSE)

# plot

summary_df <- read.csv("summary_clade_probabilities_by_family.csv")

library(dplyr)
library(tidyr)
library(ggplot2)

family_to_include <- c("Euphylliidae", "Agariciidae", "Milleporidae", "Pocilloporidae",
                       "Poritidae", "Xeniidae", "Acroporidae", "Merulinidae")

# Extract unique Family-Genus pairs from taxonomy_list
family_genus_ref <- taxonomy_list %>%
  select(Family, Genus) %>%
  distinct()

# Join Genus info to summary_df by Family
summary_with_genus <- summary_df %>%
  left_join(family_genus_ref, by = "Family")

# Prepare data for plotting
plot_data <- summary_with_genus %>%
  pivot_longer(
    cols = starts_with("mean_prob_"),
    names_to = "Clade",
    values_to = "Probability"
  ) %>%
  mutate(
    Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"),
    Temperature = (block_start + block_end) / 2
  ) %>%
  filter(Family %in% family_to_include)

# Set Family as factor with levels in the desired order for facet ordering
plot_data$Family <- factor(plot_data$Family, levels = family_to_include)

# Create a named vector for facet labels with genera listed under family
family_labels <- plot_data %>%
  group_by(Family) %>%
  summarize(genera = paste(sort(unique(Genus)), collapse = ", ")) %>%
  mutate(label = paste0(Family, "\n(", genera, ")")) %>%
  { setNames(.$label, .$Family) }

# Reorder family_labels to match the factor levels
family_labels <- family_labels[family_to_include]

# Define your clade colors (adjust as needed)
clade_colors <- c(
  "A" = "#86BABD",
  "C" = "#D9C87F",
  "D" = "#E69591"
)

# Plot with custom facet labels and ordered facets
stacked_area_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~ Family, scales = "free_x", labeller = labeller(Family = family_labels)) +
  labs(
    x = "Temperature °C",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 8),
    legend.position = "none",
    text = element_text(size = 8),
    plot.title = element_text(size = 12)
  )

print(stacked_area_plot)









summary_df <- read.csv("summary_clade_probabilities_by_family.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "#86BABD",
  "C" = "#D9C87F",
  "D" = "#E69591"
)

# List of genera to include
family_to_include <- c("Euphylliidae", "Agariciidae", "Milleporidae", "Pocilloporidae",
                        "Poritidae", "Xeniidae", "Acroporidae", "Merulinidae")

# Prepare data for plotting, including Genus
plot_data <- summary_df %>%
  pivot_longer(cols = starts_with("mean_prob_"), names_to = "Clade", values_to = "Probability") %>%
  mutate(
    Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"),
    Temperature = (block_start + block_end) / 2
  ) %>%
  filter(Family %in% family_to_include)  # Filter to include only selected genera

# Stacked area plot faceted by Genus
stacked_area_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~ Family, scales = "free_x") +  # Facet by Genus
  labs(
    x = "Temperature °C",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 16),       # Facet labels bigger
    legend.position = "none",
    axis.title = element_text(size = 16),       # Axis titles bigger
    axis.text = element_text(size = 14),        # Axis text bigger
    plot.title = element_text(size = 18, face = "bold"),  # Plot title bigger and bold
    text = element_text(size = 14)
  )

print(stacked_area_plot)
ggsave("stacked_area_plot_selected_family.pdf", plot = stacked_area_plot, width = 12.5, height = 7)

