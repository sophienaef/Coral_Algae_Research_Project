csv_files <- list.files(pattern = "\\.csv$")
for (file in csv_files) {
  df_name <- tools::file_path_sans_ext(file)
  assign(df_name, read.csv(file))
}

# min samples = 4 = b04_Ras_Suwaiyil

### bayesian

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
    
    counts <- prop.table(table(factor(block_data$Clade, levels = clade_levels)))
    
    results[[i]] <- data.frame(
      iteration = iter,
      block_start = t_start,
      block_end = t_end,
      prob_A = ifelse(!is.na(counts["A"]), counts["A"], 0),
      prob_C = ifelse(!is.na(counts["C"]), counts["C"], 0),
      prob_D = ifelse(!is.na(counts["D"]), counts["D"], 0)
    )
  }
  
  all_results[[iter]] <- do.call(rbind, results)
}

final_results <- do.call(rbind, all_results)

summary_df <- final_results %>%
  group_by(block_start, block_end) %>%
  summarise(
    mean_prob_A = mean(prob_A),
    mean_prob_C = mean(prob_C),
    mean_prob_D = mean(prob_D),
    .groups = "drop"
  )

print(summary_df)

write.csv(summary_df, "summary_clade_probabilities.csv", row.names = FALSE)


### 

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "lightpink1",
  "C" = "lightgoldenrod2",
  "D" = "lightblue2"
)

# stacked area plot
plot_data <- summary_df %>%
  pivot_longer(cols = starts_with("mean_prob_"), names_to = "Clade", values_to = "Probability") %>%
  mutate(Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"))

stacked_area_plot <- ggplot(plot_data, aes(x = (block_start + block_end)/2, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  labs(title = "Stacked Clade Probabilities Across Temperature",
       x = "Temperature (°C)", y = "Probability") +
  theme_minimal()

print(stacked_area_plot)
ggsave("stacked_area_plot.pdf", plot = stacked_area_plot, width = 8, height = 5)


# faceted plot
plot_data <- summary_df %>%
  pivot_longer(
    cols = starts_with("mean_prob_"),
    names_to = "Clade",
    values_to = "Probability"
  ) %>%
  mutate(
    Clade = recode(Clade,
                   mean_prob_A = "A",
                   mean_prob_C = "C",
                   mean_prob_D = "D"),
    Temperature = (block_start + block_end) / 2  # midpoint of the block
  )

faceted_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~Clade, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Clade Probability by Temperature (Faceted)",
       x = "Temperature (°C)", y = "Probability")

print(faceted_plot)
ggsave("faceted_clade_probability_plot.pdf", plot = faceted_plot, width = 6, height = 8)

###

# additional division by genus

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
all_genera <- unique(unlist(lapply(samples_list, function(df) unique(df$Genus))))

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
    
    # For each Genus, calculate clade proportions within this block
    for (genus in all_genera) {
      genus_data <- block_data %>% filter(Genus == genus)
      
      counts <- prop.table(table(factor(genus_data$Clade, levels = clade_levels)))
      
      results[[length(results) + 1]] <- data.frame(
        iteration = iter,
        block_start = t_start,
        block_end = t_end,
        Genus = genus,
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
  group_by(block_start, block_end, Genus) %>%
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

write.csv(summary_df, "summary_clade_probabilities_by_genus.csv", row.names = FALSE)

# plots

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "lightpink1",
  "C" = "lightgoldenrod2",
  "D" = "lightblue2"
)

# List of genera to include
genera_to_include <- c("Galaxea", "Gardineroseris", "Milleporidae", "Pocillopora", "Porites", "Seriatopora", "Stylophora", "Xenia")

# Prepare data for plotting, including Genus
plot_data <- summary_df %>%
  pivot_longer(cols = starts_with("mean_prob_"), names_to = "Clade", values_to = "Probability") %>%
  mutate(
    Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"),
    Temperature = (block_start + block_end) / 2
  ) %>%
  filter(Genus %in% genera_to_include)  # Filter to include only selected genera

# Stacked area plot faceted by Genus
stacked_area_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.8, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~ Genus, scales = "free_x") +  # Facet by Genus
  labs(
    title = "Stacked Clade Probabilities Across Temperature by Selected Genera",
    x = "Temperature (°C)",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "top"
  )

print(stacked_area_plot)
ggsave("stacked_area_plot_selected_genera.pdf", plot = stacked_area_plot, width = 10, height = 7)
