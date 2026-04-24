csv_files <- list.files(pattern = "\\.csv$")
for (file in csv_files) {
  df_name <- tools::file_path_sans_ext(file)
  assign(df_name, read.csv(file))
}

# min samples = 4 = b04_Ras_Suwaiyil

### bayesian

source("func_bb.r")
library(dplyr)

sample_names <- ls(pattern = "^sample_")

sample_names <- setdiff(sample_names, "sample_names")

samples_list <- mget(sample_names)

set.seed(123)
n_iterations <- 1000
all_results <- list()

clade_levels <- c("A", "C", "D")  # your actual clade labels

for (iter in 1:n_iterations) {

  rarefied_samples <- lapply(samples_list, function(sample_df) {
    if (nrow(sample_df) > 4) {
      sample_df %>% sample_n(4)
    } else {
      sample_df
    }
  })
  
  combined_data <- bind_rows(rarefied_samples)
  
  edges <- bayesian_blocks(combined_data$avg_sst, p0 = 0.01)
  
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

summary_df <- read.csv("summary_clade_probabilities.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "#86BABD",
  "C" = "#D9C87F",
  "D" = "#E69591"
)

# stacked area plot
plot_data <- summary_df %>%
  pivot_longer(cols = starts_with("mean_prob_"), names_to = "Clade", values_to = "Probability") %>%
  mutate(Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"))

stacked_area_plot <- ggplot(plot_data, aes(x = (block_start + block_end)/2, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  labs(       x = "Temperature °C", y = "Probability") +
  theme_minimal()

print(stacked_area_plot)
ggsave("stacked_area_plot.pdf", plot = stacked_area_plot, width = 8, height = 5)


# faceted plot

clade_colors <- c(
  "Symbiodinium" = "#86BABD",
  "Cladocopium" = "#D9C87F",
  "Durusdinium" = "#E69591"
)

plot_data <- summary_df %>%
  pivot_longer(
    cols = starts_with("mean_prob_"),
    names_to = "Clade",
    values_to = "Probability"
  ) %>%
  mutate(
    Clade = recode(Clade,
                   mean_prob_A = "Symbiodinium",
                   mean_prob_C = "Cladocopium",
                   mean_prob_D = "Durusdinium"),
    Clade = factor(Clade, levels = c("Symbiodinium", "Cladocopium", "Durusdinium")),
    Temperature = (block_start + block_end) / 2 
  )

faceted_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~Clade, ncol = 1) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    legend.position = "bottom",
    text = element_text(size = 12),
    plot.title = element_text(size = 24),
  ) +
  labs(x = "Temperature °C", y = "Probability", fill = NULL, color = NULL, shape = NULL, size = NULL)

print(faceted_plot)
ggsave("faceted_clade_probability_plot.pdf", plot = faceted_plot, width = 10, height = 7)



###



# additional division by genus

source("func_bb.r")
library(dplyr)

sample_names <- ls(pattern = "^sample_")

sample_names <- setdiff(sample_names, "sample_names")

samples_list <- mget(sample_names)

set.seed(123)
n_iterations <- 1000
all_results <- list()

clade_levels <- c("A", "C", "D")  # your actual clade labels

all_genera <- unique(unlist(lapply(samples_list, function(df) unique(df$Genus))))

for (iter in 1:n_iterations) {
  
  rarefied_samples <- lapply(samples_list, function(sample_df) {
    if (nrow(sample_df) > 4) {
      sample_df %>% sample_n(4)
    } else {
      sample_df
    }
  })
  
  combined_data <- bind_rows(rarefied_samples)
  
  edges <- bayesian_blocks(combined_data$avg_sst, p0 = 0.01)
  
  edges <- sort(unique(c(min(combined_data$avg_sst), edges, max(combined_data$avg_sst))))
  
  results <- list()
  
  for (i in 1:(length(edges) - 1)) {
    t_start <- edges[i]
    t_end <- edges[i + 1]
    
    block_data <- combined_data %>%
      filter(avg_sst >= t_start & avg_sst < t_end)
    
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

summary_df <- read.csv("summary_clade_probabilities_by_genus.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "#86BABD",
  "C" = "#D9C87F",
  "D" = "#E69591"
)

genera_to_include <- c("Galaxea", "Gardineroseris", "Milleporidae", "Pocillopora", "Porites", "Seriatopora", "Stylophora", "Xenia")

plot_data <- summary_df %>%
  pivot_longer(cols = starts_with("mean_prob_"), names_to = "Clade", values_to = "Probability") %>%
  mutate(
    Clade = recode(Clade, mean_prob_A = "A", mean_prob_C = "C", mean_prob_D = "D"),
    Temperature = (block_start + block_end) / 2
  ) %>%
  filter(Genus %in% genera_to_include)  

stacked_area_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~ Genus, scales = "free_x") + 
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
ggsave("stacked_area_plot_selected_genera.pdf", plot = stacked_area_plot, width = 12.5, height = 7)

