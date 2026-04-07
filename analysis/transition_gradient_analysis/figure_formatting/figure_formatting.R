### faceted

summary_df <- read.csv("summary_clade_probabilities.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "Symbiodinium" = "#679AA1",
  "Cladocopium" = "#BDDAD7",
  "Durusdinium" = "#F0BDB3"
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
    Temperature = (block_start + block_end) / 2  # midpoint of the block
  )

faceted_plot <- ggplot(plot_data, aes(x = Temperature, y = Probability, fill = Clade)) +
  geom_area(alpha = 0.7) +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~Clade, ncol = 3) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),
    legend.position = "top",
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 10, margin = margin(t = 10)),  # Add space above x-axis title
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),  # Add space to the right of y-axis title
    axis.text = element_text(size = 10), 
    text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
  ) +
  labs(x = "Temperature °C", y = "Probability", fill = NULL, color = NULL, shape = NULL, size = NULL)

print(faceted_plot)
ggsave("faceted_clade_probability_plot.pdf", plot = faceted_plot, width = 12.5, height = 2.5)



### by genus

summary_df <- read.csv("summary_clade_probabilities_by_genus.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "#679AA1",
  "C" = "#BDDAD7",
  "D" = "#F0BDB3"
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
  geom_area(alpha = 0.7, position = "stack") +
  scale_fill_manual(values = clade_colors) +
  facet_wrap(~ Genus, scales = "free_x") +  # Facet by Genus
  labs(
    x = "Temperature °C",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 19),       # Facet labels bigger
    legend.position = "none",
    axis.title.x = element_text(size = 19, margin = margin(t = 19)),  # Add space above x-axis title
    axis.title.y = element_text(size = 19, margin = margin(r = 19)),       # Axis titles bigger
    axis.text = element_text(size = 19),        # Axis text bigger
    plot.title = element_text(size = 19, face = "bold"),  # Plot title bigger and bold
    text = element_text(size = 19)
  )

print(stacked_area_plot)
ggsave("stacked_area_plot_selected_genera.pdf", plot = stacked_area_plot, width = 12.5, height = 7)



### by family 

summary_df <- read.csv("summary_clade_probabilities_by_family.csv")

library(ggplot2)
library(tidyr)
library(dplyr)

clade_colors <- c(
  "A" = "#679AA1",
  "C" = "#BDDAD7",
  "D" = "#F0BDB3"
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

plot_data$Family <- factor(plot_data$Family, levels = family_to_include)

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
    strip.text = element_text(size = 19),       # Facet labels bigger
    legend.position = "none",
    axis.title.x = element_text(size = 19, margin = margin(t = 19)),  # Add space above x-axis title
    axis.title.y = element_text(size = 19, margin = margin(r = 19)),     # Axis titles bigger
    axis.text = element_text(size = 19),        # Axis text bigger
    plot.title = element_text(size = 19, face = "bold"),  # Plot title bigger and bold
    text = element_text(size = 19)
  )

print(stacked_area_plot)
ggsave("stacked_area_plot_selected_family.pdf", plot = stacked_area_plot, width = 12.5, height = 7)

