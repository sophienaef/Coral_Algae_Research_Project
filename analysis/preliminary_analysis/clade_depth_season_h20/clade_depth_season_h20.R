h20_cds <- read.csv("Hume2020_combined_data.csv")

### depth

library(dplyr)
library(ggplot2)

# proportions of clades at each depth
proportions <- h20_cds %>%
  group_by(depth, clade) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(depth) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

# plot line graph
g_d <- ggplot(proportions, aes(x = depth, y = proportion, color = clade, group = clade)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +  # use linewidth instead of size here
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = c(1, 15, 30)) +
  labs(
    title = "Clade Proportion Change with Depth",
    x = "Depth (meters)",
    y = "Proportion (%)",
    color = "Clade"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("depth_clade.pdf", g_d, width = 8, height = 10)


### seasons

library(dplyr)
library(ggplot2)

# calculate proportions by season and clade
proportions_season <- h20_cds %>%
  group_by(season, clade) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(season) %>%
  mutate(total = sum(count),
         proportion = count / total) %>%
  ungroup()

# plot line chart with points for season
g_s <- ggplot(proportions_season, aes(x = season, y = proportion, color = clade, group = clade)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Clade Proportion Change with Season",
    x = "Season",
    y = "Proportion (%)",
    color = "Clade"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

ggsave("season_clade.pdf", g_s, width = 8, height = 10)

# with species on one graph

library(dplyr)
library(ggplot2)

# Calculate proportions by season, clade, and species
proportions_species <- h20_cds %>%
  group_by(season, clade, species) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(season, clade) %>%
  mutate(total_clade = sum(count),
         proportion = count / total_clade) %>%
  ungroup()

ggplot(proportions_species, aes(x = season, y = proportion, color = species, shape = clade)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Species Proportion Within Clades by Season",
    x = "Season",
    y = "Proportion within Clade (%)",
    color = "Species",
    shape = "Clade"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5)
  )

# separate plots by species

library(dplyr)
library(ggplot2)

proportions_species <- h20_cds %>%
  group_by(season, clade, species) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(season, clade) %>%
  mutate(total_clade = sum(count),
         proportion = count / total_clade) %>%
  ungroup()

# Calculate min and max proportions with padding
min_prop <- min(proportions_species$proportion, na.rm = TRUE)
max_prop <- max(proportions_species$proportion, na.rm = TRUE)

# Add padding: 5% below min, 10% above max (clamped to 0 and 1)
min_prop <- min(proportions_species$proportion, na.rm = TRUE)
y_min <- max(0, min_prop - 0.05)
y_max <- 0.38

p <- ggplot(proportions_species, aes(x = season, y = proportion, color = species, shape = clade, group = clade)) +
  geom_point(size = 4, position = position_dodge(width = 0.5)) +
  geom_line(position = position_dodge(width = 0.5), linewidth = 1) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(y_min, y_max),
    expand = expansion(mult = c(0, 0))  # disable extra expansion since we control limits
  ) +
  facet_wrap(~ species, ncol = 2) +
  labs(
    title = "Clade Proportion Change by Season for Each Species",
    x = "Season",
    y = "Proportion within Clade (%)",
    color = "Species",
    shape = "Clade"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "none"  # optional: hide legend since color = species per facet
  )

ggsave(filename = "clade_proportion_species.png", plot = p,
       width = 12, height = 20, units = "in", dpi = 300)


