# import data

data <- read.csv("combined_samples.csv")
data$T <- data$avg_sst

blocks <- read.csv("summary_clade_probabilities.csv")
blocks$T <- (blocks$block_start + blocks$block_end) / 2 # midpoint?

# Reshape bayesian block data

library(tidyr)
library(dplyr)

data_long <- blocks %>%
  pivot_longer(cols = starts_with("mean_prob_"),
               names_to = "Clade",
               values_to = "prob") %>%
  mutate(Clade = gsub("mean_prob_", "", Clade))

# using bayesian block data isnt carried on from here on
# compelte sample data is used

###

# Metabolic Benefit (B)

B_func <- function(T, m, sigma, Topt) {
  m * exp(-sigma * (T - Topt)^2)
  }

# Probability of Expulsion (P_expel)

P_expel_func <- function(B, k, theta) {
    1 / (1 + exp(k * (B - theta)))
}
  
# Parameters
  
library(dplyr)

parameters <- data %>%
  distinct(Clade) %>%
  mutate(
    m = case_when(
      Clade == "A" ~ 0.7,
      Clade == "C" ~ 0.9,
      Clade == "D" ~ 0.8,
      TRUE ~ 0
    ),
    sigma = case_when( # where from
      Clade == "A" ~ 0.7,
      Clade == "C" ~ 0.3,
      Clade == "D" ~ 0.5,
      TRUE ~ 0
    ),
    Topt = case_when( # wh
      Clade == "A" ~ 26,
      Clade == "C" ~ 27,
      Clade == "D" ~ 28,
      TRUE ~ 0
    )
  )

data <- left_join(data, parameters, by = "Clade")

# global parameters
k <- -10
theta <- 0.3
  
# Add parameters to data
  
data$B <- B_func(data$T, data$m, data$sigma, data$Topt)
data$P_expel <- P_expel_func(data$B, k, theta)
data$S <- 1 - data$P_expel

### next lines were just chat gpts suggestions to understand the data better

# Visualise

library(ggplot2)

ggplot(data, aes(x = T, y = S, color = Clade)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  labs(y = "Stability (S)", x = "Temperature")

# Find tipping point

tipping_points <- data %>%
group_by(Clade) %>%
  summarise(
    tipping_T = T[which.min(abs(B - theta))]
  )

tipping_points
