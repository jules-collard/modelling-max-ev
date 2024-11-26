library(tidyverse)
source("scripts/utils.r")

data <- load_data("data/savant_data_2015_2024.csv", write_names = TRUE)
names <- read_csv("data/batter_names.csv")

playerModels <- get_player_models(data, 250, 10)
  
playerBM <- playerModels %>% 
  unnest(bm) %>%
  rowwise() %>%
  mutate(trans = pgev(bm, xi = xi, mu = mu, beta = sigma)) %>%
  select(batter_id, trans) %>%
  group_by(batter_id) %>%
  summarise(quantiles = list(trans)) %>%
  mutate(p = map_dbl(quantiles, ksTest))

# Diagnostic Plots
ggplot(data = playerModels, mapping = aes(x = mu)) +
  geom_histogram(fill="lightblue", col="black") +
  theme_bw() +
  labs(title = "Distribution of Location Parameter for GEV Fit",
        subtitle = "MLB Players with >= 250 BBEs, 2015-2024",
        x = "Location (mu)", y = "Count")
ggsave("plots/location.png")

ggplot(data = playerModels, mapping = aes(x = sigma)) +
  geom_histogram(fill="lightblue", col="black") +
  theme_bw() +
  labs(title = "Distribution of Scale Parameter for GEV Fit",
        subtitle = "MLB Players with >= 300 BBEs, 2015-2024",
        x = "Scale (sigma)", y = "Count")
ggsave("plots/scale.png")

ggplot(data = playerModels, mapping = aes(x = xi)) +
  geom_histogram(fill="lightblue", col="black", center = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(title = "Distribution of Shape Parameter for GEV Fit",
        subtitle = "MLB Players with >= 300 BBEs, 2015-2024",
        x = "Shape (xi)", y = "Count")
ggsave("plots/shape.png")

ggplot(data = playerBM, mapping = aes(x = p)) +
  geom_histogram(fill="lightblue", col="black", breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = 0.05, linetype = "dashed") +
  theme_bw() +
  labs(title = "p-values for Kolmogorov-Smirnov Test for Uniformity",
      subtitle = "CDF Transform on Block Maxima",
      y = "Count")
ggsave("plots/kstest.png")