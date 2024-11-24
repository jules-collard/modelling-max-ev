library(tidyverse)
library(fExtremes)
library(extRemes)
library(ismev)
source("scripts/utils.r")

data <- load_data("data/savant_data_2017_2024.csv", write_names = TRUE)
names <- read_csv("data/batter_names.csv")

playerModels <- data %>%
  group_by(batter_id) %>%
  mutate(n = n()) %>%
  filter(n >= 300) %>%
  nest() %>%
  mutate(bm = map(data, get_bm),
        gev = map(bm, gev_fit),
        mu = map_dbl(gev, get_mu),
        sigma = map_dbl(gev, get_sigma),
        xi = map_dbl(gev, get_xi)) %>%
  select(-data)
  
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
        subtitle = "MLB Players with >= 300 BBEs, 2015-2024",
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
      subtitle = "Inverse Quantile Transform on GEV Fits",
      y = "Count")
ggsave("plots/kstest.png")

#Kramer-Von Mises, anderson-dalins
library(evir)
params <- distill(gev)[1:3] # mu sigma xi
transformed <- pgev(bm, xi= params[3], mu=params[1], sigma=params[2])
hist(transformed)
# Should be U[0,1]
t <- unlist(playerBM[1,2])
t <- ks.test(unlist(playerBM[1,2]), "punif", min=0, max=1)
t$p.value

pgev()

# Test for uniformity
# Estimating upper endpoint
# Age as covariate - record max likelihood and compare between models 2*diff likelihood 
# GEV upper bound:  x < mu - sigma/xi - sec 3 slide 4
