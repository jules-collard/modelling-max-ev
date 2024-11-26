library(tidyverse)
source("scripts/utils.r")

data <- load_data("data/savant_data_2015_2024.csv", write_names = TRUE)

tuning_matrix <- function(data_nested, season_pairs, block_sizes = seq(10,30), k = seq(5, 30)) {
  require(dplyr)
  results <- tibble(block_size = numeric(), k = numeric(), correlation.wOBACON = numeric(), correlation.maxEV = numeric())
  for (block_size in block_sizes) {
    for (return_length in k) {
      models <- data_nested %>%
        mutate(bm = map(data, ~get_bm(.x, block_size = block_size)),
                gev = map(bm, gev_fit),
                mu = map_dbl(gev, get_mu),
                sigma = map_dbl(gev, get_sigma),
                xi = map_dbl(gev, get_xi),
                return_level = get_return_level(return_length, mu, sigma, xi)) %>%
        select(-c(data, bm, gev))

      correlations <- season_pairs %>%
        inner_join(models, by = join_by(batter_id == batter_id, year.x == year))

      results <- results %>% add_row(
        block_size = block_size,
        k = return_length,
        correlation.wOBACON = cor(correlations$return_level, correlations$wOBACON.y),
        correlation.maxEV = cor(correlations$return_level, correlations$maxEV.y)
      )
      cat("Block: ", block_size, "\n")
      cat("k: ", return_length, "\n")
    }
  }
  results
}

data_nested <- data %>%
  group_by(batter_id, year) %>%
  mutate(n = n()) %>%
  filter(n >= 250) %>%
  nest()

player_seasons <- data %>%
  mutate(year = as_factor(year(game_date))) %>%
  group_by(batter_id, year) %>%
  mutate(med_ev = median(launch_speed),
         top50 = if_else(launch_speed >= med_ev, 1, 0)) %>% # For best speed
  group_by(batter_id, year) %>%
  summarise(balls_in_play = n(),
            wOBACON = sum(woba_value) / sum(woba_denom),
            best_speed = sum(launch_speed * top50) / sum(top50),
            avgEV = mean(launch_speed),
            maxEV = max(launch_speed),
            ev80 = quantile(launch_speed, 0.8),
            ev90 = quantile(launch_speed, 0.9),
            ev95 = quantile(launch_speed, 0.95)) %>%
  filter(balls_in_play >= 250)

season_pairs <- get_season_pairs(player_seasons, 2015, 2024)

results <- tuning_matrix(data_nested, season_pairs, block_sizes = c(5, 10, 15, 20), k = c(5, 10, 15, 20))

results %>% ggplot(aes(x = block_size, y = k, size = correlation.wOBACON)) +
  geom_point()

results %>% ggplot(aes(x = block_size, y = k, size = correlation.maxEV)) +
  geom_point()

