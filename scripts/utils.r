# Data Loading & Cleaning
clean_savant_data <- function(data_csv) {
  require(dplyr)
  require(tidyr)
  data_csv %>%
    drop_na(launch_speed) %>%
    mutate(across(game_date, ymd),
            year = as_factor(year(game_date))) %>%
    filter(woba_denom > 0)
}

write_names_csv <- function(data_tibble, path="data/batter_names.csv") {
  require(dplyr)
  require(readr)
  names <- data_tibble %>%
    distinct(batter_id, .keep_all = TRUE) %>%
    select(batter_id, player_name)

  write_csv(names, path)
}

get_name <- function(names_tibble, id) {
  require(dplyr)
  names_tibble %>%
    filter(batter_id == id) %>%
    pull(player_name)
}

load_data <- function(filename, write_names=FALSE) {
  require(readr)
  data <- clean_savant_data(read_csv(filename))

  if (write_names) {
    write_names_csv(data)
  }
  return(data)
}

# GEV Modelling
get_bm <- function(data, block_size=15) {
  require(fExtremes)
  bm <- blockMaxima(data$launch_speed, block=block_size)
  head(bm, -1) # Filter out outlier small blocks
}

gev_fit <- function(bm) {
  require(ismev)
  gev.fit(bm, show=FALSE)
}

get_mu <- function(fit) {
  unlist(fit$mle[1])
}

get_sigma <- function(fit) {
  unlist(fit$mle[2])
}

get_xi <- function(fit) {
  unlist(fit$mle[3])
}

ksTest <- function(quantiles) {
  test <- ks.test(unlist(quantiles), "punif", min = 0, max = 1)
  test$p.value
}

get_return_level <- function(k, mu, sigma, xi) {
  mu + ((sigma/xi)*((-log(1-(1/k)))^(-xi) - 1))
}

# Data Wrangling
get_player_seasons <- function(data, min_bip=250) {
  require(dplyr)
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
    filter(balls_in_play >= min_bip)
  player_seasons
}

get_season_pairs <- function(player_seasons, start_season, end_season) {
  require(dplyr)
  season_pairs <- tibble()
  for (i in 1:(length(start_season:end_season)-1)) {
    season1 <- player_seasons %>% filter(year == start_season + i - 1)
    season2 <- player_seasons %>% filter(year == start_season + i)
    season_pair <- inner_join(season1, season2, by = join_by(batter_id))
    season_pairs <- rbind(season_pairs, season_pair)
  }
  season_pairs
}

get_player_models <- function(data, min_bip=250, block_size=10) {
  require(dplyr)
  player_models <- data %>%
    group_by(batter_id, year) %>%
    mutate(n = n()) %>%
    filter(n >= min_bip) %>%
    nest() %>%
    mutate(bm = map(data, ~get_bm(.x, block_size = block_size)),
          gev = map(bm, gev_fit),
          mu = map_dbl(gev, get_mu),
          sigma = map_dbl(gev, get_sigma),
          xi = map_dbl(gev, get_xi),
          return_level = get_return_level(5, mu, sigma, xi)) %>%
    select(-data)
}
