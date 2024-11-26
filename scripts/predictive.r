library(tidyverse)
library(corrr)
source("scripts/utils.r")

data <- load_data("data/savant_data_2015_2024.csv", write_names = TRUE)
names <- read_csv("data/batter_names.csv")

# Get EV Metrics for each player by season
player_seasons <- get_player_seasons(data, 250, 10)
season_pairs <- get_season_pairs(player_seasons, 2015, 2024)
playerModels <- get_player_models(data, 250) %>%
  select(-c(bm, gev))

correlations <- season_pairs %>%
  inner_join(playerModels, by = join_by(batter_id == batter_id, year.x == year)) %>%
  correlate() %>%
  select(term, wOBACON.x, wOBACON.y, maxEV.y) %>%
  mutate(term = str_replace(term, "\\.x", "")) %>%
  rename(stat.x = term) %>%
  filter(stat.x %in% c("avgEV", "best_speed", "maxEV", "ev80", "ev90", "ev95", "return_level")) %>%
  mutate(highlight = as_factor(if_else(stat.x == "return_level", 1, 0)))

# Correlation with wOBACON
correlations %>% ggplot(aes(x = reorder(stat.x, -wOBACON.x), y = wOBACON.x, fill = highlight)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(wOBACON.x, digits=2)), vjust=2) +
  theme_bw() +
  labs(title = "EV Metrics and wOBACON",
        subtitle = "MLB 2015-2024, min. 250 BIP",
        x = "Statistic", y = "Correlation to wOBACON")
ggsave("plots/woba_correlation.png")

# Correlation with next year wOBACON
correlations %>% ggplot(aes(x = reorder(stat.x, -wOBACON.y), y = wOBACON.y, fill = highlight)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(wOBACON.y, digits=2)), vjust=2) +
  theme_bw() +
  labs(title = "EV Metrics and Next Year's wOBACON",
        subtitle = "MLB 2015-2024, min. 250 BIP",
        x = "Statistic", y = "Correlation to Next Year wOBACON")
ggsave("plots/woba_next_correlation.png")

# Correlation with next year maxEV
correlations %>%
  filter(stat.x %in% c("maxEV", "return_level")) %>%
  ggplot(aes(x = stat.x, y = maxEV.y, fill = highlight)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = round(maxEV.y, digits=2)), vjust=2) +
    theme_bw() +
    labs(title = "EV Metrics and Next Year's maxEV",
          subtitle = "MLB 2015-2024, min. 250 BIP",
          x = "Statistic", y = "Correlation to Next Year maxEV")
ggsave("plots/maxev_next_correlation.png")

changes <- season_pairs %>%
  inner_join(playerModels, by = join_by(batter_id == batter_id, year.x == year)) %>%
  rename(return_level.x = return_level) %>%
  inner_join(playerModels, by = join_by(batter_id == batter_id, year.y == year)) %>%
  rename(return_level.y = return_level) %>% # Get return levels for each year
  ungroup() %>%
  select(best_speed.x:ev95.x, return_level.x, best_speed.y:ev95.y, return_level.y) %>%
  mutate(rownum = row_number()) %>% # for pivot_wider later
  pivot_longer(cols = -rownum,
              names_to = c("stat"),
              values_to = "val") %>%
  separate_wider_delim(cols = stat, delim = ".", names = c("stat", "period")) %>%
  mutate(across(c(stat, period), as_factor)) %>%
  group_by(stat) %>%
  mutate(std_dev = sd(val)) %>% # get sd across all players & seasons
  pivot_wider(names_from = period, values_from = val, id_cols = c(rownum, stat, std_dev)) %>%
  mutate(change = (y - x)/std_dev,
          sig_change = if_else(abs(change) >= 1, 1, 0)) %>%
  group_by(stat) %>%
  summarise(pct_change = sum(sig_change) / n()) %>%
  mutate(highlight = as_factor(if_else(stat == "return_level", 1, 0)))

changes %>% ggplot(aes(x=reorder(stat, pct_change), y=pct_change, fill=highlight)) +
  geom_col(show.legend = FALSE) +
  theme_bw() +
  labs(title = "Year-to-Year Stability of EV Metrics",
        subtitle = "MLB 2015-2024, min. 250 BIP",
        y = "% Player Seasons with >1 SD Change",
        x = element_blank()) +
  scale_y_continuous(labels = scales::percent)
ggsave("plots/stability.png")
