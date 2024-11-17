library(tidyverse)
source("scripts/setup.r")

data <- load_data("data/savant_data_2017_2024.csv", write_names = TRUE)
names <- read_csv("data/batter_names.csv")

# Overall EV Histogram
data %>%
  mutate(bin = cut(launch_speed, breaks = seq(2.5, 127.5, 5), labels = seq(5, 125, 5))) %>%
  group_by(bin) %>%
  summarise(pitches = n(),
            wOBACON = sum(woba_value) / sum(woba_denom)) %>%
  ggplot(aes(x = bin, y=pitches, fill = wOBACON)) +
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(x = "Launch Speed (mph)")

data %>%
  ggplot(aes(x=launch_speed, fill = ..woba_value..)) +
  geom_histogram(binwidth = 2, col = "white") +
  labs(x="Exit Velocity (mph)", y="Count",
      title="Histogram of Exit Velocities", subtitle = "MLB Balls in Play 2017-2024") +
  scale_fill_continuous(low="yellow", high="red")

# Player
data %>%
  filter(batter_id == 592450) %>% # Aaron Judge
  ggplot(aes(x=launch_speed, fill = after_stat(x))) +
  geom_histogram(binwidth = 2, col = "white") +
  labs(x="Exit Velocity (mph)", y="Count",
      title="Histogram of Exit Velocities",
      subtitle=get_name(names, 592450)) +
  scale_fill_continuous(low="yellow", high="red")

data %>%
  filter(batter_id == 650333) %>% # Luis Arraez
  ggplot(aes(x=launch_speed, fill = after_stat(x))) +
  geom_histogram(binwidth = 2, col = "white") +
  labs(x="Exit Velocity (mph)", y="Count",
      title="Histogram of Exit Velocities",
      subtitle=get_name(names, 650333)) +
  scale_fill_continuous(low="yellow", high="red")

# Overlaid
data %>%
  filter(batter_id %in% c(650333, 519317, 592450)) %>% # Luis Arraez & Giancarlo Stanton
  ggplot(aes(x=launch_speed, fill = player_name, y = ..density..)) +
  geom_density(alpha=0.7, col="white") +
  labs(x="Exit Velocity (mph)", y="",
      title="Histogram of Exit Velocities",
      fill = "Player") +
  scale_x_continuous(limits=c(25,130))

# EV Time Series Plot
data %>%
  filter(batter_id == 592450, year(game_date) == 2024) %>% # Aaron Judge 2024
  arrange(game_date, at_bat_number) %>%
  mutate(at_bat = row_number()) %>%
  ggplot(aes(x=at_bat, y=launch_speed)) +
  geom_segment(aes(xend=at_bat), yend=0) +
  scale_y_continuous(limits=c(100, 120))

acf(data %>% filter(batter_id == 592450) %>% select(launch_speed),
    lag.max = 30, plot = FALSE)

data %>% filter(year(game_date) == 2024) %>%
  ggplot(aes(y = as_factor(woba_value), x = launch_speed)) +
  geom_violin()
