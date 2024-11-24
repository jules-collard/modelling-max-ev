library(tidyverse)
source("scripts/utils.r")

data <- load_data("data/savant_data_2015_2024.csv", write_names = TRUE)
# write_names_csv(data, path="data/batter_names.csv")
names <- read_csv("data/batter_names.csv")

# Overall EV Histogram with wOBACON
data %>%
  mutate(bin = cut(launch_speed, breaks = seq(2.5, 127.5, 5), labels = seq(5, 125, 5))) %>%
  group_by(bin) %>%
  summarise(pitches = n(),
            wOBACON = sum(woba_value) / sum(woba_denom)) %>%
  ggplot(aes(x = bin, y=pitches, fill = wOBACON)) +
  geom_bar(stat = "identity") +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  labs(x = "Launch Speed (mph)", title = "Histogram of Exit Velocities",
        subtitle = "MLB Balls in Play 2015-2024") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank())
ggsave("plots/evHistogram.png")

# wOBACON vs EV Line Graph
data %>%
  mutate(bin = cut(launch_speed, breaks = seq(0.5, 127.5, 1), labels = seq(1, 127, 1))) %>%
  group_by(bin) %>%
  summarise(wOBACON = sum(woba_value) / sum(woba_denom)) %>%
  mutate(across(bin, as.numeric)) %>%
  ggplot(aes(x = bin, y = wOBACON)) +
  geom_smooth(se=FALSE) +
  labs(x = "Launch Speed (mph)",
        title = "Importance of Exit Velocity for Batted Ball Outcomes",
        subtitle = "MLB Balls in Play 2015-2024") +
  theme_bw()
ggsave("plots/ev_woba.png")

# Aaron Judge & Luis Arraez
data %>%
  filter(batter_id == 592450 | batter_id == 650333) %>%
  left_join(names, by = join_by(batter_id), keep = FALSE) %>%
  ggplot(aes(x=launch_speed, fill = after_stat(x))) +
  facet_wrap(~player_name.x) +
  geom_histogram(binwidth = 2, col = "white", show.legend = FALSE) +
  labs(x="Launch Speed (mph)", y="Balls in Play",
      title="Histogram of Exit Velocities") +
  scale_fill_continuous(low="yellow", high="red") +
  theme_bw()
ggsave("plots/playerComparison.png")

# Overlaid Density Plot
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
