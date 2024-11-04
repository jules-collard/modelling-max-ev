library(tidyverse)

data <- read_csv("data/savant_data_2017_2024.csv")

data %>%
  ggplot(aes(x=launch_speed, fill = ..x..)) +
  geom_histogram(binwidth = 2) +
  labs(x="Exit Velocity (mph)", y="Count") +
  scale_fill_continuous(low="yellow", high="red")

data %>%
  group_by(batter_id, year(game_date), month(game_date)) %>%
  summarise(n = n(),
            max_ev = max(launch_speed)) %>%
  filter(n >= 20) %>%
  ggplot(aes(x=max_ev, fill = ..x..)) +
  geom_histogram(binwidth=1) +
  labs(x="Monthly Max EV", y="Count") +
  scale_fill_continuous(low="yellow", high="red")
