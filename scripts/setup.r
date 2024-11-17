clean_savant_data <- function(data_csv) {
  require(dplyr)
  require(tidyr)
  data_csv %>%
    drop_na(launch_speed) %>%
    mutate(across(game_date, ymd)) %>%
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
