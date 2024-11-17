library(tidyverse)
library(fExtremes)
library(extRemes)
library(ismev)
source("scripts/setup.r")

data <- load_data("data/savant_data_2017_2024.csv", write_names = TRUE)
names <- read_csv("data/batter_names.csv")

get_bm <- function(data, block_size=20) {
  require(fExtremes)
  blockMaxima(data$launch_speed, block=block_size)
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

playerData <- data %>%
  group_by(batter_id) %>%
  mutate(n = n()) %>%
  filter(n >= 600) %>%
  nest() %>%
  mutate(bm = map(data, get_bm),
        gev = map(bm, gev_fit),
        mu = map_dbl(gev, get_mu),
        sigma = map_dbl(gev, get_sigma),
        xi = map_dbl(gev, get_xi)) %>%
  arrange(desc(mu))

bm <- blockMaxima(data_judge$launch_speed, block=20)
gev <- fevd(bm, type="GEV", method="MLE")
ci(gev, type="return.level", return.period = 20, method="proflik", xrange=c(110, 125))

#Kramer-Von Mises, anderson-dalins
library(evir)
params <- distill(gev)[1:3] # mu sigma xi
transformed <- pgev(bm, xi= params[3], mu=params[1], sigma=params[2])
hist(transformed)
# Should be U[0,1]