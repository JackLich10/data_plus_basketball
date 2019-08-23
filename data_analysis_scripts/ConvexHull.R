# load packages
library(ggpubr)
library(ggcorrplot)
library(readr)
library(tidyverse)
library(rvest)
source("ChullFunctions.R")

# load data
SportVuGames <- read_csv("data/SportVuGames.csv")
DukeCHULL <- read_csv("data/DukeConvexHull.csv")
OpponentCHULL <- read_csv("data/OpponentConvexHull.csv")

# Plot convex hulls
convex_hull(Opponent = "Presbyterian", Frame = 353)

# Testing gravity on a possession
gravity_video(fileName = "poss_test_21", Opponent = "Notre Dame", startFrame = 69276, endFrame = 69291)

# Interesting possessions
NotreDame_poss <- SportVuGames %>%
  slice(69276:69291)

Presbyterian_poss <- SportVuGames %>%
  slice(425:433)

Fairfield_poss <- SportVuGames %>%
  slice(5383:5408)
