# load packages
library(tidyverse)
library(readr)

# load data
SportVuGames <- read_csv("data/games_1415.csv")

# create convex hull data frames
x1 <- SportVuGames %>%
  dplyr::select(p1_x, p2_x, p3_x, p4_x, p5_x, game_id, game.clock) %>%
  gather(type, xcoord, -game_id, -game.clock)

y1 <- SportVuGames %>%
  dplyr::select(p1_y, p2_y, p3_y, p4_y, p5_y, game_id, game.clock) %>%
  gather(type, ycoord, -game_id, -game.clock)

xy1 <- bind_cols(x1, y1)

x2 <- SportVuGames %>%
  dplyr::select(p6_x, p7_x, p8_x, p9_x, p10_x, game_id, game.clock) %>%
  gather(type, xcoord, -game_id, -game.clock)

y2 <- SportVuGames %>%
  dplyr::select(p6_y, p7_y, p8_y, p9_y, p10_y, game_id, game.clock) %>%
  gather(type, ycoord, -game_id, -game.clock)

xy2 <- bind_cols(x2, y2)

write_csv(xy1, path = "data/DukeConvexHull.csv")
write_csv(xy2, path = "data/OpponentConvexHull.csv")

# create new name label
SportVuGames$name_lab <- paste(str_replace(SportVuGames$name, pattern = "[.]", replacement = " "))

# add minutes, seconds and opponent names
SportVuGames <- SportVuGames %>%
  dplyr::mutate(min = floor(game.clock/60),
                sec = round(game.clock - (min * 60)),
                opponent = case_when(
                  game_id == 20141114 ~ "Presbyterian",
                  game_id == 20141115 ~ "Farfield",
                  game_id == 20141118 ~ "Michigan State",
                  game_id == 20141121 ~ "Temple",
                  game_id == 20141122 ~ "Stanford",
                  game_id == 20141126 ~ "Furman",
                  game_id == 20141130 ~ "Army",
                  game_id == 20141215 ~ "Elon",
                  game_id == 20141229 ~ "Toledo",
                  game_id == 20141231 ~ "Wofford",
                  game_id == 20150103 ~ "Boston College",
                  game_id == 20150113 ~ "Miami (FL)",
                  game_id == 20150117 ~ "Louisville",
                  game_id == 20150119 ~ "Pittsburgh",
                  game_id == 20150125 ~ "St. John's",
                  game_id == 20150204 ~ "Georgia Tech",
                  game_id == 20150207 ~ "Notre Dame",
                  game_id == 20150218 ~ "UNC",
                  game_id == 20150221 ~ "Clemson",
                  game_id == 20150228 ~ "Syracuse",
                  game_id == 20150304 ~ "Wake Forest",
                  game_id == 20150312 ~ "NC State",
                  game_id == 20150313 ~ "Notre Dame",
                  game_id == 20150320 ~ "Robert Morris",
                  game_id == 20150322 ~ "San Diego State"
                ))
# format seconds to have 0s for digits between 1 and 10
SportVuGames <- SportVuGames %>%
  dplyr::mutate(sec = as.character(sec),
                sec = case_when(
                  as.numeric(sec) < 10 & as.numeric(sec) > 1 ~ paste("0", SportVuGames$sec, sep = ""),
                  TRUE ~ sec
                ))
SportVuGames$time_lab <- paste(SportVuGames$min, ":", as.character(SportVuGames$sec), sep = "")

# add positions
SportVuGames <- SportVuGames %>%
  dplyr::mutate(
    p1_pos = case_when(
      p1_global_id %in% c("601140", "842297", "842296", "696289", "842313", "786843", "756883") ~ "G",
      p1_global_id %in% c("842301", "601143") ~ "C",
      TRUE ~ "F"),
    p2_pos = case_when(
      p2_global_id %in% c("601140", "842297", "842296", "696289", "842313", "786843", "756883") ~ "G",
      p2_global_id %in% c("842301", "601143") ~ "C",
      TRUE ~ "F"),
    p3_pos = case_when(
      p3_global_id %in% c("601140", "842297", "842296", "696289", "842313", "786843", "756883") ~ "G",
      p3_global_id %in% c("842301", "601143") ~ "C",
      TRUE ~ "F"),
    p4_pos = case_when(
      p4_global_id %in% c("601140", "842297", "842296", "696289", "842313", "786843", "756883") ~ "G",
      p4_global_id %in% c("842301", "601143") ~ "C",
      TRUE ~ "F"),
    p5_pos = case_when(
      p5_global_id %in% c("601140", "842297", "842296", "696289", "842313", "786843", "756883") ~ "G",
      p5_global_id %in% c("842301", "601143") ~ "C",
      TRUE ~ "F"))

write_csv(SportVuGames, path = "data/SportVuGames.csv")

