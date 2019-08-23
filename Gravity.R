# load packages
library(tidyverse)
library(readr)

# load data
Duke201415teamstats <- read_csv("data/Duke201415teamstats.csv")
ShotChart <- read_csv("data/shot_chart_NN_SVM.csv")
PlayerStats <- read_csv("data/201415IndividualPlayerStats.csv") #individual gamelogs (simple box scores)
IndividualGravity <- read_csv("data/distance_gravity_calculations_newest_bounds.csv") #game_id, player_id, frames in each zone, gravity in each zone
Gravity <- read_csv("data/gravity_with_players_changed_zones.csv")  #game_id, possession, gravity 5 zones, pps, epps, 5 players on court

# change playerID to player name
Gravity <- Gravity %>%
  dplyr::mutate(p1 = case_when(
    p1 == 601140 ~ "Quinn Cook",
    p1 == 601143 ~ "Marshall Plumlee",
    p1 == 696290 ~ "Amile Jefferson",
    p1 == 696289 ~ "Rasheed Sulaimon",
    p1 == 756883 ~ "Matt Jones",
    p1 == 786843 ~ "Nick Pagliuca",
    p1 == 792415 ~ "Semi Ojeleye",
    p1 == 842296 ~ "Grayson Allen",
    p1 == 842297 ~ "Tyus Jones",
    p1 == 842298 ~ "Justise Winslow",
    p1 == 842301 ~ "Jahlil Okafor",
    p1 == 842313 ~ "Sean Kelly"),
    p2 = case_when(
      p2 == 601140 ~ "Quinn Cook",
      p2 == 601143 ~ "Marshall Plumlee",
      p2 == 696290 ~ "Amile Jefferson",
      p2 == 696289 ~ "Rasheed Sulaimon",
      p2 == 756883 ~ "Matt Jones",
      p2 == 786843 ~ "Nick Pagliuca",
      p2 == 792415 ~ "Semi Ojeleye",
      p2 == 842296 ~ "Grayson Allen",
      p2 == 842297 ~ "Tyus Jones",
      p2 == 842298 ~ "Justise Winslow",
      p2 == 842301 ~ "Jahlil Okafor",
      p2 == 842313 ~ "Sean Kelly"),
    p3 = case_when(
      p3 == 601140 ~ "Quinn Cook",
      p3 == 601143 ~ "Marshall Plumlee",
      p3 == 696290 ~ "Amile Jefferson",
      p3 == 696289 ~ "Rasheed Sulaimon",
      p3 == 756883 ~ "Matt Jones",
      p3 == 786843 ~ "Nick Pagliuca",
      p3 == 792415 ~ "Semi Ojeleye",
      p3 == 842296 ~ "Grayson Allen",
      p3 == 842297 ~ "Tyus Jones",
      p3 == 842298 ~ "Justise Winslow",
      p3 == 842301 ~ "Jahlil Okafor",
      p3 == 842313 ~ "Sean Kelly"),
    p4 = case_when(
      p4 == 601140 ~ "Quinn Cook",
      p4 == 601143 ~ "Marshall Plumlee",
      p4 == 696290 ~ "Amile Jefferson",
      p4 == 696289 ~ "Rasheed Sulaimon",
      p4 == 756883 ~ "Matt Jones",
      p4 == 786843 ~ "Nick Pagliuca",
      p4 == 792415 ~ "Semi Ojeleye",
      p4 == 842296 ~ "Grayson Allen",
      p4 == 842297 ~ "Tyus Jones",
      p4 == 842298 ~ "Justise Winslow",
      p4 == 842301 ~ "Jahlil Okafor",
      p4 == 842313 ~ "Sean Kelly"),
    p5 = case_when(
      p5 == 601140 ~ "Quinn Cook",
      p5 == 601143 ~ "Marshall Plumlee",
      p5 == 696290 ~ "Amile Jefferson",
      p5 == 696289 ~ "Rasheed Sulaimon",
      p5 == 756883 ~ "Matt Jones",
      p5 == 786843 ~ "Nick Pagliuca",
      p5 == 792415 ~ "Semi Ojeleye",
      p5 == 842296 ~ "Grayson Allen",
      p5 == 842297 ~ "Tyus Jones",
      p5 == 842298 ~ "Justise Winslow",
      p5 == 842301 ~ "Jahlil Okafor",
      p5 == 842313 ~ "Sean Kelly"
    ))

# filter out gravity values below 0
Gravity <- Gravity %>%
  dplyr::mutate(shot_making = pps - epps)

mean_grav <- Gravity %>%
  dplyr::group_by(game) %>%
  summarise(grav1 = mean(avg_one),
            grav2 = mean(avg_two),
            grav3 = mean(avg_three),
            grav4 = mean(avg_four),
            grav5 = mean(avg_five))

# find ePPS for neural net and support vector machine
ShotChart <- ShotChart %>%
  mutate(EPS = value * NN_probability)

ePPS <- ShotChart %>%
  dplyr::group_by(game) %>%
  dplyr::summarise(ePPS = mean(EPS)) %>%
  dplyr::select(ePPS) %>%
  pull()

ePPS_SVM <- ShotChart %>%
  dplyr::group_by(game) %>%
  dplyr::summarise(ePPS = mean(EPS_SVM)) %>%
  dplyr::select(ePPS) %>%
  pull()

ePPS = (ePPS_NN + ePPS_SVM)/2

Duke201415teamstats <- Duke201415teamstats %>%
  mutate(PPS = (pts - ft) /fga)

# filter for games we have SportVu data for
subset <- Duke201415teamstats %>%
  filter(game_number %in% c(1:7, 9, 11:13, 16, 18, 22, 23, 26, 27, 29, 30, 32:35)) %>%
  dplyr::mutate(shot_making = PPS - ePPS)

subset <- subset %>%
  dplyr::mutate(ePPS = ePPS)

ShotChart <- ShotChart %>%
  dplyr::mutate(ePPS = NN_probability * value)

# wide to long transformation
long <- subset %>%
  dplyr::select(game_number, opponent, PPS, ePPS, shot_making) %>%
  gather(type, value, -game_number, -opponent, -shot_making)

subset <- subset %>%
  mutate(pt_diff = pts - opp_pts)

subset <- bind_cols(subset, mean_grav)

# write csv
write_csv(subset, path = "data/201415GameLog.csv")

# filter player stats for games we have SportVu data
subsetPlayerStats <- PlayerStats %>%
  filter(game_id %in% c(20141114, 20141115, 20141118, 20141121, 20141122, 20141126,
                        20141130, 20141215, 20141229, 20141231, 20150103,
                        20150113, 20150119, 20150204, 20150207, 20150218,
                        20150221, 20150228, 20150304, 20150312, 20150313,
                        20150320, 20150322))

# change from playerID to names
IndividualGravity <- IndividualGravity %>%
  dplyr::mutate(name = case_when(
    player == 601140 ~ "Quinn Cook",
    player == 601143 ~ "Marshall Plumlee",
    player == 696290 ~ "Amile Jefferson",
    player == 696289 ~ "Rasheed Sulaimon",
    player == 756883 ~ "Matt Jones",
    player == 786843 ~ "Nick Pagliuca",
    player == 792415 ~ "Semi Ojeleye",
    player == 842296 ~ "Grayson Allen",
    player == 842297 ~ "Tyus Jones",
    player == 842298 ~ "Justise Winslow",
    player == 842301 ~ "Jahlil Okafor",
    player == 842313 ~ "Sean Kelly"
  )) %>%
  filter(player != 621266)
  
# create variable to join data frames by
subsetPlayerStats$join <- paste(subsetPlayerStats$game_id, subsetPlayerStats$name, sep = "")
IndividualGravity$join <- paste(IndividualGravity$game, IndividualGravity$name, sep = "")

# join data frames (putting players stats per game with their gravity scores in that game)
PlayerGravity <- full_join(subsetPlayerStats, IndividualGravity, by = c("join"))

# changing variable name.x to name
PlayerGravity$name <- PlayerGravity$name.x

# adding positions to players
PlayerGravity <- PlayerGravity %>%
  dplyr::mutate(
    position = case_when(
      name %in% c("Quinn Cook", "Tyus Jones", "Matt Jones", "Grayson Allen", "Rasheed Sulaimon", "Nick Pagliuca", "Sean Kelly") ~ "G",
      name %in% c("Jahlil Okafor", "Marshall Plumlee") ~ "C",
      TRUE ~ "F"
    ))

PlayerGravity <- PlayerGravity %>%
  dplyr::select(-c(name.y, name.x ,X1, player, join, game))

# get rid of zone defense
PlayerGravity <- PlayerGravity %>%
  filter(opponent != "Syracuse")

PlayerGravity$offball_zone_one <- str_remove(PlayerGravity$offball_zone_one, pattern = "[()]")
PlayerGravity$offball_zone_two <- str_remove(PlayerGravity$offball_zone_two, pattern = "[()]")
PlayerGravity$offball_zone_three <- str_remove(PlayerGravity$offball_zone_three, pattern = "[()]")
PlayerGravity$offball_zone_four <- str_remove(PlayerGravity$offball_zone_four, pattern = "[()]")
PlayerGravity$offball_zone_five <- str_remove(PlayerGravity$offball_zone_five, pattern = "[()]")
PlayerGravity$ball_one <- str_remove(PlayerGravity$ball_one, pattern = "[()]")
PlayerGravity$ball_two <- str_remove(PlayerGravity$ball_two, pattern = "[()]")
PlayerGravity$ball_three <- str_remove(PlayerGravity$ball_three, pattern = "[()]")
PlayerGravity$ball_four <- str_remove(PlayerGravity$ball_four, pattern = "[()]")
PlayerGravity$ball_five <- str_remove(PlayerGravity$ball_five, pattern = "[()]")

PlayerGravity <- PlayerGravity %>%
  separate(offball_zone_one, into = c("offball_zone_one", "frames_one"), sep = "[,]", convert = TRUE) %>%
  separate(offball_zone_two, into = c("offball_zone_two", "frames_two"), sep = "[,]", convert = TRUE) %>%
  separate(offball_zone_three, into = c("offball_zone_three", "frames_three"), sep = "[,]", convert = TRUE) %>%
  separate(offball_zone_four, into = c("offball_zone_four", "frames_four"), sep = "[,]", convert = TRUE) %>%
  separate(offball_zone_five, into = c("offball_zone_five", "frames_five"), sep = "[,]", convert = TRUE) %>%
  separate(ball_one, into = c("ball_one", "frames_one_ball"), sep = "[,]", convert = TRUE) %>%
  separate(ball_two, into = c("ball_two", "frames_two_ball"), sep = "[,]", convert = TRUE) %>%
  separate(ball_three, into = c("ball_three", "frames_three_ball"), sep = "[,]", convert = TRUE) %>%
  separate(ball_four, into = c("ball_four", "frames_four_ball"), sep = "[,]", convert = TRUE) %>%
  separate(ball_five, into = c("ball_five", "frames_five_ball"), sep = "[,]", convert = TRUE)

# get rid of gravities that are exactly 0
PlayerGravity <- PlayerGravity %>%
  dplyr::mutate(offball_zone_one = case_when(
    offball_zone_one == 0 ~ NaN,
    TRUE ~ offball_zone_one),
    offball_zone_two = case_when(
      offball_zone_two == 0 ~ NaN,
      TRUE ~ offball_zone_two),
    offball_zone_three = case_when(
      offball_zone_three == 0 ~ NaN,
      TRUE ~ offball_zone_three),
    offball_zone_four = case_when(
      offball_zone_four == 0 ~ NaN,
      TRUE ~ offball_zone_four),
    offball_zone_five = case_when(
      offball_zone_five == 0 ~ NaN,
      TRUE ~ offball_zone_five),
    ball_one = case_when(
      ball_one == 0 ~ NaN,
      TRUE ~ ball_one),
    ball_two = case_when(
      ball_two == 0 ~ NaN,
      TRUE ~ ball_two),
    ball_three = case_when(
      ball_three == 0 ~ NaN,
      TRUE ~ ball_three),
    ball_four = case_when(
      ball_four == 0 ~ NaN,
      TRUE ~ ball_four),
    ball_five = case_when(
      ball_five == 0 ~ NaN,
      TRUE ~ ball_five
    ))

# use weighted means to find season gravity totals by zone using the number of frames each player was in that zone
SeasonGravityTotals <- PlayerGravity %>%
  dplyr::group_by(name) %>%
  summarise(weighted.mean(ball_one, frames_one_ball, na.rm = T),
            weighted.mean(ball_two, frames_two_ball, na.rm = T),
            weighted.mean(ball_three, frames_three_ball, na.rm = T),
            weighted.mean(ball_four, frames_four_ball, na.rm = T),
            weighted.mean(ball_five, frames_five_ball, na.rm = T),
            weighted.mean(offball_zone_one, frames_one, na.rm = T),
            weighted.mean(offball_zone_two, frames_two, na.rm = T),
            weighted.mean(offball_zone_three, frames_three, na.rm = T),
            weighted.mean(offball_zone_four, frames_four, na.rm = T),
            weighted.mean(offball_zone_five, frames_five, na.rm = T))

colnames(SeasonGravityTotals)[2:11] <- c("w_ball_one", "w_ball_two", "w_ball_three", "w_ball_four",
                                         "w_ball_five", "w_off_one", "w_off_two", "w_off_three",
                                         "w_off_four", "w_off_five")

# create player gravity stats for each player's season
SeasonPlayerGravityZones <- PlayerGravity %>%
  dplyr::group_by(name) %>%
  summarise(frames_three = sum(frames_three, na.rm = T),
            frames_four = sum(frames_four, na.rm = T),
            frames_five = sum(frames_five, na.rm = T),
            frames_three_ball = sum(frames_three_ball, na.rm = T),
            frames_four_ball = sum(frames_four_ball, na.rm = T),
            frames_five_ball = sum(frames_five_ball, na.rm = T),
            minutes = sum(minutes),
            fg = sum(fg),
            fga = sum(fga),
            two_fg = sum(two_fg),
            two_fga = sum(two_fga),
            three_fg = sum(three_fg),
            three_fga = sum(three_fga),
            ft = sum(ft),
            fta = sum(fta),
            o_reb = sum(o_reb),
            d_reb = sum(d_reb),
            tot_reb = sum(tot_reb),
            ast = sum(ast),
            stl = sum(stl),
            blk = sum(blk),
            tov = sum(tov),
            foul = sum(foul),
            pts = sum(pts), na.rm = TRUE) %>%
  dplyr::mutate(games = games,
                fg_pct = fg/fga,
                two_pct = two_fg/two_fga,
                three_pct = three_fg/three_fga,
                ft_pct = ft/fta,
                PPS_two = two_fg * 2/two_fga,
                PPS_three = three_fg * 3/three_fga,
                PPS = pts/fga,
                pts_game = pts/games,
                fga_game = fga/games,
                three_game = three_fga/games,
                two_game = two_fga/games,
                position = case_when(
                  name %in% c("Quinn Cook", "Tyus Jones", "Matt Jones", "Grayson Allen", "Rasheed Sulaimon", "Nick Pagliuca", "Sean Kelly") ~ "G",
                  name %in% c("Jahlil Okafor", "Marshall Plumlee") ~ "C",
                  TRUE ~ "F"
                )) %>%
  dplyr::select(name, games, position, minutes, fg, fga, fg_pct, fga_game, two_fg, two_fga, two_game, two_pct, three_fg, three_fga, three_pct, three_game, ft, fta, ft_pct, PPS_two, PPS_three, PPS, o_reb, d_reb, tot_reb, ast, stl, blk, tov, foul, pts, pts_game, 
                frames_three_ball, frames_four_ball, frames_five_ball,
                frames_three, frames_four, frames_five)

Totals <- bind_cols(SeasonPlayerGravityZones, SeasonGravityTotals)

# get rid of extra name column and zones 1 and 2
Totals <- Totals %>%
  filter(minutes > 60) %>%
  dplyr::select(-c(name1, w_ball_one, w_ball_two, w_off_one, w_off_two))

write_csv(Totals, path = "data/SeasonPlayerZoneGravities.csv")

# get rid of zones 1 and 2
PlayerGravity <- PlayerGravity %>%
  dplyr::select(-c(ball_one, frames_one_ball, ball_two, frames_two_ball, offball_zone_one, 
                   frames_one, offball_zone_two, frames_two))

# find number of games played by player
games <- PlayerGravity %>%
  count(name) %>%
  dplyr::select(n) %>%
  pull()

# prepare for lineup data
Gravity <- Gravity %>%
  dplyr::mutate(
    JO_flag = case_when(
      p1 == "Jahlil Okafor" |  p2 == "Jahlil Okafor" | p3 == "Jahlil Okafor" | p4 == "Jahlil Okafor" | p5 == "Jahlil Okafor" ~ "Yes",
      TRUE ~ "No"),
    JW_flag = case_when(
      p1 == "Justise Winslow" |  p2 == "Justise Winslow" | p3 == "Justise Winslow" | p4 == "Justise Winslow" | p5 == "Justise Winslow" ~ "Yes",
      TRUE ~ "No"),
    QC_flag = case_when(
      p1 == "Quinn Cook" |  p2 == "Quinn Cook" | p3 == "Quinn Cook" | p4 == "Quinn Cook" | p5 == "Quinn Cook" ~ "Yes",
      TRUE ~ "No"),
    TJ_flag = case_when(
      p1 == "Tyus Jones" |  p2 == "Tyus Jones" | p3 == "Tyus Jones" | p4 == "Tyus Jones" | p5 == "Tyus Jones" ~ "Yes",
      TRUE ~ "No"),
    AJ_flag = case_when(
      p1 == "Amile Jefferson" |  p2 == "Amile Jefferson" | p3 == "Amile Jefferson" | p4 == "Amile Jefferson" | p5 == "Amile Jefferson" ~ "Yes",
      TRUE ~ "No"),
    GA_flag = case_when(
      p1 == "Grayson Allen" |  p2 == "Grayson Allen" | p3 == "Grayson Allen" | p4 == "Grayson Allen" | p5 == "Grayson Allen" ~ "Yes",
      TRUE ~ "No"),
    RS_flag = case_when(
      p1 == "Rasheed Sulaimon" |  p2 == "Rasheed Sulaimon" | p3 == "Rasheed Sulaimon" | p4 == "Rasheed Sulaimon" | p5 == "Rasheed Sulaimon" ~ "Yes",
      TRUE ~ "No"),
    MJ_flag = case_when(
      p1 == "Matt Jones" |  p2 == "Matt Jones" | p3 == "Matt Jones" | p4 == "Matt Jones" | p5 == "Matt Jones" ~ "Yes",
      TRUE ~ "No"),
    NP_flag = case_when(
      p1 == "Nick Pagliuca" |  p2 == "Nick Pagliuca" | p3 == "Nick Pagliuca" | p4 == "Nick Pagliuca" | p5 == "Nick Pagliuca" ~ "Yes",
      TRUE ~ "No"),
    SK_flag = case_when(
      p1 == "Sean Kelly" |  p2 == "Sean Kelly" | p3 == "Sean Kelly" | p4 == "Sean Kelly" | p5 == "Sean Kelly" ~ "Yes",
      TRUE ~ "No"),
    MP_flag = case_when(
      p1 == "Marshall Plumlee" |  p2 == "Marshall Plumlee" | p3 == "Marshall Plumlee" | p4 == "Marshall Plumlee" | p5 == "Marshall Plumlee" ~ "Yes",
      TRUE ~ "No"),
    SO_flag = case_when(
      p1 == "Semi Ojeleye" |  p2 == "Semi Ojeleye" | p3 == "Semi Ojeleye" | p4 == "Semi Ojeleye" | p5 == "Semi Ojeleye" ~ "Yes",
      TRUE ~ "No"
    ))

# create dataset of different lineups
Lineups <- Gravity %>%
  dplyr::group_by(JO_flag, JW_flag, QC_flag, TJ_flag, AJ_flag, GA_flag, RS_flag, NP_flag, MJ_flag, SK_flag, MP_flag, SO_flag) %>%
  add_tally() %>%
  summarise(epps = mean(epps),
            pps = mean(pps),
            grav3 = mean(avg_three),
            grav4 = mean(avg_four),
            grav5 = mean(avg_five),
            tally = mean(n))

# write csv of season stats
write_csv(SeasonPlayerGravity, path = "data/201415SeasonGravity.csv")

# write csv file of game by game player gravity
write_csv(PlayerGravity, path = "data/201415PlayerStatsWithGravity.csv")

# write csv file of lineup data
write_csv(Lineups, path = "data/201415LineupsGravity.csv")
