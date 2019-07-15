# scrape play-by-play data
library(tidyverse)
library(rvest)
library(robotstxt)

paths_allowed("https://www.espn.com")

duke_players <- c("Duke", "Javin DeLaurier", "RJ Barrett", "Cam Reddish", 
                  "Zion Williamson", "Tre Jones", "Jack White", "Antonio Vrankovic", 
                  "Jordan Goldwire", "Alex O'Connell", "Joey Baker", "Marques Bolden", 
                  "Mike Buckmire", "Brennan Besser", 
                  "Wendell Carter Jr", "Marvin Bagley")
official <- c("Official TV", "End of")
jump_shots <- c("missed Jumper", "missed Layup", "missed Three", "missed Two", "missed Dunk", "made Jumper", "made Layup", "made Three", "made Two", "made Dunk") 
made_shots <- c("made Jumper", "made Layup", "made Three", "made Two", "made Dunk")

scrape_pbp <- function(x){
  y <- read_html(x)
  score <- y %>%
    html_nodes(".combined-score") %>%
    html_text()
  play <- y %>%
    html_nodes("#gamepackage-qtrs-wrap .game-details") %>%
    html_text()
  time <- y %>%
    html_nodes(".time-stamp") %>%
    html_text()
  tibble(time = time,
         play = play,
         score = score)
}

separate_col <- function(x){
  y <- scrape_pbp(x)
  y %>%
    separate(col = score, into = c("home_score", "away_score" ), sep = '[-]', convert = TRUE) %>%
    separate(col = time, into = c("min", "sec"), sep = '[:]', convert = TRUE) %>%
    mutate(time = min * 60 + sec) %>%
    select(time, play, home_score, away_score)
}

add_team <- function(x){
  y <- separate_col(x)
  y %>%
    mutate(team = case_when(
      str_detect(play, paste(duke_players, collapse = "|")) ~ "Duke",
      str_detect(play, paste(official, collapse = "|")) ~ "Official",
      TRUE ~ "Opponent"
    )) %>%
    select(time, team, play, home_score, away_score)
}

add_possession_time <- function(x){x
  y <- add_team(x)
  time_diff <- -diff(y$time)
  time_diff <- time_diff %>%
    append(0, after = 0)
  y %>%
    add_column(time_diff) %>%
    mutate(time_diff = case_when(
      time_diff == 0 ~ 0,
      time_diff < 0 ~ 0,
      time_diff > 0 ~ time_diff
    )) %>%
    select(time, time_diff, team, play, home_score, away_score)
}

#charts full play-by-play data
full_pbp <- function(x) {
  y <- add_possession_time(x)
  y %>%
    mutate(
      asts = case_when(
        str_detect(play, "Assist") ~ 1,
        TRUE ~ 0),
      off_reb = case_when(
        str_detect(play, "Offensive Rebound") ~ 1,
        TRUE ~ 0),
      def_reb = case_when(
        str_detect(play, "Defensive Rebound") ~ 1,
        TRUE ~ 0),
      fgm = case_when(
        str_detect(play, paste(made_shots, collapse = "|")) ~ 1,
        TRUE ~ 0),
      fga = case_when(
        str_detect(play, paste(jump_shots, collapse = "|")) ~ 1,
        TRUE ~ 0),
      three_fgm = case_when(
        str_detect(play, "made Three") ~ 1,
        TRUE ~ 0),
      three_fga = case_when(
        str_detect(play, paste(c("made Three", "missed Three"), collapse = "|")) ~ 1,
        TRUE ~ 0),
      ftm = case_when(
        str_detect(play, "made Free") ~ 1,
        TRUE ~ 0),
      fta = case_when(
        str_detect(play, paste(c("made Free", "missed Free"), collapse = "|")) ~ 1,
        TRUE ~ 0),
      stl = case_when(
        str_detect(play, "Steal") ~ 1,
        TRUE ~ 0),
      blk = case_when(
        str_detect(play, "Block") ~ 1,
        TRUE ~ 0),
      to = case_when(
        str_detect(play, "Turnover") ~ 1,
        TRUE ~ 0),
      foul = case_when(
        str_detect(play, "Foul") ~ 1,
        TRUE ~ 0),
      PPS = case_when(
        three_fgm == 1 ~ 3,
        fgm == 1 ~ 2,
        ftm == 1 ~ 1,
        fga == 1 ~ 0,
        TRUE ~ NaN)) %>%
    select(time, time_diff, team, play, home_score, away_score, fgm, fga, three_fgm, three_fga, ftm, fta, PPS, asts, off_reb, def_reb, stl, blk, to, foul)
}
#gets team box score stats
team_stats <- function(x){
  y <- full_pbp(x)
  opp_pts <- y %>%
    filter(team == "Opponent") %>%
    summarise(fg = sum(fgm),
              three_fg = sum(three_fgm),
              ft = sum(ftm)) %>%
    mutate(pts = three_fg * 3 + (fg - three_fg) * 2 + ft) %>%
    select(pts) %>%
    pull()
  y %>%
    filter(team == "Duke") %>%
    summarise(fg = sum(fgm),
              fga = sum(fga),
              three_fg = sum(three_fgm),
              three_fga = sum(three_fga),
              ft = sum(ftm),
              fta = sum(fta),
              asts = sum(asts),
              o_reb = sum(off_reb),
              d_reb = sum(def_reb),
              stl = sum(stl),
              blk = sum(blk),
              to = sum(to),
              foul = sum(foul)) %>%
    mutate(fg_pct = fg/fga,
           three_pct = three_fg/three_fga,
           ft_pct = ft/fta,
           tot_reb = o_reb + d_reb,
           pts = three_fg * 3 + (fg - three_fg) * 2 + ft,
           PPS_two = (fg - three_fg) * 2/ (fga-three_fga),
           PPS_three = three_fg * 3/three_fga,
           PPS = pts/fga) %>%
    add_column(opp_pts) %>%
    select(pts, opp_pts, fg, fga, fg_pct, three_fg, three_fga, three_pct, ft, fta, ft_pct, PPS, PPS_two, PPS_three, asts, o_reb, d_reb, tot_reb, stl, blk, to, foul)
}

get_full_pbp <- function(gameID){
  url <- paste("https://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameID,"", sep = "")
  full_pbp(url)
}

get_team_stats <- function(gameID){
  url <- paste("https://www.espn.com/mens-college-basketball/playbyplay?gameId=", gameID,"", sep = "")
  team_stats(url)
}

sum <- 0
for (i in 1:length(gameIDs)) {
  sum <- nrow(get_full_pbp(gameIDs[i])) + sum
  n <- sum
}

scrape_n_pbp <- tibble(time = rep(NA, n),
                       time_diff = rep(NA, n),
                       team = rep(NA, n),
                       play = rep(NA, n),
                       home_score = rep(NA, n),
                       away_score = rep(NA, n),
                       fgm = rep(NA, n),
                       fga = rep(NA, n),
                       three_fgm = rep(NA, n),
                       three_fga = rep(NA, n),
                       ftm = rep(NA, n),
                       fta = rep(NA, n),
                       PPS = rep(NA, n),
                       asts = rep(NA, n),
                       o_reb = rep(NA, n),
                       d_reb = rep(NA, n),
                       stl = rep(NA, n),
                       blk = rep(NA, n),
                       to = rep(NA, n),
                       foul = rep(NA, n),
                       game_number = rep(NA, n))
new <- 0
for(i in 1:length(gameIDs)){
  num_rows <- nrow(get_full_pbp(gameID = gameIDs[i]))
  scrape_n_pbp[new + 1:num_rows + new,] <- get_full_pbp(gameID = gameIDs[i]) %>%
    mutate(game_number = i)
  print(new + 1)
  print(num_rows + new)
  new <- num_rows + new
}

scrape_n_pbp <- scrape_n_pbp %>%
  drop_na(-PPS)

n <- length(gameIDs)
scrape_n_games <- tibble(pts = rep(NA, n),
                         opp_pts = rep(NA, n),
                         fgm = rep(NA, n),
                         fga = rep(NA, n),
                         fg_pct = rep(NA, n),
                         three_fgm = rep(NA, n),
                         three_fga = rep(NA, n),
                         three_pct = rep(NA, n),
                         ftm = rep(NA, n),
                         fta = rep(NA, n),
                         ft_pct = rep(NA, n),
                         PPS = rep(NA, n),
                         PPS_two = rep(NA, n),
                         PPS_three = rep(NA, n),
                         asts = rep(NA, n),
                         o_reb = rep(NA, n),
                         d_reb = rep(NA, n),
                         tot_reb = rep(NA, n),
                         stl = rep(NA, n),
                         blk = rep(NA, n),
                         to = rep(NA, n),
                         foul = rep(NA, n))

for (i in 1:n){
  print(i)
  scrape_n_games[i,] <- get_team_stats(gameIDs[i])
}

scrape_n_pbp <- as.data.frame(scrape_n_pbp)

scrape_n_games <- as.data.frame(scrape_n_games)

write_csv(scrape_n_pbp, path = "data/Duke201819pbp.csv")
write_csv(scrape_n_games, path = "data/Duke201819teamstats.csv")
