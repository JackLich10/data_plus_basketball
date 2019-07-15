# scrape play-by-play data
library(tidyverse)
library(rvest)
library(robotstxt)

paths_allowed("https://www.espn.com")

jets_players <- c("S.Darnold", "B.Powell", "I.Crowell", "T.Cannon", "J.Mccown", 
                  "B.Winters", "J.Carpenter", "S.Long", "B.Shell", "B.Qvale",
                  "J.Harrison", "K.Beachum", "E.McGuire", "R.Anderson",
                  "A.Roberts right end", "D.Henderson", "Q.Enunwa",
                  "J.Myers", "L.Edwards", "A.Roberts left end", "Jason Myers")
official <- c("Two Minute Warning", "Timeout")
touch_made <- "extra point is GOOD"
touch_miss <- "PAT failed"
fg <- c("Field Goal", "field goal is GOOD")
safety <- "Safety"

nfl_pbp <- function(gameID) {
  url <- paste("https://www.espn.com/nfl/playbyplay?gameId=", gameID,"", sep = "")
  y <- read_html(url)
  down_dist <- y %>%
    html_nodes("#gamepackage-drives-wrap h3") %>%
    html_text(trim = T)
  play <- y %>%
    html_nodes(".drive-list .post-play") %>%
    html_text(trim = T)
  data <- tibble(down_dist = down_dist, 
                 play = play)
  data <- data %>%
    separate(col = play, into = c("time", "play" ), sep = '[)]', extra = "merge")
  data <- data %>%
    separate(col = time, into = c("time", "quarter"), sep = '[-]')
  data <- data %>%
    separate(col = down_dist, into = c("down", "ball_on"), sep = "at")
  data$time <- str_replace_all(data$time, '[(]', "")
  data <- data %>%
    separate(col = down, into = c("down", "dist"), sep = "&")
  data <- data %>%
    mutate(team = case_when(
    str_detect(play, paste(jets_players, collapse = "|")) ~ "NY Jets",
    str_detect(play, paste(official, collapse = "|")) ~ "Official",
    TRUE ~ "Opponent"),
           jets = case_when(
    team == "NY Jets" & str_detect(play, touch_made) ~ 7,
    team == "NY Jets" & str_detect(play, touch_miss) ~ 6,
    team == "NY Jets" & str_detect(play, paste(fg, collapse = "|")) ~ 3,
    team == "NY Jets" & str_detect(play, safety) ~ 2,
    TRUE ~ 0),
           opp = case_when(
    team == "Opponent" & str_detect(play, touch_made) ~ 7,
    team == "Opponent" & str_detect(play, touch_miss) ~ 6,
    team == "Opponent" & str_detect(play, paste(fg, collapse = "|")) ~ 3,
    team == "Opponent" & str_detect(play, safety) ~ 2,
    TRUE ~ 0))
  data <- data %>%
    mutate(jets_pts = cumsum(jets),
           opp_pts = cumsum(opp))
  data <- data %>%
    mutate_all(~replace(., is.na(.), ""))
  data$quarter <- stringr::str_trim(data$quarter)
  data$time <- stringr::str_trim(data$time)
  data$ball_on <- stringr::str_trim(data$ball_on)
  data$play <- stringr::str_trim(data$play)
  data$down <- stringr::str_trim(data$down)
  data$dist <- stringr::str_trim(data$dist)
  data %>%
    dplyr::select(quarter, time, jets_pts, opp_pts, team, ball_on, down, dist, play)
}

b <- nfl_pbp(NYJgameIDs[1])

nyj_get_game_ids <- function(Year) {
  url <- paste("https://www.espn.com/nfl/team/schedule/_/name/nyj/season/", Year, sep = "")
  y <- read_html(url) %>%
    html_nodes(".ml4 a") %>%
    html_attr("href") %>%
    substr(39, 48)
  return(y)
}
NYJgameIDs <- nyj_get_game_ids(2018)

NYJgameIDs <- NYJgameIDs[-c(17:20)]

sum <- 0
for (i in 1:length(NYJgameIDs)) {
  sum <- nrow(nfl_pbp(NYJgameIDs[i])) + sum
  n <- sum
  print(sum)
}

scrape_n_pbp <- tibble(quarter = rep(NA, n),
                       time = rep(NA, n),
                       jets_pts = rep(NA, n),
                       opp_pts = rep(NA, n),
                       team = rep(NA, n),
                       ball_on = rep(NA, n),
                       down = rep(NA, n),
                       dist = rep(NA, n),
                       play = rep(NA, n),
                       game_number = rep(NA, n))

new <- 0
for(i in 1:length(NYJgameIDs)){
  num_rows <- nrow(nfl_pbp(gameID = NYJgameIDs[i]))
  scrape_n_pbp[new + 1:num_rows + new,] <- nfl_pbp(gameID = NYJgameIDs[i]) %>%
    mutate(game_number = i)
  print(new + 1)
  print(num_rows + new)
  new <- num_rows + new
}

scrape_n_pbp <- scrape_n_pbp %>%
  drop_na()

scrape_n_pbp$jets_pts <- as.numeric(scrape_n_pbp$jets_pts)
scrape_n_pbp$opp_pts <- as.numeric(scrape_n_pbp$opp_pts)

scrape_n_pbp$ball_on <- str_trim(scrape_n_pbp$ball_on)

scrape_n_pbp <- scrape_n_pbp %>%
  mutate(pt_diff = jets_pts - opp_pts)

write_csv(scrape_n_pbp, path = "data/NYJets201819pbp.csv")

a <- scrape_n_pbp %>%
  filter(team == "NY Jets" & down == "4th") %>%
  mutate(punt_flag = case_when(
    str_detect(play, "punts") ~ 1,
    TRUE ~ 0),
         field = case_when(
    str_detect(ball_on, "NYJ") ~ "Own",
    str_detect(ball_on, "50") ~ "Midfield",
    TRUE ~ "Opponent"
         ),
    dist_to_end = )

c <- scrape_n_pbp %>%
  separate(col = ball_on, into = c("side", "ball_on"), sep = " ", convert = T, extra = "merge", fill = "right")


