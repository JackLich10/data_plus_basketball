# scrape player stats
library(tidyverse)
library(rvest)
library(robotstxt)

paths_allowed("https://www.basketball-reference.com")

scrape_player_stats <- function(x){
  y <- read_html(x)
  game_number <- y %>%
    html_nodes("tbody th") %>%
    html_text() %>%
    as.numeric()
  date <- y %>%
    html_nodes(".right+ .left a") %>%
    html_text()
  opponent <- y %>%
    html_nodes(".left~ .left+ .left a") %>%
    html_text()
  result <- y %>%
    html_nodes("tbody .left:nth-child(7)") %>%
    html_text()
  start <- y %>%
    html_nodes("tbody .left+ .right") %>%
    html_text()
  minutes <- y %>%
    html_nodes("tbody .right:nth-child(9)") %>%
    html_text() %>%
    as.numeric()
  fg <- y %>%
    html_nodes("tbody .right:nth-child(10)") %>%
    html_text() %>%
    as.numeric()
  fga <- y %>%
    html_nodes("tbody .right:nth-child(11)") %>%
    html_text() %>%
    as.numeric()
  two_fg <- y %>%
    html_nodes("tbody .right:nth-child(13)") %>%
    html_text() %>%
    as.numeric()
  two_fga <- y %>%
    html_nodes("tbody .right:nth-child(14)") %>%
    html_text() %>%
    as.numeric()
  three_fg <- y %>%
    html_nodes("tbody .right:nth-child(16)") %>%
    html_text() %>%
    as.numeric()
  three_fga <- y %>%
    html_nodes("tbody .right:nth-child(17)") %>%
    html_text() %>%
    as.numeric()
  ft <- y %>%
    html_nodes("tbody .right:nth-child(19)") %>%
    html_text() %>%
    as.numeric()
  fta <- y %>%
    html_nodes("tbody .right:nth-child(20)") %>%
    html_text() %>%
    as.numeric()
  o_reb <- y %>%
    html_nodes("tbody .right:nth-child(22)") %>%
    html_text() %>%
    as.numeric()
  d_reb <- y %>%
    html_nodes("tbody .right:nth-child(23)") %>%
    html_text() %>%
    as.numeric()
  tot_reb <- y %>%
    html_nodes("tbody .right:nth-child(24)") %>%
    html_text() %>%
    as.numeric()
  ast <- y %>%
    html_nodes("tbody .right:nth-child(25)") %>%
    html_text() %>%
    as.numeric()
  stl <- y %>%
    html_nodes("tbody .right:nth-child(26)") %>%
    html_text() %>%
    as.numeric()
  blk <- y %>%
    html_nodes("tbody .right:nth-child(27)") %>%
    html_text() %>%
    as.numeric()
  tov <- y %>%
    html_nodes("tbody .right:nth-child(28)") %>%
    html_text() %>%
    as.numeric()
  foul <- y %>%
    html_nodes("tbody .right:nth-child(29)") %>%
    html_text() %>%
    as.numeric()
  pts <- y %>%
    html_nodes("tbody .right:nth-child(30)") %>%
    html_text() %>%
    as.numeric()
  tibble(game_number = game_number,
         date = date,
         opponent = opponent,
         result = result,
         start = start,
         minutes = minutes,
         fg = fg,
         fga = fga,
         two_fg = two_fg,
         two_fga = two_fga,
         three_fg = three_fg,
         three_fga = three_fga,
         ft = ft,
         fta = fta,
         o_reb = o_reb,
         d_reb = d_reb,
         tot_reb = tot_reb,
         ast = ast,
         stl = stl,
         blk = blk,
         tov = tov,
         foul = foul,
         pts = pts)
}

add_pct <- function(x, firstName, lastName){
  y <- scrape_player_stats(x)
  y$game_id <- as.numeric(str_replace_all(y$date, "-", ""))
  y$name <- paste(str_to_title(firstName), " ", str_to_title(lastName), sep = "")
  y %>%
    dplyr::mutate(fg_pct = fg/fga,
           two_pct = two_fg/two_fga,
           three_pct = three_fg/three_fga,
           ft_pct = ft/fta,
           PPS_two = two_fg * 2/two_fga,
           PPS_three = three_fg * 3/three_fga,
           PPS = pts/fga) %>%
    dplyr::select(game_number, game_id, name,opponent, result, start, minutes, fg, fga, fg_pct, two_fg, two_fga, two_pct, three_fg, three_fga, three_pct, ft, fta, ft_pct, PPS_two, PPS_three, PPS, o_reb, d_reb, tot_reb, ast, stl, blk, tov, foul, pts)
}

#Enter player name in all lower case
get_full_player_stats <- function(firstName, lastName){
  url <- paste("https://www.sports-reference.com/cbb/players/", firstName, "-", lastName,"-1/gamelog/2015/", sep = "")
  add_pct(url, firstName, lastName)
}

# ZionWilliamsonGameLog <- get_full_player_stats("zion", "williamson")
# RJBarrettGameLog <- get_full_player_stats("rj", "barrett")
# CamReddishGameLog <- get_full_player_stats("cam", "reddish")
# 
# write_csv(ZionWilliamsonGameLog, "data/ZionWilliamsonGameLog.csv")
# write_csv(RJBarrettGameLog, "data/RJBarrettGameLog.csv")
# write_csv(CamReddishGameLog, "data/CamReddishGameLog.csv")

JOGameLog <- get_full_player_stats("jahlil", "okafor")
QCGameLog <- get_full_player_stats("quinn", "cook")
JWGameLog <- get_full_player_stats("justise", "winslow")
TJGameLog <- get_full_player_stats("tyus", "jones")
AJGameLog <- get_full_player_stats("amile", "jefferson")
MJGameLog <- get_full_player_stats("matt", "jones")
GAGameLog <- get_full_player_stats("grayson", "allen")
RSGameLog <- get_full_player_stats("rasheed", "sulaimon")
MPGameLog <- get_full_player_stats("marshall", "plumlee")
SOGameLog <- get_full_player_stats("semi", "ojeleye")
NPGameLog <- get_full_player_stats("nick", "pagliuca")
SKGameLog <- get_full_player_stats("sean", "kelly")

PlayerStats <- bind_rows(JOGameLog, QCGameLog, JWGameLog, TJGameLog,
          AJGameLog, MJGameLog, GAGameLog, RSGameLog,
          MPGameLog, SOGameLog, NPGameLog, SKGameLog)

write_csv(PlayerStats, "data/201415IndividualPlayerStats.csv")

