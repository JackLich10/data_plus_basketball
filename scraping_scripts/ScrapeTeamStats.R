# scrape play-by-play data
library(tidyverse)
library(rvest)
library(robotstxt)

page <- read_html("https://www.sports-reference.com/cbb/schools/duke/2015-gamelogs.html")

scrape_season_stats <- function(x){
    y <- read_html(x)
    opponent <- y %>%
      html_nodes("td:nth-child(4)") %>%
      html_text()
    result <- y %>%
      html_nodes(".left:nth-child(5)") %>%
      html_text()
    pts <- y %>%
      html_nodes(".right:nth-child(6)") %>%
      html_text() %>%
      as.numeric()
    opp_pts <- y %>%
      html_nodes(".right:nth-child(7)") %>%
      html_text() %>%
      as.numeric()
    fg <- y %>%
      html_nodes(".right:nth-child(8)") %>%
      html_text() %>%
      as.numeric()
    fga <- y %>%
      html_nodes(".right:nth-child(9)") %>%
      html_text() %>%
      as.numeric()
    three_fg <- y %>%
      html_nodes(".right:nth-child(11)") %>%
      html_text() %>%
      as.numeric()
    three_fga <- y %>%
      html_nodes(".right:nth-child(12)") %>%
      html_text() %>%
      as.numeric()
    ft <- y %>%
      html_nodes(".right:nth-child(14)") %>%
      html_text() %>%
      as.numeric()
    fta <- y %>%
      html_nodes(".right:nth-child(15)") %>%
      html_text() %>%
      as.numeric()
    o_reb <- y %>%
      html_nodes(".right:nth-child(17)") %>%
      html_text() %>%
      as.numeric()
    tot_reb <- y %>%
      html_nodes(".right:nth-child(18)") %>%
      html_text() %>%
      as.numeric()
    ast <- y %>%
      html_nodes(".right:nth-child(19)") %>%
      html_text() %>%
      as.numeric()
    stl <- y %>%
      html_nodes(".right:nth-child(20)") %>%
      html_text() %>%
      as.numeric()
    blk <- y %>%
      html_nodes(".right:nth-child(21)") %>%
      html_text() %>%
      as.numeric()
    tov <- y %>%
      html_nodes(".right:nth-child(22)") %>%
      html_text() %>%
      as.numeric()
    foul <- y %>%
      html_nodes(".right:nth-child(23)") %>%
      html_text() %>%
      as.numeric()
    tibble(opponent = opponent,
           result = result,
           pts = pts,
           opp_pts = opp_pts,
           fg = fg,
           fga = fga,
           three_fg = three_fg,
           three_fga = three_fga,
           ft = ft,
           fta = fta,
           o_reb = o_reb,
           tot_reb = tot_reb,
           ast = ast,
           stl = stl,
           blk = blk,
           tov = tov,
           foul = foul)
  }

Duke201415teamstats <- scrape_season_stats("https://www.sports-reference.com/cbb/schools/duke/2015-gamelogs.html")

Duke201415teamstats <- Duke201415teamstats %>%
  add_column(game_number = 1:nrow(Duke201415teamstats), .before = 1) %>%
  mutate(fg_pct = fg/fga,
       three_pct = three_fg/three_fga,
       ft_pct = ft/fta,
       d_reb = tot_reb - o_reb,
       PPS_two = (fg - three_fg) * 2/(fga-three_fga),
       PPS_three = three_fg * 3/three_fga,
       PPS = pts/fga,
       eff_fg_pct = (fg + 0.5 * three_fg)/fga) %>%
  dplyr::select(game_number, opponent, result, pts, opp_pts, fg, fga, fg_pct, three_fg, three_fga, three_pct,
         PPS_two, PPS_three, PPS, eff_fg_pct, ft, fta, o_reb, d_reb, tot_reb, ast, stl, blk, tov, foul)

Duke201415teamstats <- as.data.frame(Duke201415teamstats)

write_csv(Duke201415teamstats, "data/Duke201415teamstats.csv")

