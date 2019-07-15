get_game_ids <- function(Year) {
  url <- paste("https://www.espn.com/mens-college-basketball/team/schedule/_/id/150/season/", Year, sep = "")
  y <- read_html(url) %>%
    html_nodes(".ml4 a") %>%
    html_attr("href") %>%
    substr(57, 65)
  return(y)
}

gameIDs201415 <- get_game_ids("2015")
gameIDs201516 <- get_game_ids("2016")
gameIDs201617 <- get_game_ids("2017")
gameIDs201718 <- get_game_ids("2018")
gameIDs201819 <- get_game_ids("2019")


