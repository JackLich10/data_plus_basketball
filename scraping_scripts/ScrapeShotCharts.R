library(tidyverse)
library(rvest)
library(stringr)

stripwhite <- function(x) {
  gsub("\\s*$", "", gsub("^\\s*", "", x))
}

get_shot_locs <- function(GameID) {
  if(any(is.na(GameID))) {
    error("GameID missing with no default")
  }
  n <- length(GameID)
  for(i in 1:n) {
    message(paste("Getting Shots for Game", i, "of", n))
    url = paste0('http://www.espn.com/mens-college-basketball/playbyplay?gameId=', GameID[i])
    y <- scan(url, what = "", sep = "\n")[8]
    y <- unlist(strsplit(y, "-"))
    date <- stripwhite(y[length(y) - 1])
    date <- as.Date(date, "%B %d, %Y")
    
    away_team_name <-
      stringr::str_replace_all(xml2::read_html(url) %>% 
                                 rvest::html_nodes(".away h3") %>% 
                                 rvest::html_text(), "[\r\n\t]" , "")
    
    ## if not equal to 1, then print this
    if (length(away_team_name) == 0) {
      message("No shot location data available for this game.")
    }
    else {
      away_shot_text <- xml2::read_html(url) %>% 
        rvest::html_nodes(".away-team li") %>% 
        rvest::html_text()
      
      ## Style, get shot location data from here
      away_shot_style <- xml2::read_html(url) %>% 
        rvest::html_nodes(".away-team li") %>% 
        xml2::xml_attr("style")
      away_color <- gsub("^.*border-color:\\s*|\\s*;.*$", "", away_shot_style[1])
      
      ### home text
      home_team_name <- stringr::str_replace_all(xml2::read_html(url) %>% 
                                                   rvest::html_nodes(".home h3") %>% 
                                                   rvest::html_text(), "[\r\n\t]" , "")
      home_shot_text <- xml2::read_html(url) %>% 
        rvest::html_nodes(".home-team li") %>% 
        rvest::html_text()
      
      ## Style, get shot location data from here
      home_shot_style <- xml2::read_html(url) %>% 
        rvest::html_nodes(".home-team li") %>% 
        xml2::xml_attr("style")
      home_color <- gsub("^.*border-color:\\s*|\\s*;.*$", "", home_shot_style[1])
      
      away_df <- data.frame(
        team_name = away_team_name,
        shot_text = away_shot_text,
        shot_style = away_shot_style,
        color = away_color,
        stringsAsFactors = F
      )
      
      home_df <- data.frame(
        team_name = home_team_name,
        shot_text = home_shot_text,
        shot_style = home_shot_style,
        color = home_color,
        stringsAsFactors = F
      )
      
      total_df = rbind(away_df, home_df)
      
      total_df <-total_df %>%
        mutate(
          "date" = date,
          "outcome" = ifelse(grepl("made", shot_text), "made", "missed"),
          "shooter" = stripwhite(gsub("made.*", "", shot_text)),
          "shooter" = stripwhite(gsub("missed.*", "", shooter)),
          "asissted" = stripwhite(gsub(".{1}$", "", gsub(".*Assisted by", "", shot_text))),
          "asissted" = stripwhite(ifelse(grepl("made", asissted) |
                                           grepl("missed", asissted), NA, asissted)),
          "three_pt" = grepl("Three Point", shot_text),
          "x" = as.numeric(gsub('^.*top:\\s*|\\s*%;.*$', '', total_df$shot_style)) * 0.5,
          "y" = as.numeric(gsub('^.*left:\\s*|\\s*%;top.*$', '', total_df$shot_style)) * .94
        ) %>% 
        dplyr::select(-shot_style)
      
      if (!exists("total_df_all")) {
        total_df_all <- total_df
      }
      else {
        total_df_all <- rbind(total_df_all, total_df)
      }
    }
  }
  
  if (!exists("total_df_all")) {
    return(NULL)
  }
  return(total_df_all)
}

sum <- 0
for (i in 1:length(gameIDs201415)) {
  sum <- as.integer(nrow(get_shot_locs(gameIDs201415[i]))) + sum
  n <- sum
  print(n)
}

scrape_n_shot_locs <- tibble(team_name = rep(NA, n),
                       shot_text = rep(NA, n),
                       color = rep(NA, n),
                       date = rep(NA, n),
                       outcome = rep(NA, n),
                       shooter = rep(NA, n),
                       assisted = rep(NA, n),
                       three_pt = rep(NA, n),
                       x = rep(NA, n),
                       y = rep(NA, n),
                       game_number = rep(NA, n),
                       season = rep(NA, n))
new <- 0
for(i in 1:length(gameIDs201415)){
  num_rows <- nrow(get_shot_locs(gameIDs201415[i]))
  scrape_n_shot_locs[new + 1:num_rows + new,] <- get_shot_locs(gameIDs201415[i]) %>%
    mutate(game_number = i,
           season = "2014-15")
  print(new + 1)
  print(num_rows + new)
  new <- num_rows + new
}

scrape_n_shot_locs <- scrape_n_shot_locs %>%
  drop_na(-assisted)

Duke201415 <- scrape_n_shot_locs

a <- bind_rows(Duke201415, Duke201516, Duke201617, Duke201718, Duke201819)

write_csv(a, path = "data/Duke201419ShotCharts.csv")


              