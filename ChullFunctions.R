# load packages
library(tidyverse)
library(gganimate)
library(rvg)
library(readr)

# load data
SportVuGames <- read_csv("data/SportVuGames.csv")
DukeCHULL <- read_csv("data/DukeConvexHull.csv")
OpponentCHULL <- read_csv("data/OpponentConvexHull.csv")

# function for making video
gravity_video <- function(fileName, Opponent, startFrame, endFrame){
  file <- paste("data/", fileName, ".csv", sep = "")
  poss_data <- read_csv(file)
  colnames(poss_data)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
  poss_data <- poss_data %>%
    dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)
  SportVuPoss <- SportVuGames %>%
    slice(startFrame:endFrame)
  gravity_test <- bind_cols(SportVuPoss, poss_data)
  title <- paste("Duke vs. ", Opponent, sep = "")
  nframes <- (endFrame - startFrame + 1) * 25
  graph <- gravity_test %>%
    ggplot() +
    geom_polygon(data = court %>% filter(side == 2), aes(x = x, y = y, group = group), color = "gray") +
    stat_chull(data = DukeCHULL %>%
                 dplyr::group_by(type) %>%
                 slice(startFrame:endFrame), aes(x = xcoord, y = ycoord), alpha = 0.1, fill = "blue", geom = "polygon") +
    stat_chull(data = OpponentCHULL %>%
                 dplyr::group_by(type) %>%
                 slice(startFrame:endFrame), aes(x = xcoord, y = ycoord), alpha = 0.1, geom = "polygon") +
    geom_point(aes(x = p1_x, y = p1_y), color = "blue", size = 4) +
    geom_text(aes(x = p1_x, y = p1_y - 2, label = as.character(round((p1_grav), 2)))) +
    geom_point(aes(x = p2_x, y = p2_y), color = "blue", size = 4) +
    geom_text(aes(x = p2_x, y = p2_y - 2, label = as.character(round((p2_grav), 2)))) +
    geom_point(aes(x = p3_x, y = p3_y), color = "blue", size = 4) +
    geom_text(aes(x = p3_x, y = p3_y - 2, label = as.character(round((p3_grav), 2)))) +
    geom_point(aes(x = p4_x, y = p4_y), color = "blue", size = 4) +
    geom_text(aes(x = p4_x, y = p4_y - 2, label = as.character(round((p4_grav), 2)))) +
    geom_point(aes(x = p5_x, y = p5_y), color = "blue", size = 4) +
    geom_text(aes(x = p5_x, y = p5_y - 2, label = as.character(round((p5_grav), 2)))) +
    geom_point(aes(x = p6_x, y = p6_y), size = 4) +
    geom_point(aes(x = p7_x, y = p7_y), size = 4) +
    geom_point(aes(x = p8_x, y = p8_y), size = 4) +
    geom_point(aes(x = p9_x, y = p9_y), size = 4) +
    geom_point(aes(x = p10_x, y = p10_y), size = 4) + 
    geom_point(aes(x = ball_x, y = ball_y), color = "orange") +
    geom_text(aes(x = ball_x, y = ball_y + 2, label = str_to_sentence(event.descrip))) +
    geom_text(aes(x = 25, y = 96, label = paste("Player with poss: ", name_lab, sep = ""))) +
    geom_text(aes(x = 25, y = 99, label = paste("Game-Clock: ", time_lab, "   Shot-Clock: ", shot.clock, sep = ""))) +
    geom_text(aes(x = 25, y = 45, label = paste("Total Offensive Gravity: ", round(tot_off_gravity, 2), sep = ""))) +
    geom_text(aes(x = p1_x, y = p1_y, label = p1_pos), color = "white") +
    geom_text(aes(x = p2_x, y = p2_y, label = p2_pos), color = "white") +
    geom_text(aes(x = p3_x, y = p3_y, label = p3_pos), color = "white") +
    geom_text(aes(x = p4_x, y = p4_y, label = p4_pos), color = "white") +
    geom_text(aes(x = p5_x, y = p5_y, label = p5_pos), color = "white") +
    coord_equal(clip = "off") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    transition_time(-game.clock) +
    ease_aes("linear") +
    labs(title = title, subtitle = "Progress: {100 * round(progress, 2)}%")
  
  gganimate::animate(graph, start_pause = 10, end_pause = 10, nframes = nframes, renderer = ffmpeg_renderer())
}

# graph convex hull
convex_hull <- function(Opponent, Frame){
  SportVuPoss <- SportVuGames %>%
    slice(Frame)
  title <- paste("Duke vs. ", Opponent, sep = "")
  SportVuPoss %>%
    ggplot() +
    geom_polygon(data = court, aes(x = x, y = y, group = group), color = "gray") +
    stat_chull(data = DukeCHULL %>%
                 dplyr::group_by(type) %>%
                 slice(Frame), aes(x = xcoord, y = ycoord), alpha = 0.1, fill = "blue", geom = "polygon") +
    stat_chull(data = OpponentCHULL %>%
                 dplyr::group_by(type) %>%
                 slice(Frame), aes(x = xcoord, y = ycoord), alpha = 0.1, geom = "polygon") +
    geom_point(aes(x = p1_x, y = p1_y), color = "blue", size = 4) +
    geom_point(aes(x = p2_x, y = p2_y), color = "blue", size = 4) +
    geom_point(aes(x = p3_x, y = p3_y), color = "blue", size = 4) +
    geom_point(aes(x = p4_x, y = p4_y), color = "blue", size = 4) +
    geom_point(aes(x = p5_x, y = p5_y), color = "blue", size = 4) +
    geom_point(aes(x = p6_x, y = p6_y), size = 4) +
    geom_point(aes(x = p7_x, y = p7_y), size = 4) +
    geom_point(aes(x = p8_x, y = p8_y), size = 4) +
    geom_point(aes(x = p9_x, y = p9_y), size = 4) +
    geom_point(aes(x = p10_x, y = p10_y), size = 4) + 
    geom_point(aes(x = ball_x, y = ball_y), color = "orange") +
    geom_text(aes(x = ball_x, y = ball_y + 2, label = str_to_sentence(event.descrip))) +
    geom_text(aes(x = 25, y = 96, label = paste("Player with poss: ", name_lab, sep = ""))) +
    geom_text(aes(x = 25, y = 99, label = paste("Game-Clock: ", time_lab, "   Shot-Clock: ", shot.clock, sep = ""))) +
    geom_text(aes(x = p1_x, y = p1_y, label = p1_pos), color = "white") +
    geom_text(aes(x = p2_x, y = p2_y, label = p2_pos), color = "white") +
    geom_text(aes(x = p3_x, y = p3_y, label = p3_pos), color = "white") +
    geom_text(aes(x = p4_x, y = p4_y, label = p4_pos), color = "white") +
    geom_text(aes(x = p5_x, y = p5_y, label = p5_pos), color = "white") +
    coord_equal(clip = "off") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = title, subtitle = "Convex Hull Plot")
}

# make video with paired defense
gravity_video_paired_defense <- function(fileName, Opponent, startFrame, endFrame){
  file <- paste("data/", fileName, ".csv", sep = "")
  poss_data <- read_csv(file)
  colnames(poss_data)[c(3, 6, 9, 12, 15)] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
  poss_data <- poss_data %>%
    dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)
  SportVuPoss <- SportVuGames %>%
    slice(startFrame:endFrame)
  gravity_test <- bind_cols(SportVuPoss, poss_data)
  title <- paste("Duke vs. ", Opponent, sep = "")
  nframes <- (endFrame - startFrame + 1) * 25
  graph <- gravity_test %>%
    ggplot() +
    geom_polygon(data = court %>% filter(side == 2), aes(x = x, y = y, group = group), color = "gray") +
    geom_segment(aes(x = p1_x, y = p1_y, xend = p1_x1, yend = p1_y1), color = "red", linetype = "dashed") +
    geom_segment(aes(x = p2_x, y = p2_y, xend = p2_x1, yend = p2_y1), color = "red", linetype = "dashed") +
    geom_segment(aes(x = p3_x, y = p3_y, xend = p3_x1, yend = p3_y1), color = "red", linetype = "dashed") +
    geom_segment(aes(x = p4_x, y = p4_y, xend = p4_x1, yend = p4_y1), color = "red", linetype = "dashed") +
    geom_segment(aes(x = p5_x, y = p5_y, xend = p5_x1, yend = p5_y1), color = "red", linetype = "dashed") +
    geom_point(aes(x = p1_x, y = p1_y), color = "blue", size = 4) +
    geom_text(aes(x = p1_x, y = p1_y - 2, label = as.character(round((p1_grav), 2)))) +
    geom_point(aes(x = p2_x, y = p2_y), color = "blue", size = 4) +
    geom_text(aes(x = p2_x, y = p2_y - 2, label = as.character(round((p2_grav), 2)))) +
    geom_point(aes(x = p3_x, y = p3_y), color = "blue", size = 4) +
    geom_text(aes(x = p3_x, y = p3_y - 2, label = as.character(round((p3_grav), 2)))) +
    geom_point(aes(x = p4_x, y = p4_y), color = "blue", size = 4) +
    geom_text(aes(x = p4_x, y = p4_y - 2, label = as.character(round((p4_grav), 2)))) +
    geom_point(aes(x = p5_x, y = p5_y), color = "blue", size = 4) +
    geom_text(aes(x = p5_x, y = p5_y - 2, label = as.character(round((p5_grav), 2)))) +
    geom_point(aes(x = p6_x, y = p6_y), size = 4) +
    geom_point(aes(x = p7_x, y = p7_y), size = 4) +
    geom_point(aes(x = p8_x, y = p8_y), size = 4) +
    geom_point(aes(x = p9_x, y = p9_y), size = 4) +
    geom_point(aes(x = p10_x, y = p10_y), size = 4) + 
    geom_point(aes(x = ball_x, y = ball_y), color = "orange") +
    geom_text(aes(x = ball_x, y = ball_y + 2, label = str_to_sentence(event.descrip))) +
    geom_text(aes(x = 25, y = 96, label = paste("Player with poss: ", name_lab, sep = ""))) +
    geom_text(aes(x = 25, y = 99, label = paste("Game-Clock: ", time_lab, "   Shot-Clock: ", shot.clock, sep = ""))) +
    geom_text(aes(x = 25, y = 45, label = paste("Total Offensive Gravity: ", round(tot_off_gravity, 2), sep = ""))) +
    geom_text(aes(x = p1_x, y = p1_y, label = p1_pos), color = "white") +
    geom_text(aes(x = p2_x, y = p2_y, label = p2_pos), color = "white") +
    geom_text(aes(x = p3_x, y = p3_y, label = p3_pos), color = "white") +
    geom_text(aes(x = p4_x, y = p4_y, label = p4_pos), color = "white") +
    geom_text(aes(x = p5_x, y = p5_y, label = p5_pos), color = "white") +
    coord_equal(clip = "off") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) +
    transition_time(-game.clock) +
    ease_aes("linear") +
    labs(title = title, subtitle = "Progress: {100 * round(progress, 2)}%")
  
  gganimate::animate(graph, start_pause = 10, end_pause = 10, nframes = nframes, renderer = ffmpeg_renderer())
}

