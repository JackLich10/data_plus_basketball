library(ggpubr)
library(ggcorrplot)
library(readr)
SportVuGames <- read_csv("data/games_1415.csv")

# Track passes and dribbles per game
passes <- SportVuGames %>%
  filter(event.id == 22 & home == "yes" & game_id != 20150117 & game_id != 20150125) %>%
  group_by(game_id) %>%
  count(event.id) %>%
  dplyr::select(n) %>%
  pull()

dribbles <- SportVuGames %>%
  filter(event.id == 21 & home == "yes" & game_id != 20150117 & game_id != 20150125) %>%
  group_by(game_id) %>%
  count(event.id) %>%
  dplyr::select(n) %>%
  pull()

poss <- read_html("https://www.sports-reference.com/cbb/schools/duke/2015-gamelogs-advanced.html") %>%
  html_nodes("td:nth-child(10)") %>%
  html_text() %>%
  as.numeric()

poss <- poss[c(1:7, 9, 11:13, 16, 18, 22, 23, 26, 27, 29, 30, 32:35)]

subset <- subset %>%
  mutate(passes = passes,
         dribbles = dribbles,
         poss = poss,
         p_poss = passes/poss,
         d_poss = dribbles/poss)

b <- subset %>%
  dplyr::select(-c(1:3), -shot_making_NN, -shot_making_SVM, -ePPS_NN, -ePPS_SVM, -tooltip,
                -gameID, -onclick, -tooltip_ast)

# Correlation matrix
corr <- round(cor(b), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

# Create data frames for convex hull plots
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

SportVuGames$name_lab <- paste(str_replace(SportVuGames$name, pattern = "[.]", replacement = " "))
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
SportVuGames <- SportVuGames %>%
  dplyr::mutate(sec = as.character(sec),
                sec = case_when(
                  as.numeric(sec) < 10 & as.numeric(sec) > 1 ~ paste("0", SportVuGames$sec, sep = ""),
                  TRUE ~ sec
                ))
SportVuGames$time_lab <- paste(SportVuGames$min, ":", as.character(SportVuGames$sec), sep = "")

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

# Plot convex hulls
p <- SportVuGames %>%
  slice(343:359) %>%
  ggplot() +
  geom_polygon(data = court %>% filter(side == 2), aes(x = x, y = y, group = group), color = "gray") +
  stat_chull(data = xy1 %>%
               dplyr::group_by(type) %>%
               slice(343:359), aes(x = xcoord, y = ycoord), alpha = 0.1, fill = "blue", geom = "polygon") +
  stat_chull(data = xy2 %>%
               dplyr::group_by(type) %>%
               slice(343:359), aes(x = xcoord, y = ycoord), alpha = 0.1, geom = "polygon") +
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
  labs(title = "Convex Hull Area Plot", subtitle = "Progress: {100 * round(progress, 2)}%")

gganimate::animate(p, start_pause = 5, end_pause = 5, nframes = 200, renderer = ffmpeg_renderer())

# Testing gravity on a possession
poss_data <- read_csv("data/possession_test.csv")
poss_data2 <- read_csv("data/possession_test2.csv")
poss_data3 <- read_csv("data/possession_test3.csv")
poss_data4 <- read_csv("data/possession_test4.csv")
poss_data7 <- read_csv("data/possession_test7.csv")

colnames(poss_data)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
colnames(poss_data2)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
colnames(poss_data3)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
colnames(poss_data4)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")
colnames(poss_data7)[3:7] <- c("p1_grav", "p2_grav", "p3_grav", "p4_grav", "p5_grav")

ND_poss <- SportVuGames %>%
  slice(69276:69291)

PB_poss <- SportVuGames %>%
  slice(425:433)

FF_poss <- SportVuGames %>%
  slice(5383:5408)

poss_data <- poss_data %>%
  dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)

poss_data2 <- poss_data2 %>%
  dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)

poss_data3 <- poss_data3 %>%
  dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)

poss_data4 <- poss_data4 %>%
  dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)

poss_data7 <- poss_data7 %>%
  dplyr::mutate(tot_off_gravity = p1_grav + p2_grav + p3_grav + p4_grav + p5_grav)

gravity_test <- bind_cols(ND_poss, poss_data)
gravity_test2 <- bind_cols(PB_poss, poss_data2)
gravity_test3 <- bind_cols(FF_poss, poss_data3)
gravity_test4 <- bind_cols(FF_poss, poss_data4)
gravity_test7 <- bind_cols(FF_poss, poss_data7)

p2 <- gravity_test3 %>%
  slice(1:14) %>%
  ggplot() +
  geom_polygon(data = court, aes(x = x, y = y, group = group), color = "gray") +
  stat_chull(data = xy1 %>%
               dplyr::group_by(type) %>%
               slice(5383:5396), aes(x = xcoord, y = ycoord), alpha = 0.1, fill = "blue", geom = "polygon") +
  stat_chull(data = xy2 %>%
               dplyr::group_by(type) %>%
               slice(5383:5396), aes(x = xcoord, y = ycoord), alpha = 0.1, geom = "polygon") +
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
  labs(title = "Duke vs. Fairfield", subtitle = "Progress: {100 * round(progress, 2)}%")

gganimate::animate(p2, start_pause = 10, end_pause = 10, nframes = 350, renderer = ffmpeg_renderer())

