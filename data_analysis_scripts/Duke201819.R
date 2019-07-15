# load packages
library(tidyverse)
library(ggalt)

# load data
Duke201819pbp <- read_csv("data/Duke201819pbp.csv")
Duke201819teamstats <- read_csv("data/Duke201819teamstats.csv")

# create mean stats
meanPPS <- Duke201819teamstats %>%
  summarise(meanPPS = mean(PPS))%>%
  pull()

mean_eff_fg <- Duke201819teamstats %>%
  summarise(mean_eff_fg = mean(eff_fg_pct)) %>%
  pull()

# add conferences
Duke201819pbp <- Duke201819pbp%>%
  mutate(type = case_when(
    game_number %in% c(1:12) ~ "Non-Conference",
    game_number %in% c(13:20) ~ "Conference",
    game_number == 21 ~ "Non-Conference",
    game_number %in% c(22:31) ~ "Conference",
    game_number %in% c(32:34) ~ "ACC Tournament",
    game_number %in% c(35:38) ~ "NCAA Tournament"
  ))

# add possession length
Duke201819pbp <- Duke201819pbp %>%
  mutate(possession_length = case_when(
    !is.na(PPS) & time_diff %in% c(0:8) ~ "First 8 seconds",
    !is.na(PPS) & time_diff %in% c(9:25) ~ "Average",
    !is.na(PPS) & time_diff %in% c(26:40) ~ "Last 5 Seconds"
  ))

avg_poss_length <- Duke201819pbp %>%
  filter(!is.na(PPS)) %>%
  group_by(game_number, type, team) %>%
  dplyr::summarise(avg_poss_length = mean(time_diff))

poss_length_Duke <- avg_poss_length %>%
  ungroup() %>%
  filter(team == "Duke") %>%
  dplyr::select(avg_poss_length) %>%
  pull()

poss_length_opponent <- avg_poss_length %>%
  ungroup() %>%
  filter(team == "Opponent") %>%
  dplyr::select(avg_poss_length) %>%
  pull()

# Duke's PPS by game
Duke201819teamstats %>%
  ggplot(aes(x = reorder(opponent, PPS), y = PPS, fill = result)) + 
  geom_col() +
  coord_flip() +
  geom_hline(yintercept = meanPPS, linetype = "dashed") +
  geom_text(aes(label = round(PPS, 2)), hjust = "top", size = 3) +
  geom_text(x = length(Duke201819teamstats$game_number) - 28, y = meanPPS + .2, label = paste("Mean: ", as.character(round(meanPPS, 1)), " PPS", sep = ""), size = 3) +
  theme_classic() +
  labs(title = "Duke's Points Per Shot by Game", subtitle = "2018-2019 Season",
       x = "Opponent", y = "Points Per Shot", fill = "Result")

# Duke's game-by-game shooting
Duke201819teamstats %>%
  mutate(color = (min(eff_fg_pct) == eff_fg_pct | max(eff_fg_pct) == eff_fg_pct)) %>%
  ggplot(aes(x = reorder(opponent, game_number), y = eff_fg_pct, group = 1)) + 
  geom_line() + 
  geom_point(aes(color = color)) + 
  geom_hline(yintercept = mean_eff_fg, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("black", "red")) +
  geom_text(x = length(Duke201819teamstats$game_number) - 2, y = mean_eff_fg - .05, label = paste("Mean: ", as.character(round(100 * mean_eff_fg, 1)), "%", sep = ""), size = 3) +
  geom_text(x = length(Duke201819teamstats$game_number) - 12, y = max(Duke201819teamstats$eff_fg_pct), label = paste("Max: ", as.character(round(100 * max(Duke201819teamstats$eff_fg_pct), 1)), "%", sep = ""), size = 3) +
  geom_text(x = length(Duke201819teamstats$game_number) - 4, y = min(Duke201819teamstats$eff_fg_pct), label = paste("Min: ", as.character(round(100 * min(Duke201819teamstats$eff_fg_pct), 1)), "%", sep = ""), size = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(color = F) +
  labs(title = "Duke's Game-by-Game Shooting", subtitle = "2018-19 Season",
       x = "Opponent", y = "eFG%")

# Duke's PPS by game type
Duke201819teamstats %>%
  group_by(type) %>%
  summarise(mean = mean(PPS)) %>%
  ggplot(aes(x = reorder(type, mean), y = mean, fill = type)) + 
  geom_col() +
  geom_text(aes(label = round(mean, 2)), position = position_dodge(0.9), vjust = "bottom") +
  scale_color_manual(values = c("grey", "grey", "red", "grey"), aesthetics = "fill") +
  labs(title = "Duke's PPS by Game Type", subtitle = "2018-19 Season", x = "Game Type",
       y = "Points Per Shot", fill = "Game Type")

# Duke's PPS ny possession length
Duke201819pbp %>%
  filter(team == "Duke", !is.na(PPS)) %>%
  group_by(possession_length) %>%
  summarise(mean = mean(PPS)) %>%
  ggplot(aes(x = reorder(possession_length, mean), y = mean, fill = mean)) + 
  geom_col() +
  geom_text(aes(label = round(mean, 2)), position = position_dodge(0.9), vjust = "bottom") +
  labs(title = "Duke's PPS by Possession Length", subtitle = "Duke 2018-2019 Season", x = "Possession Length", y = "Points Per Shot", fill = "Points Per Shot")

# Duke's game-by-game possession length
Duke201819teamstats %>%
  ggplot(aes(x = game_number, y = poss_length_Duke, color = type, shape = result)) + 
  geom_point() +
  scale_shape_manual(values = c(4, 16)) +
  geom_smooth(data = Duke201819teamstats, mapping = aes(x = game_number, y = poss_length_Duke), inherit.aes = FALSE, method = "loess", se = FALSE) +
  labs(title = "Duke plays at slower pace as season progresses", subtitle = "During 2018-19 Season", x = "Game Number", y = "Average Possesion Length (s)", color = "Type", shape = "Result")

# Duke's possession length compared to opponents
Duke201819teamstats %>%
  ggplot(aes(x = poss_length_Duke, xend = poss_length_opp, y = game_number, color = result)) + 
  coord_flip() +
  geom_dumbbell(colour_x = "blue", colour_xend = "#a3c4dc", size_x = 2, size_xend = 2, show.legend = T) +
  labs(title = "Duke plays at a faster pace than their opponents", subtitle = "During 2018-19 Season", x = "Average Possession Length (s)", y = "Game Number", color = "Result")

