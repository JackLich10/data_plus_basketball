library(tidyverse)
library(ggExtra)

Duke <- read_csv("data/Duke201419ShotCharts.csv")
Duke <- Duke %>%
  filter(team_name == "Duke")

counts <- Duke %>%
  filter(season == "2014-15") %>%
  dplyr::count(shooter)

Duke <- Duke %>%
  filter(season == "2014-15")

Duke <- Duke %>%
  mutate(new_x = case_when(
    y >= 47 ~ 50 - x,
    y < 47 ~ x),
    new_y = case_when(
      y >= 47 ~ 94 - y,
      y < 47 ~ y
    ))

distance_to_hoop <- function(x, y) {
  pointDistance(c(x,y), c(25, 5.25), lonlat = F)
}

one_side <- Duke %>%
  rowwise() %>%
  dplyr::mutate(shot_zone = case_when(
    distance_to_hoop(new_x, new_y) <= 4 & new_y >= 4 ~ "Restricted",
    new_x >= 19 & new_x <= 31 & new_y <= 19 ~ "Paint (non-RA)",
    three_pt == TRUE & new_x <= 4.25 & new_y <= 10 ~ "Left Corner 3",
    three_pt == TRUE & new_x >= 45.75 & new_y <= 10 ~ "Right Corner 3",
    three_pt == TRUE & new_x < 16.5 ~ "Left Wing 3",
    three_pt == TRUE & new_x > 33.5 ~ "Right Wing 3",
    three_pt == TRUE ~ "Top of Key 3",
    TRUE ~ "Midrange"
  ))

one_side <- one_side %>%
  mutate(made_flag = case_when(
    outcome == "made" ~ 1,
    outcome == "missed" ~ 0),
    assist_flag = case_when(
      is.na(assisted) ~ 0,
      TRUE ~ 1
    ))

shotChart %>%
  dplyr::group_by(shooter) %>%
  summarise()

one_side %>% 
  ggplot() + 
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  geom_point(data = one_side, 
             aes(x = new_x, y = new_y, color = shot_zone, shape = outcome)) +
  scale_shape_manual(values = c(1, 4)) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank()) + 
  labs(title = "Duke's Shot Chart", subtitle = "2014-15", color = "Shot Zone", shape = "Outcome")

one_side <- one_side %>%
  dplyr::mutate(value = case_when(
    shot_zone %in% c("Left Corner 3", "Right Corner 3", "Top of Key 3", "Left Wing 3",
                     "Right Wing 3") ~ 3,
    TRUE ~ 2),
                PPS = made_flag * value)

one_side %>%
  filter(shooter != "Nick Pagliuca" & shooter != "Semi Ojeleye") %>%
  ggplot(aes(x = new_x, y = new_y)) +
  stat_summary_hex(aes(z = PPS), bins = 30, colour = "gray", alpha = 0.7, size = 0.2) +
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  scale_fill_gradientn(colours = c("yellow", "orange", "red")) +
  coord_equal() + 
  facet_wrap(.~shooter) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = "left") +
  labs(title = "Duke's 2014-15 Heat Map", subtitle = "By PPS", fill = "PPS")

TJones_heat <- one_side %>%
  filter(shooter == "Tyus Jones") %>%
  ggplot(aes(x = new_x, y = new_y)) +
  stat_summary_hex(aes(z = PPS), bins = 25, colour = "gray", alpha = 0.7, size = 0.2) +
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  scale_fill_gradientn(colours = c("yellow", "orange", "red")) +
  coord_equal() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        legend.position = "left") +
  labs(title = "Tre Jones' 2014-15 Heat Map", subtitle = "By PPS", fill = "PPS")

ggMarginal(TJones_heat, type = "histogram", size = 8, fill = "#001A57")

Duke201415 <- one_side %>%
  dplyr::group_by(shooter, shot_zone) %>%
  dplyr::summarise(shots_attempted = length(outcome),
            shots_made = sum(as.numeric(as.character(made_flag))),
            assisted = sum(as.numeric((as.character(assist_flag)))))

Duke201415 <- Duke201415 %>%
  dplyr::mutate(shot_accuracy = shots_made/shots_attempted,
         assist_pct = assisted/shots_made,
         PPS = case_when(
           shot_zone %in% c("Left Corner 3", "Right Corner 3", "Top of Key 3", "Left Wing 3",
                            "Right Wing 3") ~ shot_accuracy * 3,
           TRUE ~ shot_accuracy * 2),
         x = case_when(
           shot_zone %in% c("Restricted", "Paint (non-RA)", "Midrange", "Top of Key 3") ~ 25,
           shot_zone == "Right Corner 3" ~ 47.75,
           shot_zone == "Left Corner 3" ~ 2,
           shot_zone == "Left Wing 3" ~ 9.5,
           shot_zone == "Right Wing 3" ~ 40.5),
         y = case_when(
           shot_zone %in% c("Restricted", "Left Corner 3", "Right Corner 3") ~ 6.75,
           shot_zone == "Paint (non-RA)" ~ 12.75,
           shot_zone == "Midrange" ~ 20.75,
           shot_zone == "Top of Key 3" ~ 28,
           shot_zone == "Right Wing 3" ~ 22,
           shot_zone == "Left Wing 3" ~ 22
         ))

Duke201415 %>% 
  ggplot() + 
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  geom_point(data = one_side, 
             aes(x = new_x, y = new_y, color = shot_zone, shape = outcome)) +
  scale_shape_manual(values = c(1, 4)) +
  facet_wrap(.~ shooter) +
  theme(panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank()) + 
  labs(title = "Duke's Shot Chart", subtitle = "2014-15", color = "Shot Zone", shape = "Outcome")

Duke201415$shot_accuracy_lab <- paste(as.character(round(100 * Duke201415$shot_accuracy, 1)), "%", sep="")
Duke201415$PPS_lab <- paste(as.character(round(Duke201415$PPS, 2)), " PPS", sep = "")
Duke201415$shot_attempts_lab <- paste(as.character(Duke201415$shots_attempted), " FGA", sep = "")
Duke201415$assist_lab <- paste(as.character(round(100 * Duke201415$assist_pct, 1)), "%", sep = "")

TJones <- Duke201415 %>%
  filter(shooter == "Tyus Jones")

P_half + geom_point(data = TJones, aes(x = x, y = y, color = shot_zone, size = shot_accuracy, alpha = 0.3)) + 
  geom_text(data = TJones, aes(x = x, y = y, color = shot_zone, label = shot_accuracy_lab), vjust = -1.2) + 
  geom_text(data = TJones, aes(x = x + 5, y = y, color = shot_zone, label = PPS_lab),
            size = 3) +
  geom_text(data = TJones, aes(x = x - 5, y = y, color = shot_zone, label = shot_attempts_lab),
            size = 3) +
  guides(alpha = F, size = F) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank())




  