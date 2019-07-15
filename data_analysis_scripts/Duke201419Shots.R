# load packages
library(tidyverse)
library(gganimate)

# load data
Duke201419Shots <- read_csv("data/Duke201419ShotCharts.csv")

# transform to half-court
one_side <- Duke201419Shots %>%
  mutate(new_x = case_when(
    y >= 47 ~ 50 - x,
    y < 47 ~ x),
    new_y = case_when(
      y >= 47 ~ 94 - y,
      y < 47 ~ y
    ))

one_side <- one_side %>%
  mutate(team_flag = case_when(
    team_name == "Duke" ~ "Duke",
    TRUE ~ "Opponent"))

# add shot zone
distance_to_hoop <- function(x, y) {
  pointDistance(c(x,y), c(25, 5.25), lonlat = F)
}

one_side <- one_side %>%
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

# plot shot chart 2014-19
one_side %>%
  ggplot() + 
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  coord_equal() +
  geom_point(aes(x = new_x, y = new_y, color = shot_zone, shape = outcome)) +
  scale_shape_manual(values = c(1, 4)) + 
  facet_wrap(. ~ team_flag) + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank()) +
  labs(title = "Duke's Shot Chart vs. Opponent Shot Chart", subtitle = "From 2014-19", color = "Shot Zone", shape = "Outcome")

# plot shot accuracy by zone and season
shotS <- one_side %>%
  group_by(season, shot_zone, team_flag) %>%
  summarise(shots_attempted = length(outcome),
            shots_made = sum(as.numeric(as.character(made_flag))),
            assisted = sum(as.numeric((as.character(assist_flag)))))

shotS <- shotS %>%
  mutate(shot_accuracy = shots_made/shots_attempted,
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

shotS$shot_accuracy_lab <- paste(as.character(round(100 * shotS$shot_accuracy, 1)), "%", sep="")
shotS$season_lab <- str_replace(shotS$season, "20", "")
shotS$PPS_lab <- paste(as.character(round(shotS$PPS, 2)), " PPS", sep = "")
shotS$shot_attempts_lab <- paste(as.character(shotS$shots_attempted), " FGA", sep = "")
shotS$assist_lab <- paste(as.character(round(100 * shotS$assist_pct, 1)), "%", sep = "")

shotS %>%
  ggplot() +
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  geom_point(aes(x = x, y = y, color = shot_zone, size = shot_accuracy, alpha = 0.3), size = 2) + 
  geom_text(data = shotS, aes(x = x, y = y, color = shot_zone, label = shot_accuracy_lab), vjust = -1.2, size = 2) + 
  geom_text(data = shotS, aes(x = x + 5, y = y, color = shot_zone, label = PPS_lab),
            size = 1.5) +
  geom_text(data = shotS, aes(x = x - 5, y = y, color = shot_zone, label = shot_attempts_lab),
            size = 1.5) +
  guides(alpha = F, size = F, color = F) +
  coord_equal() +
  facet_wrap(.~ team_flag) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank()) +
  transition_states(season) +
  labs(title = "Duke's shot accuracy vs. opponent's during {closest_state} season")


# plot assist percentage by shot zone
shotS %>%
  filter(team_flag == "Duke") %>%
  ggplot(aes(x = season_lab, y = assist_pct, color = team_flag, group = team_flag)) + 
  geom_point() +
  geom_line() +
  facet_wrap(.~ shot_zone) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Duke's assist percentage highest on three's", subtitle = "2014-19", x = "Season", y = "Assist Percentage") +
  scale_color_manual(values = "#001A57") +
  guides(color = F)

# plot shot volume by shot zone
shotS %>%
  ggplot(aes(x = season_lab, y = shots_attempted, color = team_flag, group = team_flag)) + 
  geom_point() +
  geom_line() +
  facet_wrap(.~ shot_zone) +
  scale_color_manual(values = c("#001A57", "grey")) +
  labs(title = "Duke's shot volume by location", subtitle = "2014-19", x = "Season", y = "Shots Attempted", color = "Team")

# plot shot frequency by shot zone
shotS %>%
  ungroup() %>%
  mutate(shot_zone_basic = case_when(
    shot_zone == "Midrange" ~ "Midrange",
    shot_zone %in% c("Paint (non-RA)", "Restricted") ~ "Paint",
    shot_zone %in% c("Right Corner 3", "Left Corner 3", "Top of Key 3",
                     "Right Wing 3", "Left Wing 3") ~ "Three Point")) %>%
  group_by(season, team_flag) %>%
  mutate(shot_freq = shots_attempted/sum(shots_attempted)) %>%
  group_by(season, team_flag, shot_zone_basic) %>%
  mutate(new_shot_freq = sum(shot_freq)) %>%
  filter(team_flag == "Duke") %>%
  ggplot(aes(x = season_lab, y = new_shot_freq, color = shot_zone_basic, group = shot_zone_basic)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Duke's shot frequency by shot zone", subtitle = "From 2014-2019", x = "Season", y = "Shot Frequency", color = "Shot Zone")

# plot shot accuracy by shot zone
shotS %>%
  ggplot(aes(x = season_lab, y = shot_accuracy, color = team_flag, group = team_flag)) + 
  geom_point() + 
  geom_line() +
  facet_wrap(.~ shot_zone) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#001A57", "grey")) +
  labs(title = "Duke's shot accuracy by shot zone", subtitle = "From 2014-2019", x = "Season", y = "FG%", color = "Team")

# plot heat map
one_side %>%
  ggplot() + 
  stat_density_2d(data = one_side %>% filter(shot_zone != "Restricted"), 
                  aes(x = new_x, y = new_y, fill = stat(density/max(density))),
                  geom = "raster", contour = FALSE, interpolate = TRUE, n = 200) +
  geom_polygon(data = court %>% filter(side == 1), aes(x = x, y = y, group = group), color = "gray") +
  coord_equal() + 
  facet_wrap(.~ team_flag) +
  scale_fill_viridis_c("Shot Frequency",
                       limits = c(0, 1),
                       breaks = c(0, 1),
                       labels = c("Lower", "Higher"),
                       option = "plasma") + 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank()) + 
  labs(title = "Shot Frequency", subtitle = "Excluding Restricted Area", x = "", y = "")

