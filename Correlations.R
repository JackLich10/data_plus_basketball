# load packages
library(ggcorrplot)
library(tidyverse)
library(broom)
library(readr)

# load data
PlayerGravity <- read_csv("data/201415PlayerStatsWithGravity.csv")
SeasonPlayerGravity <- read_csv("data/201415SeasonGravity.csv")
Lineups <- read_csv("data/201415LineupsGravity.csv")
GameLog <- read_csv("data/201415GameLog.csv")

b <- GameLog %>%
  filter(opponent != "Syracuse(2)") %>%
  dplyr::select(-c(game_number, opponent, game, ft, fta, opp_pts, result, blk, foul, tot_reb, d_reb, stl, tov, o_reb, ast,
                   grav1, grav2)) 

corr <- round(cor(b, use = "complete.obs"), 2)

plot(b)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(shot_making ~ grav5, data = b)
glance(m)

b %>%
  ggplot() +
  geom_point(aes(x = grav5, y = shot_making))

# Individual game logs
PlayerGravity <- PlayerGravity %>%
  dplyr::mutate(avg_tot_grav = (avg_offball + avg_onball)/2)

PlayerGravity %>%
  filter(minutes > 9) %>%
  ggplot() +
  geom_point(aes(x = avg_offball, y = avg_onball, color = position))

PlayerGravity %>%
  filter(three_fga > 1) %>%
  ggplot() +
  geom_point(aes(x = avg_offball, y = three_pct)) +
  geom_smooth(aes(x = avg_offball, y = three_pct), method = "lm") 
  
# Correlation matrix
b <- PlayerGravity %>%
  dplyr::select(-c(game_number, opponent, game_id, result, start, name, position, blk, foul, tot_reb, d_reb, stl, tov, o_reb, ast))

corr <- round(cor(b, use = "complete.obs"), 2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(fg_pct ~ grav2, data = b)
glance(m)

GameLog %>%
  ggplot() +
  geom_point(aes(x = grav2, y = ePPS))

GameLog %>%
  filter(three_fga > 14, opponent != "Syracuse(2)") %>%
  ggplot() +
  geom_point(aes(x = grav5, y = three_pct)) +
  geom_smooth(aes(x = grav5, y = three_pct), method = "lm", se = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Higher Off-Ball Perimeter Gravity Leads to Better Three Point Shooting",
       subtitle = "Duke 2014-15 Season", x = "Off-Ball Perimeter Gravity (20.75+ feet)",
       y = "3FG%", caption = "Minimum 15 3PA")

m <- lm(pts ~ avg_tot_grav, data = na.omit(PlayerGravity))
glance(m)

m2 <- step(m, direction = "forward")
glance(m2)


plot(b)

PlayerGravity %>%
  filter(minutes > 9) %>%
  ggplot() +
  geom_point(aes(x = avg_tot_grav, y = pts)) +
  facet_wrap(.~ position)

m3 <- lm(three_pct ~ avg_offball, data = PlayerGravity %>% filter(three_fga > 4))
glance(m3)

# Season totals
SeasonPlayerGravity <- SeasonPlayerGravity %>%
  filter(minutes > 60) %>%
  dplyr::mutate(fga_game = fga/games,
                three_game = three_fga/games)

SeasonPlayerGravity %>%
  filter(three_fga > 1) %>%
  ggplot() +
  geom_point(aes(x = mean_offgrav, y = three_pct)) + 
  geom_smooth(aes(x = mean_offgrav, y = three_pct), method = "lm", se = F)

m <- lm(three_pct ~ mean_offgrav, data = SeasonPlayerGravity %>% filter(minutes > 62, three_fga > 1))    
glance(m)

b <- SeasonPlayerGravity %>%
  dplyr::select(-name, -position, -blk, -foul, -tot_reb, -d_reb, -stl, -tov, -ast, -o_reb) %>%
  dplyr::mutate(fga_game = fga/games,
                three_game = three_fga/games)

# Correlation matrix
corr <- round(cor(b, use = "complete.obs"), 2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(three_game ~ mean_offgrav, data = b %>% filter(minutes > 100))
glance(m)

m <- lm(mean_offgrav ~ three_fga, data = b)
glance(m)

plot(b %>%
       dplyr::select(-PPS_two, -PPS_three, -games))

b <- GameLog %>%
  filter(opponent != "Syracuse(2)") %>%
  dplyr::select(-c(game_number, opponent, result,
                   blk, foul, tot_reb, d_reb, stl, tov))

# Correlation matrix
corr <- round(cor(b, use = "complete.obs"), 2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(three_pct ~ mean_gravity, data = b)
glance(m)




b <- Gravity %>%
  filter(game != 20150228) %>%
  dplyr::select(-c(X1,game, time, shot_number, value, probability, p1, p2,
                   p3, p4, p5, JO_flag, JW_flag, QC_flag, TJ_flag, AJ_flag, GA_flag, RS_flag, NP_flag, MJ_flag, SK_flag, MP_flag, SO_flag))

# Correlation matrix
corr <- round(cor(b, use = "complete.obs"), 2)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(pps ~ avg_gravity, data = b)
glance(m)

plot(b)

Gravity %>%
  ggplot() +
  geom_point(aes(x = avg_gravity, y = epps)) + 
  geom_smooth(aes(x = avg_gravity, y = epps), method = "lm", se = F)

Gravity %>%
  ggplot() +
  geom_boxplot(aes(x = as.factor(pps), y = avg_gravity))

Gravity %>%
  ggplot() +
  geom_point(aes(x = avg_gravity, y = shot_making)) + 
  geom_smooth(aes(x = avg_gravity, y = shot_making), method = "lm", se = F)

GameLog %>%
  ggplot() +
  geom_point(aes(x = reorder(opponent, game_number), y = mean_gravity)) + 
  geom_line(aes(x = reorder(opponent, game_number), y = mean_gravity, group = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

GameLog %>%
  ggplot() +
  geom_point(aes(x = mean_gravity, y = eff_fg_pct)) +
  geom_smooth(aes(x = mean_gravity, y = eff_fg_pct), method = "lm", se = F)







# Lineups
Lineups <- Lineups %>%
  dplyr::mutate(shot_making = pps - epps)

b <- Lineups %>%
  ungroup() %>%
  filter(tally > 14) %>%
  dplyr::select(-JO_flag, -JW_flag, -QC_flag, -TJ_flag, -AJ_flag, -GA_flag, -RS_flag, -NP_flag, -MJ_flag, -SK_flag, -MP_flag, -SO_flag,
                -grav1, -grav2)

plot(b)

b <- b %>%
  dplyr::mutate(grav35 = grav3 + grav5)

# Correlation matrix
corr <- round(cor(b, use = "complete.obs"), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"))

m <- lm(shot_making ~ grav3, data = b)
glance(m)


b %>%
  ggplot() +
  geom_point(aes(x = grav3, y = shot_making))

Lineups %>%
  filter(tally > 15) %>%
  dplyr::group_by(MP_flag) %>%
  summarise(epps = mean(epps),
            pps = mean(pps),
            shot_making = mean(shot_making),
            gravity = mean(gravity),
            tally = sum(tally))

Lineups %>%
  filter(tally > 15) %>%
  dplyr::group_by(TJ_flag, QC_flag) %>%
  summarise(epps = mean(epps),
            pps = mean(pps),
            shot_making = mean(shot_making),
            gravity = mean(gravity),
            tally = sum(tally))




  