# load libaries
library(tidyverse)

# load data
Williamson <- read_csv("data/ZionWilliamsonGameLog.csv")

# create mean PPS by game
Williamson_PPS <- Williamson %>%
  filter(!is.na(PPS)) %>%
  summarise(mean = mean(PPS)) %>%
  pull()

# plot Zion PPS by game
Williamson %>%
  ggplot(aes(x = game_number, y = PPS, color = result)) + 
  geom_point() +
  geom_hline(yintercept = Williamson_PPS, linetype = "dashed") +
  geom_smooth(data = Williamson, mapping = aes(x = game_number, y = PPS), inherit.aes = FALSE, method = "loess", se = F) + 
  geom_text(data = Williamson, aes(x = length(Williamson$game_number) - 2, y = Williamson_PPS + .1, label = paste("Mean: ", as.character(round(Williamson_PPS, 2)), " PPS", sep = "")), size = 3, inherit.aes = F) +
  labs(title = "Zion Williamson PPS by game", subtitle = "2018-19 Season", x = "Game Number",
       color = "Result")
