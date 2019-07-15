library(tidyverse)
library(readr)
library(jpeg)
library(grid)
library(readr)

NBAPlayerShotsData_1_ <- read_csv("data/NBAPlayerShotsData (1).csv")

NBAPlayerShotsData_1_ %>%
  filter(VTM == "BKN") %>%
  ggplot(aes(x = `Loc X`, y = `Loc Y`, shape = `Event Type`, color = `Shot Zone Basic`)) + geom_point(alpha = 0.5) + 
  scale_shape_manual(values = c(1, 4)) +
  coord_fixed()

BKN <- NBAPlayerShotsData_1_ %>%
  filter(VTM == "BKN")

BKN %>%
  ggplot(aes(x = `Loc X`, y = `Loc Y`, color = `Shot Zone Basic`, shape = `Event Type`)) +
  scale_shape_manual(values = c(1, 4)) + 
  annotation_custom(court, -250, 250, -50, 420)  + 
  geom_point(alpha = 0.5) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank()) + 
  labs(title = "Brooklyn Nets Shot Chart") +
  xlim(c(-250, 250)) + ylim(c(-50, 420)) + coord_fixed()

BKN %>%
  select(`Shot Zone Basic`, `Event Type`, `Loc X`, `Loc Y`) %>%
  group_by(`Shot Zone Basic`) %>%
  count(`Event Type`) %>%
  mutate(FG_percentage = n/sum(n)) 

# exclude backcourt shots
BKN_norm <- BKN[which(!BKN$`Shot Zone Basic`=='Backcourt'), ]
BKN$`Shot Made Flag`
# summarise shot data
library(plyr)
shotS <- ddply(BKN_norm, .(`Shot Zone Basic`), summarize, 
               shots_attempted = length(`Shot Made Flag`),
               shots_made = sum(as.numeric(as.character(`Shot Made Flag`))),
               m_loc_x = mean(`Loc X`),
               m_loc_y = mean(`Loc Y`))

# calculate shot zone accuracy and add zone accuracy labels
shotS$shot_accuracy <- (shotS$shots_made / shotS$shots_attempted)
shotS$shot_accuracy_lab <- paste(as.character(round(100 * shotS$shot_accuracy, 1)), "%", sep="")

shotS$m_loc_y[1] <- shotS$m_loc_y[1] + 30
shotS$m_loc_y[4] <- shotS$m_loc_y[4] + 40

shotS %>%
  ggplot(aes(x = m_loc_x, y = m_loc_y)) +
  annotation_custom(court, -250, 250, -50, 420) +
  geom_point(aes(color = `Shot Zone Basic`, size = shot_accuracy, alpha = 0.3), size = 5) +
  geom_text(aes(color = `Shot Zone Basic`, label = shot_accuracy_lab), vjust = -1.2, size = 5) + 
  guides(alpha = F, size = F) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        axis.title = element_blank()) + labs(title = "Brooklyn Nets Shot Accuracy") +
  xlim(c(-250, 250)) + ylim(c(-50, 420)) + coord_fixed()

