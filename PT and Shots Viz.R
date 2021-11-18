library(tidyverse)
library(extrafont)
library(jsonlite)
library(httr)
library(hablar)
library(janitor)
library(paletteer)
library(prismatic)
library(scales)
library(tidyverse)
library(nbastatR)
library(extrafont)
library(ballr)
library(rvest)
library(ggforce)
library(ggbrace)
library(magick)
library(ggtext)
library(dplyr)
library(gt)
library(future)
library(furrr)
library(ggrepel)
library(ggtext)
library(ggimage)
library(grImport2)
library(ggplot2)
#download the csv with the shot area and play type data
shots <- read_csv("NBAShotsDiff.csv")
pt <- read_csv("NBAPlayTypesDiff.csv")
#merge the shot area and play type data
data <- merge(pt, shots, by="urlThumbnailTeam")
data <- data %>%
  #create the total difference of offensive and defensive shot area
  mutate(ZDiffShots = ZOff.y + ZDef.y) %>%
  #create the total difference between the offensive and defensive play type
  mutate(ZDiffPT = ZOff.x + ZDef.x) %>%
  #get the total difference between both shot area and play type
  mutate(ZTot = ZDiffPT+ZDiffShots) %>%
  #select the columns we want to use for the plot
  select(urlThumbnailTeam, slugTeam, ZDiffPT, ZDiffShots, ZTot)
#create a csv with the data we just manipulated
write.csv(data, "NBATotalDiff.csv", row.names = FALSE)
#create a scatterplot comparing the play type and shot area differences
ggplot(data, aes(ZDiffPT, ZDiffShots, image = urlThumbnailTeam)) +
  xlim(-4, 4) +
  ylim(-4, 4) +
  geom_abline(slope=0, intercept=0,  col = "white", size=1) +
  annotate("text", label = "High Play Type & \nHigh Shot Area Change", x = 3.25, y = 3.25, size = 3.5, colour = "white", face = 'bold') +
  geom_vline(xintercept = 0, size = 1, col = "white") +
  annotate("text", label = "High Play Type & \nLow Shot Area Change", x = 3.25, y = -3.25, size = 3.5, colour = "white", face = 'bold') +
  annotate("text", label = "Low Play Type & \nLow Shot Area Change", x = -3.25, y = -3.25, size = 3.5, colour = "white", face = 'bold') +
  annotate("text", label = "Low Play Type & \nHigh Shot Area Change", x = -3.25, y = 3.25, size = 3.5, colour = "white", face = 'bold') +
  geom_image(size = 0.1) +
  geom_smooth(method=lm) +
  theme_dark() +
  theme(strip.text.x = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        plot.title.position = 'plot',
        plot.title = element_text(face =  'bold', size = 15),
        plot.subtitle = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'floralwhite', colour = "floralwhite"),
        plot.margin = unit(c(.5, .5, 1, .5), "lines")) + 
  labs(x = "Total Play Type Change Z-Score", 
       y = "Total Shot Area Change Z-Score", 
       title = "What's Changed?", 
       subtitle = paste0("Shot Area & Play Type Data per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))