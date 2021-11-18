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
#open the csv created from the scraper
NBAPlayTypesOff <- read_csv("NBAPlayTypesOff.csv")
NBAPlayTypesDef <- read_csv("NBAPlayTypesDef.csv")
#vector of the play types used
playTypes <- c("Transition", "Isolation", "PRBallHandler", "PRRollman", "Postup", "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound")
#create new dataframes to calculate differences in each play type
data_off <- NBAPlayTypesOff %>% select(urlThumbnailTeam, slugTeam)
data_def <- NBAPlayTypesDef %>% select(urlThumbnailTeam, slugTeam)
#create a variable to help manipulate the data
count = 5
#for loop to get the differences in each play type and insert them into the new dataframes we just created
for (i in playTypes) {
  #create a name for the new column
  name = paste(i, "diff", sep = "")
  #calculate the difference between play type from different season and add them to the new dataframe
  data_off[name] <- NBAPlayTypesOff[count]-NBAPlayTypesOff[count-1]
  data_def[name] <- NBAPlayTypesDef[count]-NBAPlayTypesDef[count-1]
  #update the variable for continuation of for loop
  count = count+2
}
#create a new row with the sum of absolute value in each column
data_off$absDiff <- rowSums(abs(data_off[3:12]))
data_def$absDiff <- rowSums(abs(data_def[3:12]))
#create the table showing the differences in offensive play type data
data_off %>%
  arrange(desc(absDiff)) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             slugTeam = "Team",
             Transitiondiff = "Transition",
             Isolationdiff = "Isolation",
             PRBallHandlerdiff = "PnR Ball Handler",
             PRRollmandiff = "PnR Roll Man",
             Postupdiff = "Post Up",
             Spotupdiff = "Spot Up",
             Handoffdiff = "Handoff",
             Cutdiff = "Cut",
             OffScreendiff = "Off Screen",
             OffRebounddiff = "Off Rebound",
             absDiff = "Absolute Sum") %>%
  tab_header(
    title = "What's Changed?",
    subtitle = "2021-22 Regular Season Offensive Play Types"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  fmt_percent(
    columns = vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff),
    decimals = 2
  )  %>%
  data_color(
    columns = c(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff)
  ) %>%
  cols_width(vars(absDiff) ~ px(45),
             vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = slugTeam == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = slugTeam == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 24,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
  ) %>%
  gtsave("NBA Off Play Type Diff.png")
#create a table showing the differences between defensive play type data
data_def %>%
  arrange(desc(absDiff)) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             slugTeam = "Team",
             Transitiondiff = "Transition",
             Isolationdiff = "Isolation",
             PRBallHandlerdiff = "PnR Ball Handler",
             PRRollmandiff = "PnR Roll Man",
             Postupdiff = "Post Up",
             Spotupdiff = "Spot Up",
             Handoffdiff = "Handoff",
             Cutdiff = "Cut",
             OffScreendiff = "Off Screen",
             OffRebounddiff = "Off Rebound",
             absDiff = "Absolute Sum") %>%
  tab_header(
    title = "What's Changed?",
    subtitle = "2021-22 Regular Season Defensive Play Types"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  fmt_percent(
    columns = vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff),
    decimals = 2
  )  %>%
  data_color(
    columns = c(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff, absDiff)
  ) %>%
  cols_width(vars(absDiff) ~ px(45),
             vars(Transitiondiff, Isolationdiff, PRBallHandlerdiff, PRRollmandiff, Postupdiff, Spotupdiff, Handoffdiff, Cutdiff, OffScreendiff, OffRebounddiff) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = slugTeam == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = slugTeam == "League Average")
  ) %>%
  tab_options(
    table.background.color = "floralwhite",
    column_labels.font.size = 10.5,
    table.font.size = 10,
    heading.title.font.size  = 24,
    heading.title.font.weight = 'bold',
    heading.subtitle.font.size = 11,
    table.font.names = "Consolas",
    table.font.color = 'black',
    table.border.top.color = "transparent",
    data_row.padding = px(2),
    footnotes.font.size = 8,
    source_notes.font.size = 9,
    footnotes.padding = px(1),
  ) %>%
  gtsave("NBA Def Play Type Diff.png")
#create a new dataframe with the z-score of offensive play types for easier comparison across different statistics
z_off <- data_off %>%
  mutate(ZOff = (absDiff - mean(data_off$absDiff)) / sd(data_off$absDiff)) %>%
  select(urlThumbnailTeam, slugTeam, ZOff)
#create a new dataframe with the z-score of defensive play types for easier comparison across different statistics
z_def <- data_def %>%
  mutate(ZDef = (absDiff - mean(data_def$absDiff)) / sd(data_def$absDiff)) %>%
  select(urlThumbnailTeam, ZDef)
#merge the z-score data from the offensive and defensive dataframes
data <- merge(z_off, z_def, by="urlThumbnailTeam")
#write a csv for the data we just created
write.csv(data, "NBAPlayTypesDiff.csv", row.names = FALSE)
#create a scatterplot of our data
ggplot(data, aes(ZOff, ZDef, image = urlThumbnailTeam)) +
  xlim(-3, 3) +
  ylim(-3, 3) +
  geom_abline(slope=0, intercept=0,  col = "white", size=1) +
  annotate("text", label = "High Off. & Def.\nChange", x = 2.5, y = 2.5, size = 5, colour = "white", face = 'bold') +
  geom_vline(xintercept = 0, size = 1, col = "white") +
  annotate("text", label = "High Off. & Low Def.\nChange", x = 2.5, y = -2.5, size = 5, colour = "white", face = 'bold') +
  annotate("text", label = "Low Off. & Low Def.\nChange", x = -2.5, y = -2.5, size = 5, colour = "white", face = 'bold') +
  annotate("text", label = "Low Off. & High Def.\nChange", x = -2.5, y = 2.5, size = 5, colour = "white", face = 'bold') +
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
  labs(x = "Offensive Play Type Change Z-Score", 
       y = "Defensive Play Type Change Z-Score", 
       title = "What's Changed?", 
       subtitle = paste0("Play Type Data per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))