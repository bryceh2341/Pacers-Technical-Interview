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
#get the teams from this season
teams <- nbastatR::nba_teams()
colnames(teams)[which(names(teams) == "nameTeam")] <- "Team"
teams <- teams %>% 
  select(urlThumbnailTeam, Team) %>%
  filter(urlThumbnailTeam != "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg")
teams[teams == "Los Angeles Clippers"] <- "LA Clippers"
  
#download the data we got from the scraper
data_off <- read_csv("NBAShotsOff.csv")
data_def <- read_csv("NBAShotsDef.csv")
#add a column with the sum of differences between each shot type
data_off$absDiff <- rowSums(abs(data_off[2:7]))
data_def$absDiff <- rowSums(abs(data_def[2:7]))
#change the column names for easier use
colnames(data_off) <- c("Team", "RA", "PaintNonRA", "Mid", "LeftCorner", "RightCorner", "ATB", "absDiff")
colnames(data_def) <- c("Team", "RA", "PaintNonRA", "Mid", "LeftCorner", "RightCorner", "ATB", "absDiff")
#merge the offensive shot area data with the team data to get the logo url for the table
data_off <- merge(data_off, teams, by="Team") 
data_def <- merge(data_def, teams, by="Team")
#move the logo url to the front of the dataframe for the table
data_off <- data_off[c(9, 1:8)]
data_def <- data_def[c(9, 1:8)]
#create the offensive shot area table
data_off %>%
  arrange(desc(absDiff)) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             Team = "Team",
             RA = "RA",
             PaintNonRA = "Paint(non-RA)",
             Mid = "Midrange",
             LeftCorner = "Left Corner",
             RightCorner = "Right Corner",
             ATB = "ATB3",
             absDiff = "Absolute Sum"
             ) %>%
  tab_header(
    title = "What's Changed?",
    subtitle = "2021-22 Regular Season Offensive Shot Areas"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB, absDiff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB, absDiff)
  ) %>%
  cols_width(vars(absDiff) ~ px(45),
             vars(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Team == "League Average")
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
  gtsave("NBA Off Shot Area Diff.png")
#create the defensive shot area table
data_def %>%
  arrange(desc(absDiff)) %>%
  gt()  %>%
  cols_label(urlThumbnailTeam = "",
             Team = "Team",
             RA = "RA",
             PaintNonRA = "Paint(non-RA)",
             Mid = "Midrange",
             LeftCorner = "Left Corner",
             RightCorner = "Right Corner",
             ATB = "ATB3",
             absDiff = "Absolute Sum"
  ) %>%
  tab_header(
    title = "What's Changed?",
    subtitle = "2021-22 Regular Season Defensive Shot Areas"
  )  %>%
  text_transform(
    locations = cells_body(vars(urlThumbnailTeam)),
    fn = function(x) {
      web_image(url = x,
                height = px(22.5))
    }
  ) %>%
  data_color(
    columns = c(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB, absDiff),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB, absDiff)
  ) %>%
  cols_width(vars(absDiff) ~ px(45),
             vars(RA, PaintNonRA, Mid, LeftCorner, RightCorner, ATB) ~ px(30)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Team == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Team == "League Average")
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
  gtsave("NBA Def Shot Area Diff.png")
#create a dataframe for the z-score of offensive shot areas for comparison across statistics
z_off <- data_off %>%
  mutate(ZOff = (absDiff - mean(data_off$absDiff)) / sd(data_off$absDiff)) %>%
  select(urlThumbnailTeam, Team, ZOff)
#create a dataframe for the z-score of defensive shot areas for comparison across statistics
z_def <- data_def %>%
  mutate(ZDef = (absDiff - mean(data_def$absDiff)) / sd(data_def$absDiff)) %>%
  select(urlThumbnailTeam, ZDef)
#merge the offensive and defensive z-score shot area data
data <- merge(z_off, z_def, by="urlThumbnailTeam")
#save the new dataframe as a csv
write.csv(data, "NBAShotsDiff.csv", row.names = FALSE)
#create a scatterplot comparing the offensive and defensive z-score data
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
  labs(x = "Offensive Shot Area Change Z-Score", 
       y = "Defensive Shot Area Change Z-Score", 
       title = "What's Changed?", 
       subtitle = paste0("Shot Area Data per NBA.com\nUpdated ", format(Sys.Date(), "%B %d, %Y")))