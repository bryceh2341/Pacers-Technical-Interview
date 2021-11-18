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
library(plyr)

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

#url for transition play type data 2021-22
url <- "https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=Transition&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=2021-22&TypeGrouping=offensive"
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Transition <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Transition) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the type of the data to numeric for use
Transition[7:22] <- sapply(Transition[7:22], as.numeric)
Transition <- Transition %>%
  select(TEAM_ID, TEAM_ABBREVIATION, POSS_PCT, PPP) %>%
  #create the z-score of the data
  mutate(Z = (POSS_PCT - mean(Transition$POSS_PCT)) / sd(Transition$POSS_PCT)) %>%
  #select the Pacers data and the columns we want
  filter(TEAM_ABBREVIATION == "IND") %>%
  select(POSS_PCT, Z, PPP)
#create a table of the data we just got
Transition %>%
  gt()  %>%
  cols_label(POSS_PCT = "% of Poss.",
             PPP = "PPP",
             Z = "Z-Score") %>%
  tab_header(
    title = "Pacers Transition",
    subtitle = "2021-22 Regular Season | Z-Scores"
  )  %>%
  cols_align(
    align = "right",
    columns = vars(POSS_PCT, PPP, Z)
  ) %>%
  cols_width(vars(POSS_PCT, PPP, Z) ~ px(75)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = POSS_PCT == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = POSS_PCT == "League Average")
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
  gtsave("Pacers Transition.png")
