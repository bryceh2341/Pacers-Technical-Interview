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

#url for general advanced stats from nba.com
url <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
GenAdv <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(GenAdv) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the data to type numeric for use
GenAdv[3:47] <- sapply(GenAdv[3:47], as.numeric)
#select the columns we want to use
GenAdv <- GenAdv %>%
  select(TEAM_ID, TEAM_NAME, OFF_RATING, DEF_RATING, NET_RATING)
#url for general advanced stats in the clutch
url_1 <- "https://stats.nba.com/stats/leaguedashteamclutch?AheadBehind=Ahead+or+Behind&ClutchTime=Last+5+Minutes&Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&PointDiff=5&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
ClutchAdv <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(ClutchAdv) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the data to type numeric for use
ClutchAdv[3:47] <- sapply(ClutchAdv[3:47], as.numeric)
#select the columns we want to use
ClutchAdv <- ClutchAdv %>%
  select(TEAM_ID, OFF_RATING, DEF_RATING, NET_RATING)

#merge the two datasets we just created
Adv <- merge(GenAdv, ClutchAdv, by="TEAM_ID")
#get the difference between the total and clutch stats
Adv[3:5] <- Adv[6:8]-Adv[3:5]
#select the columns we want to use
Adv <- Adv %>% select(1:5)
#change the names of the columns for use
colnames(Adv) <- c('TEAM_ID', "TEAM_NAME", "OFF_RTG", "DEF_RTG", "NET_RTG")
Adv <- Adv %>%
  #get the z-scores for offensive rating, defensive rating, and net rating
  mutate(ZOff = round((OFF_RTG - mean(Adv$OFF_RTG)) / sd(Adv$OFF_RTG), 2)) %>%
  mutate(ZDef = round(((-1*DEF_RTG) - -1*(mean(Adv$DEF_RTG))) / sd(Adv$DEF_RTG), 2)) %>%
  mutate(ZNet = round((NET_RTG - mean(Adv$NET_RTG)) / sd(Adv$NET_RTG), 2)) %>%
  #filter for the Pacers
  filter(TEAM_NAME == "Indiana Pacers") %>%
  #select the columns we want to use int he table
  select(3:8)

#create the table
Adv %>%
  gt()  %>%
  cols_label(OFF_RTG = "Off. Rtg.",
             DEF_RTG = "Def. Rtg.",
             NET_RTG = "Net Rtg.",
             ZOff = "Off. Rtg.",
             ZDef = "Def. Rtg.",
             ZNet = "Net Rtg.") %>%
  tab_header(
    title = "Pacers Clutch",
    subtitle = "2021-22 Regular Season | Data & Z-Scores"
  )  %>%
  tab_spanner(
    label = "Difference",
    columns = vars(OFF_RTG, DEF_RTG, NET_RTG)
  ) %>%
  tab_spanner(
    label = "Z-Scores",
    columns = vars(ZOff, ZDef, ZNet)
  ) %>%
  cols_align(
    align = "right",
    columns = vars(OFF_RTG, DEF_RTG, NET_RTG, ZOff, ZDef, ZNet)
  ) %>%
  cols_width(vars(OFF_RTG, DEF_RTG, NET_RTG, ZOff, ZDef, ZNet) ~ px(65)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = OFF_RTG == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = OFF_RTG == "League Average")
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
  gtsave("Pacers Clutch.png")

  
