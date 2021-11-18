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

#url for four factor data on nba.com
url <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
FourFactors <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(FourFactors) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the type to numeric for the data we want to use
FourFactors[c("EFG_PCT", "FTA_RATE", "TM_TOV_PCT", "OREB_PCT", "OPP_EFG_PCT", "OPP_FTA_RATE", "OPP_TOV_PCT", "OPP_OREB_PCT")] <- sapply(FourFactors[c("EFG_PCT", "FTA_RATE", "TM_TOV_PCT", "OREB_PCT", "OPP_EFG_PCT", "OPP_FTA_RATE", "OPP_TOV_PCT", "OPP_OREB_PCT")], as.numeric)
#select the columns we want to use
FourFactors <- FourFactors %>%
  select(TEAM_ID, TEAM_NAME, EFG_PCT, FTA_RATE, TM_TOV_PCT, OREB_PCT, OPP_EFG_PCT, OPP_FTA_RATE, OPP_TOV_PCT, OPP_OREB_PCT)

#create vectors for the factors we want to be higher and lower
positive_factors <- c("EFG_PCT", "FTA_RATE", "OREB_PCT", "OPP_TOV_PCT")
negative_factors <- c("TM_TOV_PCT", "OPP_EFG_PCT", "OPP_FTA_RATE", "OPP_OREB_PCT")

#for loop to create z-scores for the positive and negatives four factors and add them to the dataframe
for (column in colnames(FourFactors)) {
  name <- paste(column, "Z", sep = "")
  if (column %in% positive_factors){
    FourFactors[name] <- round((FourFactors[[column]]-mean(FourFactors[[column]])) / sd(FourFactors[[column]]), 3)
  }
  else if (column %in% negative_factors){
    FourFactors[name] <- round(((-1*FourFactors[[column]])-(-1*mean(FourFactors[[column]]))) / sd(FourFactors[[column]]), 3)
  }
}

#filter for the Pacers and select the columns we want to use for the table
FourFactors <- FourFactors %>% 
  filter(TEAM_NAME == "Indiana Pacers") %>%
  select(11:18)

#create the table
FourFactors %>%
  gt()  %>%
  cols_label(EFG_PCTZ = "eFG%",
             FTA_RATEZ = "FTAr",
             TM_TOV_PCTZ = "TOV%",
             OREB_PCTZ = "OReb%",
             OPP_EFG_PCTZ = "eFG%",
             OPP_FTA_RATEZ = "FTAr",
             OPP_TOV_PCTZ = "TOV%",
             OPP_OREB_PCTZ = "OReb%") %>%
  tab_header(
    title = "Pacers Four Factors",
    subtitle = "2021-22 Regular Season | Z-Scores"
  )  %>%
  
  tab_spanner(
    label = "Offense",
    columns = vars(EFG_PCTZ, FTA_RATEZ, TM_TOV_PCTZ, OREB_PCTZ)
  ) %>%
  tab_spanner(
    label = "Defense",
    columns = vars(OPP_EFG_PCTZ, OPP_FTA_RATEZ, OPP_TOV_PCTZ, OPP_OREB_PCTZ)
  ) %>%
  cols_align(
    align = "right",
    columns = vars(EFG_PCTZ, FTA_RATEZ, TM_TOV_PCTZ, OREB_PCTZ, OPP_EFG_PCTZ, OPP_FTA_RATEZ, OPP_TOV_PCTZ, OPP_OREB_PCTZ)
  ) %>%
  cols_width(vars(EFG_PCTZ, FTA_RATEZ, TM_TOV_PCTZ, OREB_PCTZ, OPP_EFG_PCTZ, OPP_FTA_RATEZ, OPP_TOV_PCTZ, OPP_OREB_PCTZ) ~ px(50)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = EFG_PCTZ == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = EFG_PCTZ == "League Average")
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
  gtsave("Pacers Four Factors.png")
