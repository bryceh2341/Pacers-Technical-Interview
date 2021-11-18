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

#url for offensive shot area data 2021-22
url <- "https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
#scrape offensive 2021-22 shot area data
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Shots22Off <- data.frame(json_resp[["resultSets"]][["rowSet"]])
#change the type to numeric to use the data
Shots22Off[4:23] <- sapply(Shots22Off[4:23], as.numeric)
#select field goal attempt rate for each area
Shots22Off <- Shots22Off %>%
  select(2, 4, 7, 10, 13, 16, 19)
Shots22Off[2:7] <- Shots22Off[2:7]/rowSums(Shots22Off[2:7])
#change column names
colnames(Shots22Off) <- c("Team", "RA", "PaintNonRA", "MidRange", "LeftCorner3", "RightCorner3", "ATB3")

#url for defensive 2021-22 shot area data
url_2 <- "https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Opponent&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
#scrape the defensive 2021-22 shot area data
res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Shots22Def <- data.frame(json_resp[["resultSets"]][["rowSet"]])
#change the data type to numeric to use the data
Shots22Def[4:23] <- sapply(Shots22Def[4:23], as.numeric)
#Select the field goal attempt rate for each area
Shots22Def <- Shots22Def %>%
  select(2, 4, 7, 10, 13, 16, 19)# %>%
Shots22Def[2:7] <- Shots22Def[2:7]/rowSums(Shots22Def[2:7])
#change the column names
colnames(Shots22Def) <- c("Team", "RA", "PaintNonRA", "MidRange", "LeftCorner3", "RightCorner3", "ATB3")
#create vectors for shots we want more of and shots we want less of
Positive <- c("RA", "LeftCorner3", "RightCorner3", "ATB3")
Negative <- c("PaintNonRA", "MidRange")
#for loop that creates the z-score for both positive shots and negative shots on offense and defense
for (column in colnames(Shots22Off)) {
  name <- paste(column, "Z", sep = "")
  if (column %in% Positive){
    Shots22Off[name] <- round((Shots22Off[[column]]-mean(Shots22Off[[column]])) / sd(Shots22Off[[column]]), 3)
    Shots22Def[name] <- round(((-1*Shots22Def[[column]])-(-1*mean(Shots22Def[[column]]))) / sd(Shots22Def[[column]]), 3)
  }
  else if (column %in% Negative) {
    Shots22Def[name] <- round((Shots22Def[[column]]-mean(Shots22Def[[column]])) / sd(Shots22Def[[column]]), 3)
    Shots22Off[name] <- round(((-1*Shots22Off[[column]])-(-1*mean(Shots22Off[[column]]))) / sd(Shots22Off[[column]]), 3)
  }
}
#select the data for the Pacers
Shots22Off <- Shots22Off %>% filter(Team == "Indiana Pacers") %>% select(8:13)
Shots22Def <- Shots22Def %>% filter(Team == "Indiana Pacers") %>% select(8:13)
#merge the two dataframes
data <- rbind(Shots22Off, Shots22Def)
#name the offense and defense rows, reorder the columns so the offense/defense row is in the front
data <- data %>% mutate(OffDef = c("Offense", "Defense")) %>% select(7, 1:6)
#create a table of the data
data %>%
  gt()  %>%
  cols_label(OffDef = "",
             RAZ = "Restricted Area",
             PaintNonRAZ = "Paint (Non-RA)",
             MidRangeZ = "Midrange",
             LeftCorner3Z = "Left Corner 3",
             RightCorner3Z = "Right Corner 3",
             ATB3Z = "Above the Break 3") %>%
  tab_header(
    title = "Pacers Shot Areas",
    subtitle = "2021-22 Regular Season | Z-Scores"
  )  %>%
  cols_align(
    align = "right",
    columns = vars(OffDef, RAZ, PaintNonRAZ, MidRangeZ, LeftCorner3Z, RightCorner3Z, ATB3Z)
  ) %>%
  cols_width(vars(OffDef, RAZ, PaintNonRAZ, MidRangeZ, LeftCorner3Z, RightCorner3Z, ATB3Z) ~ px(65)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = OffDef == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = OffDef == "League Average")
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
  gtsave("Pacers Shot Areas.png")