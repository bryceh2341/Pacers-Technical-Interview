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

#function to scrape data for open shots by quarter
get_open_shots <- function(quarter) {
  #url for shots with 6+ feet of space
  url <- paste0("https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=6%2B+Feet+-+Wide+Open&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=", quarter,"&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=&VsConference=&VsDivision=&Weight=")
  #scrape teh data
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  #filter the Pacers, add a column for quarters, select the data we want to use
  df <- df %>%
    filter(TEAM_ABBREVIATION == "IND") %>%
    mutate(Quarter = quarter) %>%
    select(Quarter, FG3A_FREQUENCY)
  #change the type to numeric
  df[c(1:2)] <- sapply(df[c(1:2)], as.numeric)
  #return the dataframe
  return(df)
  
}

#function for getting self created shots by quarter
get_self_created <- function(quarter) {
  #url for shots that are taken with 2 or less seconds or touch time
  url <- paste0("https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=", quarter,"&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+%3C+2+Seconds&VsConference=&VsDivision=&Weight=")
  #scrape the data
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
  #change the type to numeric
  df[c(4:18)] <- sapply(df[c(4:18)], as.numeric)
  #filter for the Pacers, add a quarter column, add a self created shots column (1-not self created), and select the data we want
  df <- df %>%
    filter(TEAM_ABBREVIATION == "IND") %>%
    mutate(Quarter = quarter) %>%
    mutate(self_created = 1-FGA_FREQUENCY) %>%
    select(Quarter, self_created)
  #change the type for the quarter column to numeric
  df[c(1)] <- sapply(df[c(1)], as.numeric)
  
  return(df)
  
}

#function for getting shot location data yb quarter
get_shot_locations <- function(quarter) {
  #url for offensive shot locations
  url <- paste0("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=", quarter,"&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=")
  #scrape the data
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  #change the column names so we can reference the columns in manipulation
  colnames(df) <- c(1:23)
  #change the type to numeric
  df[c(3:23)] <- sapply(df[c(3:23)], as.numeric)
  #filter the team to the Pacers, add a quarter column, select the columns we want to use and reorder them
  df <- df %>%
    filter(df[2] == "Indiana Pacers") %>%
    mutate(Quarter = quarter) %>%
    select(24, 4, 7, 10, 13, 16,  19)
  #change the data from FGA to percentage of shots from each area
  df[2:7] <- round(df[2:7]/rowSums(df[2:7]), 3)
  #change the quarter column to numeric
  df[c(1)] <- sapply(df[c(1)], as.numeric)
  #change to column names to be more descriptive
  colnames(df) <- c("Quarter", "RestrictedArea", "PaintNonRA", "Midrange", "LeftCorner3", "RightCorner3", "AbovetheBreak3")
  #create a column for rim or 3 point attempts (shots we like)
  df <- df %>% mutate(RimOr3 = RestrictedArea+LeftCorner3+RightCorner3+AbovetheBreak3) %>% select(Quarter, RimOr3)
  
  return(df)
  
}

#function for getting opponent shot location data yb quarter
get_opp_shot_locations <- function(quarter) {
  #url for defensive shot locations
  url <- paste0("https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Opponent&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=", quarter,"&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=")
  #scrape the data
  res <- GET(url = url, add_headers(.headers=headers))
  json_resp <- fromJSON(content(res, "text"))
  df <- data.frame(json_resp$resultSets$rowSet)
  #change the column names so we can reference the columns in manipulation
  colnames(df) <- c(1:23)
  #change the type to numeric
  df[c(3:23)] <- sapply(df[c(3:23)], as.numeric)
  #filter the team to the Pacers, add a quarter column, select the columns we want to use and reorder them
  df <- df %>%
    filter(df[2] == "Indiana Pacers") %>%
    mutate(Quarter = quarter) %>%
    select(24, 4, 7, 10, 13, 16,  19)
  #change the data from FGA to percentage of shots from each area
  df[2:7] <- round(df[2:7]/rowSums(df[2:7]), 3)
  #change the quarter column to numeric
  df[c(1)] <- sapply(df[c(1)], as.numeric)
  #change to column names to be more descriptive
  colnames(df) <- c("Quarter", "RestrictedArea", "PaintNonRA", "Midrange", "LeftCorner3", "RightCorner3", "AbovetheBreak3")
  #create a column for opponent rim or 3 point attempts (shots we don't like)
  df <- df %>% mutate(OppRimOr3 = RestrictedArea+LeftCorner3+RightCorner3+AbovetheBreak3) %>% select(Quarter, OppRimOr3)
  
  return(df)
  
}

#create a vector of quarters
quarters <- c("1", "2", "3", "4")
#use the function to create a dataframe of open shots
OpenShots <- future_map_dfr(quarters, get_open_shots)
#use the function to get a dataframe of self created shots
SelfCreated <- future_map_dfr(quarters, get_self_created)
#use the function to get a dataframe of shot locations
ShotLocation <- future_map_dfr(quarters, get_shot_locations)
#use the function to get a dataframe of opponent shot locations
OppShotLocation <- future_map_dfr(quarters, get_opp_shot_locations)

#merge the data from the dataframes we just got by using the function
data <- merge(OpenShots, SelfCreated, by="Quarter")
data <- merge(data, ShotLocation, by="Quarter")
data <- merge(data, OppShotLocation, by="Quarter")

#create a table fo the data
data %>%
  gt()  %>%
  cols_label(Quarter = "Quarter",
             FG3A_FREQUENCY = "Open 3 Freq.",
             self_created = "Self Created Freq.",
             RimOr3 = "Rim or 3 Freq.",
             OppRimOr3 = "Opp Rim or 3 Freq.") %>%
  tab_header(
    title = "Pacers Closing Struggles",
    subtitle = "2021-22 Regular Season | Quarterly Stats"
  )  %>%
  data_color(
    columns = c(FG3A_FREQUENCY, self_created, RimOr3, OppRimOr3),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(Quarter, FG3A_FREQUENCY, self_created, RimOr3, OppRimOr3)
  ) %>%
  cols_width(vars(Quarter, FG3A_FREQUENCY, self_created, RimOr3, OppRimOr3) ~ px(65)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Quarter == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Quarter == "League Average")
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
  gtsave("Pacers Quarter Stats.png")