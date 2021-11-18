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
#get the teams in the NBA
teams <- nbastatR::nba_teams()
teams <- teams %>%
  select(idTeam, slugTeam, urlThumbnailTeam) %>%
  filter(urlThumbnailTeam != "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg") %>%
  arrange(desc(slugTeam))
#url for general stats of every player during the 2021-22 season
url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
#scrape the data from the url
res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Players22 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Players22) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the minutes column to type numeric to use the data
Players22[c("MIN")] <- sapply(Players22[c("MIN")], as.numeric)
#select the columns we need
Players22 <- Players22 %>%
  select(PLAYER_ID, TEAM_ABBREVIATION, MIN)
#url for general stats of every player during the 2020-21 season
url_1 <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision=&Weight="
#scrape the data from the url
res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Players21 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Players21) <- json_resp[["resultSets"]][["headers"]][[1]]
#change minutes to type numeric for use
Players21[c("MIN")] <- sapply(Players21[c("MIN")], as.numeric)
#select the columns we want to use
Players21 <- Players21 %>%
  select(PLAYER_ID, TEAM_ABBREVIATION)
#url for general team stats
url_2 <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res <- GET(url = url_2, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Teams <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Teams) <- json_resp[["resultSets"]][["headers"]][[1]]
#change minutes to type numeric for use
Teams[c("MIN")] <- sapply(Teams[c("MIN")], as.numeric)
#select the columns we want to use
Teams <- Teams %>%
  select(TEAM_ID, MIN)
#get the team abbreviations added to the team data for use between the team data and player data
Teams <- merge(Teams, teams, by.x = "TEAM_ID", by.y = "idTeam")
Teams <- Teams %>% select(slugTeam, MIN)
#merge the general players dataframe from each season
Minutes <- merge(Players21, Players22, by="PLAYER_ID")
#filter players that remained on the same team from 2020-21 to 2021-22
Minutes <- subset(Minutes, Minutes$TEAM_ABBREVIATION.x == Minutes$TEAM_ABBREVIATION.y)
#sum up the miutes by team
Minutes <- ddply(Minutes,"TEAM_ABBREVIATION.x",numcolwise(sum))
#merge the player and team data for minutes
Minutes <- merge(Minutes, Teams, by.x = "TEAM_ABBREVIATION.x", by.y = "slugTeam")
#calculate the percentage of returning minutes for each team
Minutes <- Minutes %>% mutate(MIN = MIN.x/(MIN.y*5)) %>% select(TEAM_ABBREVIATION.x, MIN)

#url for passing data from 2021-22
url_3 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#scrape the data from the url
res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Passes22 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Passes22) <- json_resp[["resultSets"]][["headers"]][[1]]
#get the data we want into numeric type for use
Passes22[c("PASSES_MADE", "POTENTIAL_AST")] <- sapply(Passes22[c("PASSES_MADE", "POTENTIAL_AST")], as.numeric)
#select the data we want to use
Passes22 <- Passes22 %>%
  select(TEAM_ABBREVIATION, POTENTIAL_AST, PASSES_MADE)
#url for passing data from the 2020-21 season
url_4 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=Passing&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#scrape the data from the url
res <- GET(url = url_4, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Passes21 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Passes21) <- json_resp[["resultSets"]][["headers"]][[1]]
#get the data we want to numeric type for use and select that data
Passes21[c("PASSES_MADE", "POTENTIAL_AST")] <- sapply(Passes21[c("PASSES_MADE", "POTENTIAL_AST")], as.numeric)
Passes21 <- Passes21 %>%
  select(TEAM_ABBREVIATION, POTENTIAL_AST, PASSES_MADE)
#merge the 2020-21 and 2021-22 passing data
Passes <- merge(Passes21, Passes22, by="TEAM_ABBREVIATION")
Passes <- Passes %>%
  #get the differences between potential assists and passes made from 2020-21 to 2021-22
  mutate(PotAstDiff = POTENTIAL_AST.y-POTENTIAL_AST.x) %>%
  mutate(PassMadeDiff = PASSES_MADE.y-PASSES_MADE.x) %>%
  select(TEAM_ABBREVIATION, PotAstDiff, PassMadeDiff)

#get the teams in the NBA
coaches <- nbastatR::nba_teams()
coaches <- coaches %>%
  filter(urlThumbnailTeam != "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg") %>%
  select(slugTeam) %>%
  arrange(desc(slugTeam))
#add a column for new coaches, 1 if the coach is in his first full season and 0 if he isn't
coaches$newCoach = ifelse(coaches$slugTeam %in% c("WAS", "POR", "ORL", "BOS", "MIN", "ATL", "NOP", "BOS", "DAL", "IND"),1,0)

#url for 2021-22 touch time shooting data
url_5 <- "https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+%3C+2+Seconds&VsConference=&VsDivision=&Weight="
#scrape the data from the url
res <- GET(url = url_5, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Creation22 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Creation22) <- json_resp[["resultSets"]][["headers"]][[1]]
#get the data to numeric type for use and select the data
Creation22[c("FGA_FREQUENCY")] <- sapply(Creation22[c("FGA_FREQUENCY")], as.numeric)
Creation22 <- Creation22 %>%
  select(TEAM_ABBREVIATION, FGA_FREQUENCY)
#url for the 2020-21 touch time shooting data
url_6 <- "https://stats.nba.com/stats/leaguedashteamptshot?CloseDefDistRange=&College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&DribbleRange=&GameScope=&GameSegment=&GeneralRange=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&ShotDistRange=&StarterBench=&TeamID=0&TouchTimeRange=Touch+%3C+2+Seconds&VsConference=&VsDivision=&Weight="
#scrape the url
res <- GET(url = url_6, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Creation21 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Creation21) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the type to numeric and select the data we want to use
Creation21[c("FGA_FREQUENCY")] <- sapply(Creation21[c("FGA_FREQUENCY")], as.numeric)
Creation21 <- Creation21 %>%
  select(TEAM_ABBREVIATION, FGA_FREQUENCY)
#merge the shooting touch time data from 2020-21 and 2021-22
Creation <- merge(Creation21, Creation22, by="TEAM_ABBREVIATION")
Creation <- Creation %>%
  #data scraped was for 2 or less seconds of touch time, adjust to 2 or more seconds of touch time and get differences
  mutate(SelfDiff = (1-FGA_FREQUENCY.y)-(1-FGA_FREQUENCY.x)) %>%
  select(TEAM_ABBREVIATION, SelfDiff)

#url for 2021-22 speed and distance data
url_7 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2021-22&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#scrape the url
res <- GET(url = url_7, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Speed22 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Speed22) <- json_resp[["resultSets"]][["headers"]][[1]]
#change speed to numeric type and select the data
Speed22[c("AVG_SPEED")] <- sapply(Speed22[c("AVG_SPEED")], as.numeric)
Speed22 <- Speed22 %>%
  select(TEAM_ABBREVIATION, AVG_SPEED)
#url for 2021-22 speed and distance data
url_8 <- "https://stats.nba.com/stats/leaguedashptstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerExperience=&PlayerOrTeam=Team&PlayerPosition=&PtMeasureType=SpeedDistance&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
#scrape the url
res <- GET(url = url_8, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Speed21 <- data.frame(json_resp[["resultSets"]][["rowSet"]])
colnames(Speed21) <- json_resp[["resultSets"]][["headers"]][[1]]
#change the speed to type numeric and select the data
Speed21[c("AVG_SPEED")] <- sapply(Speed21[c("AVG_SPEED")], as.numeric)
Speed21 <- Speed21 %>%
  select(TEAM_ABBREVIATION, AVG_SPEED)
#merge the speed data from 2020-21 and 2021-22
Speed <- merge(Speed21, Speed22, by="TEAM_ABBREVIATION")
#calculate the difference between average speed from each season
Speed <- Speed %>% mutate(SpeedDiff = AVG_SPEED.y-AVG_SPEED.x) %>% select(TEAM_ABBREVIATION, SpeedDiff)
#merge the data into one dataframe
data <- merge(Passes, Minutes, by.x = "TEAM_ABBREVIATION", by.y = "TEAM_ABBREVIATION.x")
data <- merge(data, coaches, by.x = "TEAM_ABBREVIATION", by.y = "slugTeam")
data <- merge(data, Creation, by="TEAM_ABBREVIATION")
data <- merge(data, Speed, by="TEAM_ABBREVIATION")
#save the dataframe as a csv
write.csv(data, "NBAOtherData.csv", row.names = FALSE)