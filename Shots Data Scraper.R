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
  select(2, 4, 7, 10, 13, 16, 19, 22)
#change column names
colnames(Shots22Off) <- c("Team", json_resp[["resultSets"]][["headers"]][["columnNames"]][[1]])
#url for offensive shot area data 2020-21
url_1 <- "https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
#scrape offensive 2020-21 shot area data
res <- GET(url = url_1, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Shots21Off <- data.frame(json_resp[["resultSets"]][["rowSet"]])
#change the type to numeric to use the data
Shots21Off[4:23] <- sapply(Shots21Off[4:23], as.numeric)
#select the field goal attempt rate for each area
Shots21Off <- Shots21Off %>%
  select(2, 4, 7, 10, 13, 16, 19, 22)
#change column names
colnames(Shots21Off) <- c("Team", json_resp[["resultSets"]][["headers"]][["columnNames"]][[1]])
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
  select(2, 4, 7, 10, 13, 16, 19, 22)
#change the column names
colnames(Shots22Def) <- c("Team", json_resp[["resultSets"]][["headers"]][["columnNames"]][[1]])
#url for 2020-21 defensive shot area data
url_3 <- "https://stats.nba.com/stats/leaguedashteamshotlocations?Conference=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Opponent&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision="
#scrape 2020-21 shot area data
res <- GET(url = url_3, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
Shots21Def <- data.frame(json_resp[["resultSets"]][["rowSet"]])
#change data type to numeric to use the data
Shots21Def[4:23] <- sapply(Shots21Def[4:23], as.numeric)
#select the field goal attemps rate for each area
Shots21Def <- Shots21Def %>%
  select(2, 4, 7, 10, 13, 16, 19, 22)
#change the columnn names
colnames(Shots21Def) <- c("Team", json_resp[["resultSets"]][["headers"]][["columnNames"]][[1]])
#merge the offensive shot area data
data_off <- merge(Shots21Off, Shots22Off, by="Team")
#merge the defensive shot area data
data_def <- merge(Shots21Def, Shots22Def, by="Team")
#create a vector of the different shot area
shotTypes <- c(json_resp[["resultSets"]][["headers"]][["columnNames"]][[1]])
#create a variable to help manipulate the data
count = 2
#for loop to calculate the differential in frequency of shots from each area
for (i in shotTypes) {
  #create a name for the new column
  name = paste(i, "diff", sep = "")
  #calculate the differences in frequency, place into the dataframe
  data_off[name] <- data_off[count+7] - data_off[count]
  data_def[name] <- data_def[count+7] - data_def[count]
  #add to variable to repeat the process with all the shot areas
  count = count+1
}
#select only the team and differences in their shot area for offense and defense
data_off <- data_off %>%
  select(1, 16:21)
data_def <- data_def %>%
  select(1, 16:21)
#create a csv for offensive and defensive data
write.csv(data_off, "NBAShotsOff.csv", row.names = FALSE)
write.csv(data_def, "NBAShotsDef.csv", row.names = FALSE)