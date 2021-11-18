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
#create dataframe of nba teams teams 
teams <- nbastatR::nba_teams()
teams <- teams %>% 
  select(idTeam, slugTeam, urlThumbnailTeam) %>%
  filter(urlThumbnailTeam != "https://stats.nba.com/media/img/teams/logos/NBA_logo.svg") %>%
  arrange(desc(slugTeam))
#create vector of play types to scrape, excluded misc
playTypes <- c("Transition", "Isolation", "PRBallHandler", "PRRollman", "Postup", "Spotup", "Handoff", "Cut", "OffScreen", "OffRebound")
#seasons to scrape
seasons <- c("2020-21", "2021-22")
#for loop to scrape playtype data for each season
for (i in playTypes)
{
  for (j in seasons){
    url <- paste0("https://stats.nba.com/stats/synergyplaytypes?LeagueID=00&PerMode=PerGame&PlayType=", i,"&PlayerOrTeam=T&SeasonType=Regular+Season&SeasonYear=", j,"&TypeGrouping=offensive")
    res <- GET(url = url, add_headers(.headers=headers))
    json_resp <- fromJSON(content(res, "text"))
    df <- data.frame(json_resp$resultSets$rowSet)
    colnames(df) <- json_resp[["resultSets"]][["headers"]][[1]]
    #create column name
    name <- paste(i, j, sep="")
    #arrange data to add to the teams dataframe
    df <- df %>%
      arrange(desc(TEAM_NAME))
    #add data from playtype to the team dataframe
    teams[[name]] <- df$POSS_PCT
  }
}
#create csv of the dataframe
write.csv(teams, "NBAPlayTypesOff.csv", row.names = FALSE)