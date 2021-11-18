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
#open the data with the different variables
data <- read.csv("NBAOtherData.csv")
#open the data with the differences in shot area and play type and get the total differences
diff <- read.csv("NBATotalDiff.csv")
diff <- diff %>%
  select(slugTeam, ZTot)
#merge the two datasets
data <- merge(data, diff, by.x = "TEAM_ABBREVIATION", by.y = "slugTeam", all = FALSE)
#create a vector of the row names that we want to run a regression on
rows <- colnames(data)[2:7]
#create an empty vector for insertion of our r-squared values later
rsquared <- c()
#for loop to run the regression and get r-squared values for each variable
for (i in rows){
  model <- lm(get(i)~ZTot, data=data)
  rsquared <- c(rsquared, summary(model)$r.squared)
}
#add a row for total
rows <- c(rows, "Total")
#run the regression and get the r-squared for all the variables combined
model <- lm(ZTot~., data=data[,-1])
rsquared <- c(rsquared, summary(model)$r.squared)
#create a dataframe with the r-squared values
total_data <- data.frame(Predictor = c("Potential Assists", "Passes", "Returning Minutes", "New Coach", "Self Creation", "Average Speed", "Total"), RSquared = round(rsquared, 3))
#create a table of the data
total_data %>%
  gt()  %>%
  cols_label(RSquared = "R-Squared") %>%
  tab_header(
    title = "What Causes Change?",
    subtitle = "R-Squared for Predicting 2021-22 Changes"
  )  %>%
  data_color(
    columns = c(RSquared),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::green_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_align(
    align = "right",
    columns = vars(RSquared)
  ) %>%
  cols_width(vars(RSquared) ~ px(125),
             vars(Predictor) ~ px(125)) %>%
  tab_style(
    style = list(
      cell_borders(
        side =  "top",
        color = 'gray55',
        weight = px(2)
      )
    ),
    locations = cells_body(
      rows = Predictor == "League Average"
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "floralwhite"),
    locations = cells_body(
      rows = Predictor == "League Average")
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
  gtsave("NBA R-Squared Cause.png")
