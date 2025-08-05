# load packages

library(shiny)
library(shinyWidgets)
library(ggplot2)
library(highcharter)
library(plotly)
library(tidyr)
library(leaflet)
library(scales)
library(gapminder)
library(tidytext)
library(stopwords)
library(dplyr)
library(wordcloud2)
library(ggiraph)
library(bslib)
library(googlesheets4)
library(lubridate)



# open datasets from google sheets ----
load(file = "data/maillist.RData")

# # load maillist
# maillist <- read_sheet("https://docs.google.com/spreadsheets/d/19bi4UudF_-MU__DELcDrZoqY9njZn30wvKcXXnLEQ08/edit?gid=243593460#gid=243593460", 
#                        sheet = "maillist")
# 
# # load monthly
# monthly <- read_sheet("https://docs.google.com/spreadsheets/d/19bi4UudF_-MU__DELcDrZoqY9njZn30wvKcXXnLEQ08/edit?gid=271632609#gid=271632609", 
#                       sheet = "monthly")
# 
# # load bezug
# bezug <- read_sheet("https://docs.google.com/spreadsheets/d/19bi4UudF_-MU__DELcDrZoqY9njZn30wvKcXXnLEQ08/edit?gid=64839699#gid=64839699", 
#                     sheet = "bezug")
# 
# # load motivation
# motivation <- read_sheet("https://docs.google.com/spreadsheets/d/19bi4UudF_-MU__DELcDrZoqY9njZn30wvKcXXnLEQ08/edit?gid=713246802#gid=713246802", 
#                          sheet = "motivation")
# 



# modules
source('modules/wordcloud2a.R')

