# load packages

suppressWarnings(suppressMessages({
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
  library(waiter)
}))


# open datasets from google sheets ----
# maillist <- read_sheet("https://docs.google.com/spreadsheets/d/1KBk0gaN0qXwsLR-sW9TcBsmKMFbUoEQVptF9TlL4g18/",
#                        sheet = "2021 Masterlist",
#                        range = "C5:T28",
#                        col_types = "ccccnnnnnnnnnnnccc"
# ) 
load(file = "data/maillist.RData")


# modules
source('modules/wordcloud2a.R')

# ui modules

