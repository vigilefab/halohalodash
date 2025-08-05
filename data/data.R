
#----- PRELIMINARIES -----#

# deactivate scientific notation
options(scipen=999)


# set wd
setwd("C:/Users/user/Dropbox/Halo-Halo/Halo-Halo IG & Website/Sonstiges/Pinboard Map/git/halohalodash")


# setup libraries
library(lubridate)
library(dplyr)
library(magrittr)
library(data.table)
library(googlesheets4)
library(tidyr)
library(stopwords)
library(ggwordcloud)
library(tidytext)


# clear workspace
rm(list = ls())


# inspiration: https://www.appsilon.com/post/r-shiny-highcharts




#-------------------------------------------------
#----- DATA FOR DASHBOARD -----#
#-------------------------------------------------

# import data
load(file = "../../maillist.RData")







#-------------------------------------------------
#----- Create country groups -----#
#-------------------------------------------------

# create countries (with all < 2 members as Sonstige)
maillist <- maillist %>% 
  filter(!Land == "") %>%
  group_by(Land) %>%
  mutate(members = n()) %>%
  mutate(Land2 = replace(Land, members < 10, "Sonstiges"),
         datetime = mdy_hms(Timestamp) )
  








#-------------------------------------------------
#----- Map Variables -----#
#-------------------------------------------------
# for entries without a timestamp, we give "2021-11-01 20:30:00"
maillist <- maillist %>% mutate(Timestamp = mdy_hms(Timestamp)) %>%
  mutate(Timestamp = replace(Timestamp, is.na(Timestamp), mdy_hms("11012021 20:30:00")))


# create membership tenure
now <- Sys.time()
maillist <- maillist %>%
  mutate(tenure = as.numeric(as.Date(now) - as.Date(Timestamp)))


# add coords and zoom for each country (Worldwide, DE, AU, CH and Sonstiges)
Land2 <-  c("Deutschland", "Österreich", "Schweiz", "Sonstiges")
lon   <-  c(10.4541,       15.3541,      8.2275,    0)
lat   <-  c(51.1642,       47.8286,      46.8182,   0)
zoom  <-  c(5,             7,            7,         2)

center_geom <- tibble(Land2 = Land2,
                      center_lon = lon,
                      center_lat = lat,
                      center_zoom = zoom)

maillist <- left_join(x = maillist, y = center_geom, by = "Land2")


# labels for each point (it should say "Member since Month dd, 20xx")
maillist$label <- paste("Member since", format(as.Date(maillist$Timestamp), "%B %d, %Y"))










#-------------------------------------------------
#----- Chart Variables -----#
#-------------------------------------------------

# fix region so that region = Land if Land2 == sonstiges
maillist <- maillist %>% mutate(region = replace(region, Land2 == "Sonstiges", Land))

monthly <- maillist %>%
    mutate(month = format(Timestamp, "%Y-%m")) %>%
    filter(!is.na(month)) %>%
    group_by(Land2, month) %>%
    summarise(membr = n())

allmonths <- expand(monthly, Land2, month) %>% distinct
monthly <- left_join(x = allmonths, y = monthly,
                                     by = c("Land2", "month")) %>%
                             mutate(membr = replace(membr, is.na(membr), 0))








#-------------------------------------------------
#----- Word Cloud Dataset -----#
#-------------------------------------------------

# relevant variables
worddat <- maillist %>% 
  select(Land, Land2, region, contains("Welchen"), contains("Wieso"))
colnames(worddat) <- c("Land", "Land2", "region", "bezug", "motivation")

worddat <- filter(worddat, bezug != "" | motivation != "")
stopwrd <- tibble(word = c(stopwords("de"), "philippinen", "philippinisch", "philippinische", "philippinischen", 
                           "philippinischer", "filippinisch", "filipino", "filipinos", "dass",
                           "filipina", "deutsch", "deutschen", "deutscher", "deutschland", 
                           "möchte", "gerne", "gern", "gibt", "manchmal","all", "oft",
                           "mehr", "ch", "de", "at", "city", "san", "schweiz", "wien", "österreich",
                           "is", "jedoch", "ays", "beim", "because", "bisher", "to", "a", "i", 
                           "davon", "dabei"))

# bezug
bezug <- worddat %>% group_by(Land, Land2, region) %>%
  # tokenization
  unnest_tokens(word, bezug) %>% 
  # remove stopwords
  anti_join(stopwrd) %>% 
  # count words
  count(word, sort = TRUE) %>%
  # remove numbers
  filter(!grepl(x = word, pattern = "[0-9]")) %>%
  # rename n
  mutate(wordcount = n)


# motivation
motivation <- worddat %>% group_by(Land, Land2, region) %>%
  # tokenization
  unnest_tokens(word, motivation) %>% 
  # remove stopwords
  anti_join(stopwrd) %>% 
  # count words
  count(word, sort = TRUE) %>%
  # remove numbers
  filter(!grepl(x = word, pattern = "[0-9]")) %>%
  # rename n
  mutate(wordcount = n)

  








#-------------------------------------------------
#----- Save Datasets -----#
#-------------------------------------------------

# save to google sheets
sheet <- as_sheets_id("https://docs.google.com/spreadsheets/d/1KBk0gaN0qXwsLR-sW9TcBsmKMFbUoEQVptF9TlL4g18/edit?gid=1449400037#gid=1449400037")
sheet_delete(sheet, list("maillist", "monthly", "motivation", "bezug"))
sheet_write(maillist, ss = sheet)
sheet_write(monthly, ss = sheet)
sheet_write(motivation, ss = sheet)
sheet_write(bezug, ss = sheet)


# save in local drive
save(maillist, monthly, motivation, bezug, sheet, file = "data/maillist.RData")

# ── <googlesheets4_spreadsheet> ─────────────────────────────────────────────────
# Spreadsheet name: halohalodash                                
# ID: 1sPtPuX3i1exYl_-t6q37lCwqhP1fk9jd7DIyJpQoaFU
# Locale: en_US                                       
# Time zone: Etc/GMT                                     
# # of sheets: 1                                           
# 
# ── <sheets> ────────────────────────────────────────────────────────────────────
# (Sheet name): (Nominal extent in rows x columns)
# data: 304 x 19




