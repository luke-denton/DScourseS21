# PS5 #3

# load in packages
library(tidyverse)
library(rvest)

# load in website
jump <- read_html('https://en.wikipedia.org/wiki/Long_jump')

# create table of top 25 jumps of all time for men
record <- 
  jump %>% 
  html_nodes('#mw-content-text > div.mw-parser-output > table:nth-child(91)') %>% # select table element
  '[['(1)%>% # list function that makes it easier to chain code
  html_table()  

# #4

# load in packages
library(tidyverse)
library(fredr)

# import unemployment data from fred
df <- fredr(series_id = "UNRATE",
            observation_start = as.Date("2000-01-01"),
            observation_end = as.Date("2021-03-01"))

summary(df$value)
