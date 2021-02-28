# download file
system('wget -O dates.json "https://www.vizgr.org/historical-events
       /search.php?format=json&begin_date=00000101&%20end_date=20210219&lang=en"')

# print file to console
system('cat dates.json')

# call libraries
library(tidyverse)
library(jsonlite)

# convert JSON to list
mylist <- fromJSON('dates.json')

# convert list to df
mydf <- bind_rows(mylist$result[-1])

# what type of an object is mydf$date?
class(mydf$date)
# "character"

# List the first n rows of mydf
head(mydf,20)
# first 20 rows
