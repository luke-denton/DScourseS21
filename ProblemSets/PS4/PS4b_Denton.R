# install packages
install.packages(tidyverse)
install.packages(sparklyr)

# load packages
library(tidyverse)
library(sparklyr)
library(dplyr)

# set up a connection to Spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# create a tibble that loads in the iris data 
df1 <- as_tibble(iris)

# copy df1 to spark
df <- copy_to(sc,df1)

# verify data frames are different types
class(df1)
#"tbl_df" "tbl" "data.frame"
class(df)
#"tbl_spark" "tbl_sql" "tbl_lazy" "tbl"

# now using RDD commands

# list first 6 rows of sepal length and species
df %>% select(Sepal_Length,Species) %>% head %>% print

# filter to firs 6 sepal lengths greater than 5
df %>% filter(Sepal_Length>5.5) %>% head %>% print

# combine both operations into one line
df %>% select(Sepal_Length,Species) %>% filter(Sepal_Length>5.5) %>% head %>% print

# group_by and calculate mean of each species
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

# sort the grouped observations by ascending species name
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print
df2 %>% arrange(Species) %>% head %>% print
