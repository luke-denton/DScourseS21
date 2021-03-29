library(tidyverse)
library(ggthemes)
library(dplyr)
library(magrittr)

# find fun data to work with
data()

midw <- as_tibble(midwest)

# want to focus on midwestern county demographics
# want to split data into metro and non-metro counties
# so facet wrap by metro and nonmetro
# need to convert to factor
midw %<>% mutate(inmetro = factor(inmetro),
                 inmetro = fct_recode(inmetro, Metro = "1", NonMetro = "0"))


# let's try to visualize race demographics between metro and nonmetro counties
ggplot(data = midw,
       aes(x = poptotal, y =percwhite)) +
       geom_jitter() +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Total Population", y = "% White",
            caption = "% of Population White Among NonMetro and Metro Counties")

# this gives us a decent picture of demographics
# as counties get larger, they get more racially diverse
# lets zoom in to get a better picture
# get rid of counties 120k+ for ease of comparison
# we already know from previous graph that these counties are more diverse

filtered_midw <- midw 
filtered_midw %<>% filter(poptotal<120000)

ggplot(data = filtered_midw,
      aes(x = poptotal, y =percwhite)) +
      geom_jitter() +
      theme_minimal() +
      facet_wrap(vars(inmetro), ncol = 2) +
  labs(x = "Total Population", y = "% White",
       caption = "% of Population White Among NonMetro and Metro Midwest Counties, Pop <120k ")
# here we can see a high concentration of nonmetro, very white counties
# lets try a heat map

# let's compare pct college educated to total pop
ggplot(data = filtered_midw,
       aes(x = poptotal, y = percollege)) +
       geom_bin2d() +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Total Population", y = "% College Educated",
       caption = "% of Population College Educated Among NonMetro and Metro Counties, Pop <120k ")

# clearly more college educated people in higher population and metro areas
# try with jitter
ggplot(data = filtered_midw,
       aes(x = poptotal, y = percollege)) +
       geom_jitter() +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Total Population", y = "% College Educated",
       caption = "% of Population College Educated Among NonMetro and Metro Counties, Pop <120k ")
# clustering is more evident here, I like this one more

# let's try with no filters, jitter
ggplot(data = midw,
       aes(x = poptotal, y = percollege)) +
       geom_jitter() +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Total Population", y = "% College Educated",
       caption = "% of Population College Educated Among NonMetro and Metro Counties")
# this is too skewed to make any solid conclusions

# try filtered with smooth
ggplot(data = filtered_midw,
       aes(x = poptotal, y = percollege)) +
       geom_smooth() +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Total Population", y = "% College Educated",
       caption = "% of Population College Educated Among NonMetro 
       and Metro Midwest Counties, Pop <120k ")

# now let's try to find some other meaningful demographics

ggplot(data = filtered_midw,
       aes(x = percbelowpoverty)) +
       geom_histogram(binwidth = 1, bins = 50) +
       theme_minimal() +
       facet_wrap(vars(inmetro), ncol = 2) +
       labs(x = "Counties", y = "% Below Poverty Line",
       caption = "% Population Below Poverty Line among NonMetro and Metro Midwest Counties, Pop <120k")
