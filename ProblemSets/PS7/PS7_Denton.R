library(modelsummary)
library(mice)
library(tidyverse) #always
library(magrittr)
library(dplyr)

df <- read.csv("wages.csv")
df %<>% mutate(tenuresq = tenure^2)
df %<>% mutate(college = as.factor(college))
df %<>% mutate(married = as.factor(married))

# drop off na's of hgc or tenure

df2 <- df %>% drop_na(hgc, tenure)
datasummary_skim(df2, output="latex", histogram = FALSE)


# listwise deletion of logwage
df3 <- df %>% drop_na(logwage)

est.mcar <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,
               data = df3)

# mean imputation of logwage

# find mean of logwage with all na's excluded
mean(df3$logwage)
# 1.622102

# now plug in the mean for each na
df4 <- df2
df4$logwage[is.na(df4$logwage)] <- 1.622102


est.meanimp <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,
               data = df4)

# use predicted values from model above for "missing at random" assumption
df5 <- df2
df5$logwage[is.na(df5$logwage)] <- predict(est.meanimp, newdata = df5[is.na(df5$logwage),])
est.mar <- lm(logwage ~ hgc + college + tenure + tenuresq + age + married,
              data = df5)

# multiple imputation
dat_mice <- mice(df2, m = 10, printFlag = FALSE,  seed = 12345)

fit <- with(dat_mice, lm(logwage ~ hgc + college + tenure + tenuresq + age + married))
mices <- mice::pool(fit)

# model summary of all models
modelsummary(list(est.mcar, est.meanimp, est.mar, mices),output="latex")
