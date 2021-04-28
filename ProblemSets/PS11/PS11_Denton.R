library(tidyverse)
library(magrittr)
library(tidymodels)
library(modelsummary)

# find data here: https://archive.ics.uci.edu/ml/datasets/Student+Performance
# yes I know I can automate it into my code, I will polish that up!

math <- read.csv("student-mat.csv",header=TRUE)
port <- read.csv("student-por.csv",header=TRUE)

# create df for students overlapping in both classes
mport=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students

# estimate regression of Math final grade on other vars
est <- lm(G3 ~ Medu + Fedu + studytime, data = math )
summary(est)

# need to mutate some factor vars of interest
math$sex <- factor(math$sex)
math$address <- factor(math$address)
math$famsize <- factor(math$famsize)
math$Pstatus <- factor(math$Pstatus)
math$activities <- factor(math$activities)

# estimate additional regression
est2 <- lm(G3 ~ Medu + Fedu + studytime + address + famsize + Pstatus + activities + Walc, data = math)
summary(est2)


modelsummary(list(est, est2), output = "latex")

# next steps
# need to do a ML model where I try to find the best predictors of test scores