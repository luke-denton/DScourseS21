library(modelsummary)
library(tidyverse)
library(sampleSelection)
library(magrittr)
library(mlogit)

df <- read.csv("wages12.csv")
as.data.frame(df)

#### Q5
df$college <- factor(df$college)
df$union <- factor(df$union)
df$married <- factor(df$married)

#### Q6
datasummary_skim(data = df, output = "latex", histogram = FALSE)

#### Q7
# listwise deletion
df2 <- df %>% drop_na(logwage)
est.mcar <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = df2)

# mean imputation
mean(df2$logwage) # 1.651812
df3 <- df
df3$logwage[is.na(df3$logwage)] <- 1.651812
est.mimp <- lm(logwage ~ hgc + union + college + exper + I(exper^2), data = df3)

# sampleSelection
df4 <- df

# input 0's where the na's are
df4$logwage[is.na(df4$logwage)] <- 0

# create new variable that is valid if logwage > 0 
df4$valid[df4$logwage > 0] <- "true"
df4$valid[df4$logwage == 0] <- "false"

df4$valid <- factor(df4$valid)


heckit <- selection(selection = valid ~ hgc + union + college + exper + married + kids,
          outcome = logwage ~ hgc + union + college + exper + I(exper^2),
          data = df4, method = "2step")

modelsummary(list(est.mcar, est.mimp, heckit), output = "latex")

#### Q8

est.probit <- glm(union ~ hgc + college + exper + I(exper^2), 
                  family=binomial(link='probit'),data=df4)
est.probit %>% summary %>% print
modelsummary(est.probit, output = "latex")

#### Q9

# get predicted probability from already-calculated probit
df4$predProbit <- predict(est.probit, newdata = df4, type = "response")
print(summary(df4$predProbit))

# estimate logit and get predicted probabilities
est.logit <- glm(union ~ hgc + college + exper + I(exper^2), 
                 family=binomial(link='probit'),data=df4)
df4$predLogit <- predict(est.logit, newdata = df4, type = "response")
print(summary(df4$predLogit))

# counterfactual policy

est.probit$coefficients[c("married, kids")] <- 0
df4$predProbitCFP <- predict(est.probit, newdata = df4, type = "response")
print(summary(df4$predProbitCFP))

est.logit$coefficients[c("married, kids")] <- 0
df4$predLogitCFP <- predict(est.logit, newdata = df4, type = "response")
print(summary(df4$predLogitCFP))

