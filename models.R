library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(lubridate)
library(hexbin)
library(lmtest)
library(splines)
library(polycor)
nba <- read.csv("d:/NBA_stats.csv")

head(nba,2)





#fit each regressor individually
mod1 <- lm(salary ~ fgm, data = nba)
mod2 <- lm(salary ~ fga, data = nba)
mod3 <- lm(salary ~ fg_perc, data = nba)
mod4 <- lm(salary ~ three_pm, data = nba)
mod5 <- lm(salary ~ three_pa, data = nba)
mod6 <- lm(salary ~ three_p_perc, data = nba)
mod7 <- lm(salary ~ ftm, data = nba)
mod8 <- lm(salary ~ fta, data = nba)
mod9 <- lm(salary ~ ft_perc, data = nba)
mod10 <- lm(salary ~ two_pm, data = nba)
mod11 <- lm(salary ~ two_pa, data = nba)
mod12 <- lm(salary ~ two_p_perc, data = nba)
mod13 <- lm(salary ~ pps, data = nba)
mod14 <- lm(salary ~ afg_perc, data = nba)

summary(mod1)

# model 1 using all the made to fit a model
modnew1 <- lm(salary ~ fgm+three_pm+ftm+two_pm+pps,data = nba)

nba <- mutate(nba, predicted1= predict(modnew1, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted1))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

summary(modnew1)

# using all the attemp to fit a model

modnew2 <- lm(salary ~ fga+three_pa+fta+two_pa+pps,data = nba)

nba <- mutate(nba, predicted2= predict(modnew2, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted2))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

# mod new 1 without pps

modnew3 <- lm(salary ~ fgm+three_pm+ftm+two_pm,data = nba)

nba <- mutate(nba, predicted3= predict(modnew3, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted3))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

# 3 points made+attemp+pps

modnew4 <- lm(salary ~ three_pm+three_pa+pps, data = nba)

nba <- mutate(nba, predicted4= predict(modnew4, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted4))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

# 2 points made+attemp+pps

modnew5 <- lm(salary ~ two_pm+two_pa+pps, data = nba)

nba <- mutate(nba, predicted5= predict(modnew5, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted5))

# ftm+fta+pps

modnew6 <- lm(salary ~ fgm+fga+pps, data = nba)

nba <- mutate(nba, predicted6= predict(modnew6, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted6))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))


