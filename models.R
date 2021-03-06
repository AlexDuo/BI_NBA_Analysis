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

#use all corrolated KPI
modnew0 <- lm(salary ~ fgm+fga+three_pm+three_pa+ftm+fta+two_pm+two_pa+pps, data = nba)

nba <- mutate(nba, predicted0 = predict(modnew0, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted0))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(nba)), color = "red")

summary(modnew0)

# model 1 using all the made to fit a model
modnew1 <- lm(salary ~ fgm+three_pm+ftm+two_pm+pps,data = nba)

nba <- mutate(nba, predicted1= predict(modnew1, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted1))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(nba)), color="red")

summary(modnew1)

# using all the attemp to fit a model

modnew2 <- lm(salary ~ fga+three_pa+fta+two_pa,data = nba)

nba <- mutate(nba, predicted2= predict(modnew2, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted2))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(nba)), color="red")

summary(modnew2)

# mod new 1 without pps

modnew3 <- lm(salary ~ fgm+three_pm+ftm+two_pm,data = nba)

nba <- mutate(nba, predicted3= predict(modnew3, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted3))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

summary(modnew3)

# 3 points made+attemp+pps

modnew4 <- lm(salary ~ three_pm+three_pa+pps, data = nba)

nba <- mutate(nba, predicted4= predict(modnew4, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted4))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba),color = "red"), y = seq(from = 0, to = 40000000, length = nrow(nba)),color="red")

summary(modnew4)
# 2 points made+attemp+pps

modnew5 <- lm(salary ~ two_pm+two_pa+pps, data = nba)

nba <- mutate(nba, predicted5= predict(modnew5, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted5))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba),color="red"), y = seq(from = 0, to = 40000000, length = nrow(nba)),color="red")
summary(modnew5)
# ftm+fta+pps
  
modnew6 <- lm(salary ~ fgm+fga+pps, data = nba)

nba <- mutate(nba, predicted6= predict(modnew6, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted6))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

summary(modnew6)

# put two new variables into table fgr, three_pr, two_pr, and ftr  the rate = made/attemp

nba <- mutate(nba, fgr= (fgm/fga))
nba <- mutate(nba, three_pr= (three_pm/three_pa))
nba <- mutate(nba, two_pr= (two_pm/two_pa))
nba <- mutate(nba, ftr= (ftm/fta))

#ignore missing data
newdata <- na.omit(nba)

modnew7 <- lm(salary ~ fgr+three_pr+two_pr+ftr, data = newdata)

newdata <- mutate(newdata, predicted7= predict(modnew7, newdata))

ggplot(data = newdata)+
  geom_point(mapping = aes(x = salary, y = predicted7))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(newdata)), y = seq(from = 0, to = 40000000, length = nrow(newdata)))

# modnew3 modify

modnew3modify <- lm(salary ~ fgm+three_pm+ftm+two_pm+pps,data = nba)

nba <- mutate(nba, predicted8 = predict(modnew3modify, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted8))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

summary(modnew3modify)

# abandoned regressors all-in

modnew9 <- lm(salary ~ fg_perc+three_p_perc+ft_perc+two_p_perc+afg_perc,data = nba)

nba <- mutate(nba, predicted9 = predict(modnew9, nba))

ggplot(data = nba)+
  geom_point(mapping = aes(x = salary, y = predicted9))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(nba)), y = seq(from = 0, to = 40000000, length = nrow(nba)))

# modnew3modify modify

modnew3modify2 <- lm(salary ~ fgm+three_pm+ftm+two_pm+pps+fgr+three_pr+two_pr+ftr,data = newdata)

newdata <- mutate(newdata, predicted10= predict(modnew3modify2, newdata))

ggplot(data = newdata)+
  geom_point(mapping = aes(x = salary, y = predicted10))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(newdata), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(newdata)), color = "red")

summary(modnew3modify2)

# test boxplot
ggplot(data = nba)+
geom_boxplot(mapping = aes(x = type,y = salary))


#--------------------------------------------------------------------------------------------------------
modnew3modify3 <- lm(salary ~ fgm+three_pm+ftm+two_pm+fgr+three_pr+two_pr+ftr,data = newdata)

newdata <- mutate(newdata, predicted11= predict(modnew3modify3, newdata))

ggplot(data = newdata)+
  geom_point(mapping = aes(x = salary, y = predicted11))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(newdata), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(newdata)), color = "red")

summary(modnew3modify3)
#--------------------------------------------------------------------------------------------------------
modnew3modify4 <- lm(salary ~ fgm+three_pm+ftm+two_pm+fg_perc+three_p_perc+ft_perc+two_p_perc+pps,data = newdata)

newdata <- mutate(newdata, predicted12= predict(modnew3modify4, newdata))

ggplot(data = newdata)+
  geom_point(mapping = aes(x = salary, y = predicted12))+
  geom_path(x = seq(from = 0, to = 40000000, length = nrow(newdata), color = "red"), y = seq(from = 0, to = 40000000, length = nrow(newdata)), color = "red")

summary(modnew3modify4)

#--------------------------------------------------------------------------------------------------------
# according to the box plot we can see that most of nba players' salary are drop in the zone of 2400000 and 13000000
nba2 <- read.csv("d:/NBA_stats2.csv")



nba2 <- mutate(nba2, fgr= (fgm/fga))
nba2 <- mutate(nba2, three_pr= (three_pm/three_pa))
nba2 <- mutate(nba2, two_pr= (two_pm/two_pa))
nba2 <- mutate(nba2, ftr= (ftm/fta))

newdata2 <- na.omit(nba2)

modnew3modify3 <- lm(salary ~ fgm+three_pm+ftm+two_pm+pps+fgr+three_pr+two_pr+ftr,data = newdata2)

newdata2 <- mutate(newdata2, predicted11 = predict(modnew3modify3, newdata2))

summary(modnew3modify3)

ggplot(data = newdata2)+
  geom_point(mapping = aes(x = salary, y = predicted11))+
  geom_path(x = seq(from = 0, to = 13000000, length = nrow(newdata2), color = "red"), y = seq(from = 0, to = 13000000, length = nrow(newdata2)), color = "red")

# #-----------------------------------------------------------------------------------------------------