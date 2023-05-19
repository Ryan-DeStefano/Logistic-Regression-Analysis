library(VGAM)
library(lmtest)
library(car)
library(ggeffects)
library(patchwork)

# Section 1: Data Exploration

nba = nba.feature.importance

#1
prop.table(table(nba$Game.Type, nba$Swarm.Correct.ATS), margin = 1)
gametype = table(nba$Game.Type, nba$Swarm.Correct.ATS)

chisq.test(gametype)

#2
nba$New.Date = substring(nba$Date, 6, 7)
nba$New.Date = as.integer(nba$New.Date)
nba$New.Date = month.name[nba$New.Date]

prop.table(table(nba$New.Date, nba$Swarm.Correct.ATS), margin = 1)

#3
StrongestPick = aggregate(ats_brainpower ~ Session.Num, data = nba, max) 

for (i in 1:nrow(StrongestPick)) {
  for (j in 1:nrow(nba)) {
    if (nba[j,5] == StrongestPick[i,1] && nba[j,24] == StrongestPick[i,2]) {
      StrongestPick[i,3] = nba[j,10]
    }
  }
}

names(StrongestPick)[3] = "Swarm.Correct.ATS"

AverageAcc = sum(StrongestPick$Swarm.Correct.ATS) / nrow(StrongestPick)
AverageAcc

table(StrongestPick$Swarm.Correct.ATS)
51/93

#4
FavoritePicks = subset(nba, Favorite == 1)

WeakestPick = aggregate(ats_brainpower ~ Session.Num, data = FavoritePicks, min)

for (i in 1:nrow(WeakestPick)) {
  for (j in 1:nrow(nba)) {
    if (nba[j,5] == WeakestPick[i,1] && nba[j,24] == WeakestPick[i,2]) {
      WeakestPick[i,3] = nba[j,10]
    }
  }
}

names(WeakestPick)[3] = "Swarm.Correct.ATS"

AverageAcc = sum(WeakestPick$Swarm.Correct.ATS) / nrow(WeakestPick)
AverageAcc

table(WeakestPick$Swarm.Correct.ATS)
37/90

#Section 2: Model Building

# 1

# Momentum, brainpower, impulse, interpolation are the only significant predictors by themselves
# Individually all swarm variables besides the time ones 
# are significant predictors of correctly picking the game

cor(nba[23:26]) #These variables are also very correlated however, so only including 1 is best. 
#Including more than 1 doesn't decrease the residual deviance much either so the simpler the model is the better

Original = glm(Swarm.Correct.ATS ~ ats_momentum, data = nba, family=binomial(link=logit))
summary(Original)

test1 = glm(Swarm.Correct.ATS ~ ats_momentum+New.Date, data = nba, family=binomial(link=logit))
summary(test1)
lrtest(Original, test1)

test2 = glm(Swarm.Correct.ATS ~ ats_momentum*New.Date, data = nba, family=binomial(link=logit))
summary(test2)
lrtest(Original, test2)

# Date does not appear to be useful to the model, reduces deviance but increases AIC which is not good

test3 = glm(Swarm.Correct.ATS ~ ats_momentum+Game.Type, data = nba, family=binomial(link=logit))
summary(test3)
lrtest(Original, test3)

# Game type does not help either

test4 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num, data = nba, family=binomial(link=logit))
summary(test4)
lrtest(Original, test4)

# Session number is now significant when paired with momentum, model is significantly better than the only momentum model

test5 = glm(Swarm.Correct.ATS ~ ats_momentum*Session.Num, data = nba, family=binomial(link=logit))
summary(test5)
lrtest(test4, test5)

# Interaction between the two does not help however

test6 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+Odds.on.Swarm.Pick, data = nba, family=binomial(link=logit))
summary(test6)
lrtest(test4, test6)

# Odds on the swarm pick is not a significant predictor

test7 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+Favorite, data = nba, family=binomial(link=logit))
summary(test7)
lrtest(test4, test7)

# Favorite is not a significant predictor

test8 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+point_spread, data = nba, family=binomial(link=logit))
summary(test8)
lrtest(test4, test8)

# Point spread is not significant

test9 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+New.Date, data = nba, family=binomial(link=logit))
summary(test9)
lrtest(test4, test9)

# Month of the pick is now significant

test10 = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+New.Date+win_pct_swarm_pick, data = nba, family=binomial(link=logit))
summary(test10)
lrtest(test10, test9)

# Winning percentage of the swarm pick is not significant

FinalModel = glm(Swarm.Correct.ATS ~ ats_momentum+Session.Num+New.Date, data = nba, family=binomial(link=logit))

summary(FinalModel)

# 2
nbaSmall = nba[,c(5,10,23,43)]

# Session 267

game1 = exp(1)^(1.671*.8379686+.029*267-2.187*1)/(1+exp(1)^(1.671*.8379686+.029*267-2.187*1))
game1

game2 = exp(1)^(1.671*.6946830+.029*267-2.187*1)/(1+exp(1)^(1.671*.6946830+.029*267-2.187*1))
game2

game3 = exp(1)^(1.671*.7092244+.029*267-2.187*1)/(1+exp(1)^(1.671*.7092244+.029*267-2.187*1))
game3

game4 = exp(1)^(1.671*.9339253+.029*267-2.187*1)/(1+exp(1)^(1.671*.9339253+.029*267-2.187*1))
game4

game5 = exp(1)^(1.671*.6149897+.029*267-2.187*1)/(1+exp(1)^(1.671*.6149897+.029*267-2.187*1))
game5

game6 = exp(1)^(1.671*.6705191+.029*267-2.187*1)/(1+exp(1)^(1.671*.6705191+.029*267-2.187*1))
game6

game7 = exp(1)^(1.671*.7796614+.029*267-2.187*1)/(1+exp(1)^(1.671*.7796614+.029*267-2.187*1))
game7

game8 = exp(1)^(1.671*.8163405+.029*267-2.187*1)/(1+exp(1)^(1.671*.8163405+.029*267-2.187*1))
game8

game9 = exp(1)^(1.671*.7850618+.029*267-2.187*1)/(1+exp(1)^(1.671*.7850618+.029*267-2.187*1))
game9

game10 = exp(1)^(1.671*.6387083+.029*267-2.187*1)/(1+exp(1)^(1.671*.6387083+.029*267-2.187*1))
game10

game11 = exp(1)^(1.671*.8725818+.029*267-2.187*1)/(1+exp(1)^(1.671*.8725818+.029*267-2.187*1))
game11

game12 = exp(1)^(1.671*.7975724+.029*267-2.187*1)/(1+exp(1)^(1.671*.7975724+.029*267-2.187*1))
game12

ProbabilityCorrectLastSession = data.frame(c(game1,game2,game3,game4,game5,game6,game7,game8,game9,game10,game11,game12))
names(ProbabilityCorrectLastSession)[1] = "ProbOfCorrectPick"

MomentumCutoff = quantile(nba$ats_momentum, .95)
DateCutoff = "May"   # May is the only month with a positive coefficient

Elite = data.frame(matrix(ncol = 3, nrow = 0))
for (i in 1:nrow(nba)){
  if(nba[i,23]>MomentumCutoff && nba[i,43] == DateCutoff){
    Elite[i,1] = nba[i,23]
    Elite[i,2] = nba[i,5]
    Elite[i,3] = "May"
  }
}

Elite = na.omit(Elite)

# Rows 139, 183, and 187 are included in this top crop of games. These are the 3 I would be most confident in betting on.

# Section 3: Visual Representation

plot(ggpredict(FinalModel,"ats_momentum"))

plot(ggpredict(FinalModel,"Session.Num"))

plot(ggpredict(FinalModel,"New.Date"))

plot(ggpredict(FinalModel,c("ats_momentum","Session.Num","New.Date")))
