library(tidyverse)
library(mlogit)
library(lmtest)
library(survival)
library(mclogit)
library(MNP)
library(margins)
library(nnet)
library(readr)
library(stargazer)
library(foreign)
library(nnet)
library(stargazer)

df<-read.csv("case2_cheese_brand.csv")
view(df)

#transform the choice variable as a factor
df$cheese <- as.factor(df$cheese)

#set the value "0" as a reference level
df$cheese<- relevel(df$cheese, ref = "0")
levels(df$cheese) 

#Check current level
levels(df$cheese)[0]


#building model with ordinary chees
mod.cheese.0 <- multinom(cheese ~ age + vine + male + aware, data = df)
summary(mod.cheese.0)

library(stargazer)
stargazer(mod.cheese.0, type="text", out="mod.cheese.0.text")


#With a unit increase in age of individual is more  likely to choose the 2-d type of cheese relative to the base 0 type cheese. 
#With a unit increase in vine of individual is less  likely to choose the 2-d type of cheese relative to the base 0 type cheese. 
#For males  is less  likely to choose the 2-d type of cheese relative to the base 0 type cheese. 
#With a unit increase in aware of individual is less  likely to choose the 2-d type of cheese relative to the base 0 type cheese. 
#For males   is less  likely to choose the 1-d type of cheese relative to the base 0 type cheese. 
#With a unit increase in aware of individual is less  likely to choose the 1-d type of cheese relative to the base 0 type cheese. 

(exp(coef(mod.cheese.0))-1)*100


#Check the predicted probability for each program (first 30 entries)
head(mod.cheese.0$fitted.values,30)

#calculate marginal effect for each observation
marginal_effects(mod.cheese.0)

#calculate marginal average effects
sapply(marginal_effects(mod.cheese.0), mean)


apply(marginal_effects(mod.cheese.0), mean)


mod.cheese.0.rrr = exp(coef(mod.cheese.0)) 
mod.cheese.0.rrr 

stargazer(mod.cheese.0, type="html", coef=list(mod.cheese.0.rrr), p.auto=FALSE, out="mod.cheese.0rrr.htm")
stargazer(mod.cheese.0, type="text", coef=list(mod.cheese.0.rrr), p.auto=FALSE, out="mod.cheese.0rrr.text")

#with a additional year   in age the odds that an individual chooses the 2 type of cheese  relative to the base alternative go up by (1.030-1)*100%=30%
#with a unit increase in buying vine the odds that an individual chooses the 2 type of cheese  relative to the base alternative go down by (0.531-1)*100%=46,9%
#Foe males a male the odds that an individual chooses the 2 type of cheese  relative to the base alternative go down by (0.482-1)*100%=51,8%
#if person is aware the odds that an individual chooses the 2 type of cheese  relative to the base alternative go down by (0.616-1)*100%=38,4%

#if person is aware the odds that an individual chooses the 1 type of cheese  relative to the base alternative go down by (0.616-1)*100%=65,6%
#For males the odds that an individual chooses the 1 type of cheese  relative to the base alternative go down by (0.482-1)*100%=25%

#predictions

table(predict(mod.cheese.0))

allmean <- data.frame(age=rep(mean(df$age),2), 
                     vine=rep(mean(df$vine),2),  
                     male = c(1,0),
                     aware =rep(mean(df$aware),2)) 
print(allmean)

allmean[, c("pred.prob")] <- predict(mod.cheese.0, newdata=allmean, type="probs")
print(allmean)

 
allmean <- data.frame(age=rep(mean(df$age),2), 
                      vine=rep(mean(df$vine),2),  
                      male = c(female,male),
                      aware =rep(mean(df$aware),2)) 

