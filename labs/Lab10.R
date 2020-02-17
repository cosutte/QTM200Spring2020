#####################################################
## File: Lab10.R                                   ##
## Multiple Regression in R                        ##
#####################################################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()
# Jeff wd
setwd('/Users/chloesutter/GitHub/QTM200Spring2020/labs')

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("faraway"), pkgTest)


#######################
# Multiple Regression
#######################


# load dataset from Faraway package
load("/Users/chloesutter/Downloads/fake.RData")
data(sat)
?sat

# Estimate the effect of "expend" on "takers" 
# using a 95% level of significance
sat1 <- lm(takers ~ expend, data=sat)
summary(sat1)

# What if we control for other factors?
sat2 <- lm(takers ~ expend + ratio + salary, data=sat)
summary(sat2)

# How do you interpret the coefficient for salary?
# Once we include salary, the estimate effect of expend on our outcome variable is not distinguishable to the relationship.
# 3.6% 


# What if we include all the variables in the data?
sat3 <- lm(takers ~ ., data=sat)
summary(sat3)
# total is NA because it is verbal + math

# Some studies show that politically irrelevant events, such as 
# sports events and shark attacks, affect voters' retrospective
# evaluation of government performance. For example, Busby et al.
# (2017) find that the outcome of a college football game affects
# presidental job approval among students.

load("Busby_Football.RData")
load("/Users/chloesutter/Downloads/Busby_Football.RData")
colnames(x)

# Experimental setting: Busby et al. (2017) randomly assigned students 
# from Ohio State University (OSU) and University of Oregon (UO) to 
# answer a survey before and after the 2015 College Football Playoff 
# National Championship game. OSU won the game 42-20, and thus, OSU is 
# the "winning school" and UO is the "losing school." They find that among 
# OSU students, presidential approval was higher for those who answered 
# the survey after the game than those who answered the survey before the 
# game. By contast, among UO students, presidential approval was lower for
# those who answered the survey after the game than those who answered the 
# survey before the game. t-tests below confirm their findings.

# papprove: presidential approval
# Post: 0 = pre-game survey vs 1 = post-game survey
# osu: 0 = UO (losing) vs 1 = OSU (winning)

# Comparison of OSU students before and after the game
t.test(papprove ~ Post, data=x[x$osu==1,])

# Comparison of UO students before and after the game
t.test(papprove ~ Post, data=x[x$osu==0,])

# Source: Besby, Ethan C, James N. Druckman, and Alexandria Fredendall,
# 2017, "The Political Relevance of Irrelevant Events," Journal of 
# Politics 79(1).


# 2. Run a linear model with papprove as a dependent variable and 
#    Post, osu, and the interaction of the two as independent variables.

model1 <- lm(papprove ~ post, data=x)
model2 <- lm(papprove ~ osu, data=x)
model3 <- lm(papprove ~ post + osu , data=x)
summary(model1)
summary(model2)
summary(model3)

# 3. Answer the following questions based on the results.

# 3a. What is the predicted presidential approval of OSU students who received
#     the survey BEFORE the game?


# 3b. What is the predicted presidential approval of OSU students who received
#     the survey AFTER the game?


# 3c. What is the predicted presidential approval of UO students who received
#     the survey BEFORE the game?


# 3d. What is the predicted presidential approval of UO students who received
#     the survey AFTER the game?


# 3e. What is the marginal effect of Post on presidential approval
#     when osu=1?




################ Additional Question ################ 

# 4. The dataset includes a lot of variables about student characteristics,
#    like age and gender. Add some of these variables to the model above and
#    re-run a regression. Does the inclusion of these controls attenuate their
#    findings? Why or why not?