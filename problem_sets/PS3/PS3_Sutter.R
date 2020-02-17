#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2"),  pkgTest)
library(ggplot2)

lapply(c("Rcpp"), pkgTest)
library(Rcpp)


#Set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS3")


#####################
# Question 1
# We are interested in knowing how the difference in campaign spending 
# between incumbent and challenger affects the incumbent’s vote share.

#####################

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is difflog.
# import data, already subset to include relevant variables
#look at data
summary(incumbents_subset)
table1 <- data.frame(incumbents_subset$voteshare, incumbents_subset$difflog)
table1
# estimate regression
regress1 <- lm(incumbents_subset$voteshare~ incumbents_subset$difflog)
regress1

# Coefficients:
  # (Intercept)      difflog  
  # 0.57903      0.04167 

# 2. Make a scatterplot of the two variables and add the regression line.
plot(incumbents_subset$voteshare~incumbents_subset$difflog, xlab="difflog", ylab = 
      "vote share", main="The Effect of difflog on voteshare")
# display estimated regression line
abline(regress1)
# show how far each prediction is from the estimated regression line
# preds <- predict(regress1) 
# segments(incumbents_subset$difflog, incumbents_subset$voteshare, preds)

# 3. Save the residuals of the model in a separate object.
residuals(regress1)
regress1resid <- residuals(regress1)

# 4. Write the prediction equation.
  # Y = alpha + Beta(x)
  # Y = .57 + .04X
  # Where Y = vector of observed outcomes for voteshare 
  # and X = vector of difflog

#####################
# Question 2
# We are interested in knowing how the difference between 
# incumbent and challenger’s spending and the vote share of the 
# presidential candidate of the incumbent’s party are related.
#####################

# 1. Run a regression where the outcome variable is presvote and the explanatory variable is difflog.
# estimate regression
regress2 <- lm(incumbents_subset$presvote~incumbents_subset$difflog)
regress2
#Coefficients:
  #(Intercept)  incumbents_subset$difflog  
  #0.50758                    0.02384  

# 2. Make a scatterplot of the two variables and add the regression line.
plot(incumbents_subset$presvote,incumbents_subset$difflog, xlab="difflog", ylab="presvote", main="Difference Between presvote and difflog ")
# display estimated regression line
abline(regress2)
# show how far each prediction is from the estimated regression line
# preds2 <- predict(regress2) 
# segments(incumbents_subset$difflog, incumbents_subset$presvote, preds)

# 3. Save the residuals of the model in a separate object.
residuals(regress2)
regress2resid <- residuals(regress2)

# 4. Write the prediction equation.
  # Y = alpha + Beta(x)
  # Y = .507 + .023X
  # Where Y = vector of observed outcomes for presvote 
  # and X = vector of difflog

#####################
# Question 3
# We are interested in knowing how the vote share of the 
# presidential candidate of the incumbent’s party is associated 
# with the incumbent’s electoral success.
#####################

# 1. Run a regression where the outcome variable is voteshare and the explanatory variable is presvote.
regress3 <- lm(incumbents_subset$voteshare~incumbents_subset$presvote)
regress3
# Coefficients:
  # (Intercept)  incumbents_subset$presvote  
  # 0.4413                      0.3880  

# 2. Make a scatterplot of the two variables and add the regression line.
plot(incumbents_subset$voteshare~incumbents_subset$presvote,  xlab="presvote", ylab="voteshare", main="Difference Between presvote and voteshare")
# display estimated regression line
abline(regress3)
# show how far each prediction is from the estimated regression line
# preds3 <- predict(regress3) 
# segments(incumbents_subset$presvote, incumbents_subset$voteshare, preds)

# 3. Save the residuals of the model in a separate object.
residuals(regress3)
regress3resid <- residuals(regress3)

# 4. Write the prediction equation.
  # Y = alpha + Beta(x)
  # Y = .44 + .38X
  # Where Y = vector of observed outcomes for voteshare 
  # and X = vector of presvote


#####################
# Question 4

# The residuals from part (a) tell us 
# how much of the variation in VOTESHARE is NOT explained 
# by the difference in spending between incumbent and challenger.

# The residuals in part (b) tell us 
# how much of the variation in PRESVOTE is NOT explained 
# by the difference in spending between incumbent and challenger 
# in the district.
#####################

# 1. Run a regression where the 
# outcome variable is the residuals from Question 1 (Y = regress1resid)
# explanatory variable is the residuals from Question 2 (X = regress2resid)

lm(regress1resid~regress2resid)
regress4 <- lm(regress1resid~regress2resid)
# find residuals 
regress4resid <- residuals(regress4)

# Coefficients:
  # (Intercept)            regress2resid  
  # -0.00000000000000000486   0.25687701270009788423  

# 2. Make a scatterplot of the two residuals and add the regression line.
plot(regress1resid~regress2resid, main="Unexplained Variation in voteshare by presvote")
# display estimated regression line
abline(regress4)

# 3. Write the prediction equation.
  # Y = alpha + Beta(x)
  # Y = -0.0000000000000000048 + 0.25X
  # Where Y = residuals describing how much of the variation in VOTESHARE is NOT explained by the difference in spending between incumbent and challenger
  # and X = residuals describing how much of the variation in PRESVOTE is NOT explained by the difference in spending between incumbent and challenger in the district.

#####################
# Question 5
# What if the incumbent’s vote share is affected by 
# BOTH the president’s popularity
# AND the difference in spending between incumbent and challenger?
#####################

# 1. Run a regression where the outcome variable is the incumbent’s voteshare
# and the explanatory variables are difflog AND presvote.
  # Y: voteshare
  # X1: difflog
  # X2: presvote

lm_by_hand <- function(inputDF, covariates, outcome){
  #load required packages
  require(MASS)
  #create matrices
  n=nrow(Y)
  X <- matrix(c(rep(1,n), incumbents_subset$difflog, incumbents_subset$presvote), ncol=3)
  Y <- matrix(incumbents_subset$voteshare, ncol=3)
  dim(X)
  #calculate betas
  betas <- solve((t(X)%*%X))%*%(t(X)%*%Y)
  rownames(betas)[3] <- "intercept"
  k <- ncol(X)
  #calculate SEs for betas
  #estimate sigma-squared
  sigma_squared <- sum((Y-X%*%betas)^2)/(nrow(X)-ncol(X))
  #create variance-covariance matrix for betas
  var_covar_mat <- sigma_squared*solve(t(X)%*%X)
  #standard errors for coefficient estimates
  SEs <- sqrt(diag(var_covar_mat))
  #get t-stat and p-vals
  TS <- (betas-0)/SEs
  p_values <- 2*pt(abs(TS), n-k, lower.tail=F)
  #regression
  reg <- lm_by_hand(incumbents_subset, c("difflog", "presvote"), "voteshare")
}

regression5 <- lm(voteshare ~ difflog + presvote, data=incumbents_subset)
regression5
#Coefficients:
  #(Intercept)      difflog     presvote  
  #0.44864      0.03554      0.25688  
summary(regression5)
regress5resid <- residuals(regression5)
summary(regress5resid)

# 2. Write the prediction equation. 
  # μy = β0 + β1x1 + β2x2
  # μy = 0.449 + 0.035X1 + 0.256X2
  # Where X1 represents difflog and X2 represents presvote

# 3. What is it in this output that is identical to the output in Question 4? Why do you think this is the case?
# Question 4 residuals: 
regress4resid <- residuals(regress4)
summary(regress4resid)
# Question 5 residuals:
regress5resid <- residuals(regression5)
summary(regress5resid)
# Why?
# Question 4 shows how much unexplained variation on presvote is caused by the unexplained variation in voteshare.
# Question 5 shows the explained variation between difflog and presvote on voteshare.
# The residuals are the same for both, because their observed and predicted values should have the same difference. 
