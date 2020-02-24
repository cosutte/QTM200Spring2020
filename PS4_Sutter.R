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

library(MASS)

#Set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/")

#####################
# Question 1
#####################

# first, run the following commands
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

# We would like to study whether individuals with higher levels of income 
# have more prestigious jobs. 
# Moreover, we would like to study whether professionals have more prestigious jobs 
# than blue and white collar workers.

# A. Create a new variable professional by recoding the variable type so that 
# professionals are coded as 1, 
# and blue and white collar workers are coded as 0 (Hint: ifelse.)

Prestige$type
professional <- as.factor(ifelse(Prestige$type == "prof", 1, 0) )
professional

# B. Run a linear model with prestige as an outcome and income, 
# professional, and the interaction of the two as predictors 
# (Note: this is a continuous × dummy interaction.)

reg1 <- lm(prestige~income + professional + income:professional, data=Prestige)
reg1

# Coefficients:
# (Intercept)                income         professional1  income:professional1  
# 21.142259              0.003171             37.781280             -0.002326  

# C. Write the prediction equation based on the results. 
# 21.14 + 0.0032 * income + 37.78 * professional - 0.0023 * income * professional
# blue / white collar workers, Di = 0
    # y(hat)i = 21.142 + 0.003x + 37.781 x 0 - 0.002X x 0
    # y(hat)i = 21.142 + 0.003X
# professionals, Di = 1
    # (21.142 + 37.781) + (0.003 - 0.002)X

# D. Interpret the coefficient for INCOME
# Conditional on professional and the interaction between income and professional, a one unit increase of the independent variables
# will result in a 0.003 unit increase in INCOME.

# E. Conditional on income and the interation between income and professional, a one unit increase in the independent variables
# will result in a 37.781 unit increase in professional. 

# F. What is the effect of a $1,000 increase in income on prestige score for professional occupations?
# professionals, Di = 1
(21.142 + 37.781) + (0.003 - 0.002)*1000 #59.923

# G. What is the effect of changing one’s occupations from non-professional to professional when her income is $6,000?
# blue / white collar workers, Di = 0
# professional
professional <- 21.14 + 37.78 + (0.0032 - 0.0023) * 6000 #64.32
# blue/white collar
collar <- 21.14 + 0.0032 *6000 #40.34
professional - collar #23.98


#####################
# Question 2
#####################

# A. Use the results to determine whether having these yard signs in a precinct affects vote share 
# (e.g., conduct a hypothesis test with α = .05).

# H0 = Bint,D1 = Bint,D2 = 0
# Ha = at least one slope does not equal 0
tval <- .042/.016
tval #2.625
pval <- 2*pt(2.625, df=129, lower.tail=FALSE)
pval #0.009
# p < .05, so we reject the null hypothesis.

# B. Use the results to determine whether being next to precincts with these yard signs affects 
# vote share (e.g., conduct a hypothesis test with α = .05).

# H0 = Bint,D1 = Bint,D2 = 0
# Ha = at least one slope does not equal 0
tval2 <- .042/.013
tval2 #3.23
pval2 <- 2*pt(3.23, df=129, lower.tail=FALSE)
pval2 #0.001
# p < .05, so we reject the null hypothesis.

# C. Interpret the coefficient for the constant term substantively.
# The intercept for the constant estimates the dependent variable when precint assigned law signs and precint adjacent ro lawn signs are set to 0, 
# meaning, when precints are neither assigned lawn signs nor adjacent to lawn signs, the impact of lawn signs on vote share will be 0.302. 
# Given our data, this will be rather unreasonable.

# Evaluate the model fit for this regression. 
# What does this tell us about the importance of yard signs versus other factors that are not modeled?

# 9.4% of the model's variation can be explained by assigned/adjacent lawn signs. Thus, yard signs are not very important extrememly important in this models.
# There must be other factors not modeled that account for the other 90.6% of variation. 






