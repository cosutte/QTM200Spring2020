###########################################
## Lab 9: Bivariate Regression in R      ##
###########################################


## Goals:
## 1. Run a bivariate regression in R
## 2. Plot the regression line
## 3. Visualize and assess residuals


## Data on 1932 German Election (at precinct level)
## Paper: G. King, O. Rosen, M. Tanner, A.F. Wagner (2008)
## “Ordinary economic voting behavior in the extraordinary
## election of Adolf Hitler.” 
load("nazis.Rdata")


## Variables
## nvoter: number of eligible voters
## nazivote: number of votes for Nazis
## shareself: proportion of self-employed potential voters
## shareunemployed: proportion of unemployed potential voters
## sharewhite: proportion of white-collar potential voters

## DV for today -- nazi share of the vote in each precinct
nazis$nazishare <- nazis$nazivote / nazis$nvoter 
hist(nazis$nazishare)


## lm() function is for regression aka "linear model"
?lm


## Bivariate regression where outcome variable is share of 
## vote for Nazis and explanatory variable is proportion of
## self-employed voters
## Theory:  those who were hurt by the economy but were at little risk of
## unemployment -- such as self-employed shopkeepers and professionals,
## were the groups that gave the most disproportionate support to the Nazis
mod_self <- lm(nazishare ~ shareself, data = nazis)


## summary() function displays detailed model output
## Do you see beta, standard error, tvalue, and pvalue?
summary(mod_self)


## Note that beta/std error = tvalue
## Why? What were the null and alternative hypotheses?
.455/.08175


## What is our conclusion?
## How do we interpret the coefficient on shareself?


## Confidence interval for our slope estimate
tval <- qt(1-(.05/2), df = 681-2, lower.tail = TRUE) ## high n, so approx 1.96
.455 - (tval*0.08175)
.455 + (tval*0.08175)


## Let's plot our line against the data
plot(x = nazis$shareself, y = nazis$nazishare, pch = "+",
     cex = .75, xlab = "Share Self-Employed", ylab = "Nazi Share of Vote",
     main = "1932 German Election Results,\nby Precinct")


## We can add our fitted line in 2 ways
abline(mod_self, col = "red", lwd = 2) ## use the model itself
abline(a = .3310, b = .4554, col = "blue", lwd = 2)  ## use coefficient results


## Let's check out the residuals for the first 10 observations
## Part of the model object
mod_self$residuals[1:10]


## Remember residuals are the observed value - fitted value
nazis$nazishare[1:10] - mod_self$fitted.values[1:10]


## We should look at the residuals because when we use OLS,
## we assume errors are normally distributed!  Let's check.


## Residuals against fitted values
plot(x = mod_self$fitted.values, y = mod_self$residuals)
abline(h = 0)


## Like we saw above, not as many large, positive residuals,
## but this is pretty good
hist(mod_self$residuals)




### Exercises

## Load the following SAT data.
#install.packages("faraway")
library(faraway)
data(sat)
?sat




## (1) Plot expenditures per pupil as your explanatory variable and
## % of eligible students taking the SAT as our outcome.  Do these data
## look like good candidates for OLS?





## (2) Run a bivariate linear regression to test the hypothesis that
## expenditures are associated with more students taking the SAT






## (3) What are the null and alternative hypotheses?





## (4) Interpret the effect of the coefficient on "expend".






## (5) Plot your fitted line against the data.  Do you think
## the line fits the data well?





## (6) Visually examine the residuals.  Does your evaluation of the use
## of OLS with these data change?  In other words, is there something
## wrong with your residual plot?






