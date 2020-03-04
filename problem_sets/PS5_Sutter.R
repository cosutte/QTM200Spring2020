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

lapply(c("faraway"),  pkgTest)


# set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/")


#####################
# Problem 1
#####################

# load data
gamble <- (data=teengamb)
# run regression on gamble with specified predictors
model1 <- lm(gamble ~ sex + status + income + verbal, gamble)
model1
# Coefficients:
# (Intercept)          sex       status       income       verbal  
    # 22.55565    -22.11833      0.05223      4.96198     -2.95949 

# (a) Check the constant variance assumption for the errors by plotting the residuals versus the fitted values.
plot(model1)
  # The variance assumption is primarily upheld, as the values of y at each value of x follow (for the most part) a normal distributions.
  # There appear to be outliers at 24, 36, and 39, however.

# (b) Check the normality assumption with a Q-Q plot of the studentized residuals.
plot(model1)
  # The normality assumption is upheld, as the values of y at each value of x follow a normal distribution. 
  # The same three outliers still exist, however. 

# (c) Check for large leverage points by plotting the h values.
plot(hatvalues(model1))
abline(h=2*5/47)
abline(h=3*5/47)
  # four points demonstrate high leverage ( > 2*5/47 ), and thus, they have potential to influence the model.

# (d) Check for outliers by running an outlierTest. 
# install car package
outlierTest(model1)
#    rstudent unadjusted p-value Bonferroni p
#   24 6.016116         4.1041e-07   1.9289e-05
  # Since the adjusted p value for the model is substantially smaller than 0.05, we would neject the null hypothesis and conclude that the model does display certain extreme residuals. 

# (e) Check for influential points by creating a ”Bubble plot” with the hat-values and studentized residuals.
plot(hatvalues(model1), rstudent(model1), type = "n")
cook <- sqrt(cooks.distance(model1))
points(hatvalues(model1), rstudent(model1), cex=10*cook/max(cook))
abline(h=c(-2,0,2), lty=2)
abline(c=c(2,3)*3/45, lty=2)
identify(hatvalues(model1), rstudent(model1))

# one point has both large leverage and large regression residuals, so it has the highest influence on the model.
