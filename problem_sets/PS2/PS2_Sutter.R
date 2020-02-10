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
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("ggplot2"),  pkgTest)
library(ggplot2)

#Set working directory
setwd("~/GitHub/QTM200Spring2020/problem_sets/PS2")

#####################
# Question 1
#####################

# Calculate the X^2 test statistic by hand (chi-square test of independence)
# Recreate table.

classbribe <- matrix(c(14, 6, 7, 7, 7, 1), ncol=3, byrow=TRUE)
rownames(classbribe) <- c("Upper class", "Lower class")
colnames(classbribe) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
classbribe

# A. Find chi-square by hand

# Check by-hand answer for chi-square
chisq.test(classbribe) #3.79

# B. Calculate the p-value. What do you conclude if α = .1?
# find df and calculate p value
(nrow(classbribe)-1)*(ncol(classbribe)-1)
pval <- pchisq(3.8, df=2, lower.tail=FALSE)
pval
# p-value = 0.15. At α = .1, we fail to reject the null hypothesis that the variables are statistially independent. 

# C. Calculate the standardized residuals for each cell. 
(14-13.5)/sqrt(13.5*(1-(27/42))*(1-(21/42))) # 0.32
(6-8.3)/sqrt(8.3*(1-(27/42))*(1-(13/42))) # -1.60
(7-5.1)/sqrt(5.1*(1-(27/42))*(1-(8/42))) # 1.56 
(7-7.5)/sqrt(7.5*(1-(15/42))*(1-(21/42))) # -0.32
(7-4.6)/sqrt(4.6*(1-(15/42))*(1-(13/42))) # 1.67
(1-2.8)/sqrt(2.8*(1-(15/42))*(1-(8/42))) # -1.49

# Put standardized residuals in the table.

standardresid.classbribe <- matrix(c(0.32, -1.60, 1.56 , -0.32, 1.67, -1.49), ncol=3, byrow=TRUE)
rownames(standardresid.classbribe) <- c("Upper class", "Lower class")
colnames(standardresid.classbribe) <- c("Not Stopped", "Bribe requested", "Stopped/given warning")
standardresid.classbribe

# D. How might the standardized residuals help you interpret the results?
# The researchers were interested in whether officers were more or less likely to solicit a bribe from drivers depending on their class, either upper class or lower class. 
# The chi-square test revealed that the two variables were statistically independent. 
# The standardized residuals help interpret this result, as it reveals where the lack of deviation from independence took place. The proportion of "not stopped," for example, deviated the least from the expected value for both upper and lower class officers.

#####################
# Question 2
#####################

# A. 
# H0 = 0 (no effect)
# Ha > 0 (has an effect)

# B. Run a bivariate regression to test this hypothesis.

mean(women$reserved)
mean(women$water)
sum(women$reserved)
sum(women$water)
sum(((women$reserved)-mean(women$reserved))*((women$water)-mean(women$water)))
sum((women$water-mean(women$water))^2)

regress <- lm(data=women, water~reserved)
regress
# (Intercept)     reserved  
# 14.738        9.252 

# slope (beta) = 9.252
# intercept (alpha) = 14.74

# C. Interpret the coefficient estimate for reservation policy.

# Y = α + βX
# Y = 14.738 + 9.252X
# When reservation policy is 0, the number of repaired drinking water fountains is 14.738, and a one unit increase in the reserved variable is associated with a 9.252 unit increase in the number of new or repaired drinking-water facilitie in the village since the reserve policy started.
# We can reject the null hypothesis, since there is an effect between the two variables.


#####################
# Question 3
#####################

# 1. Import the data set and obtain summary statistiscs and examine the distribution of the overall lifespan of the fruitflies.
summary(fruitfly)
hist(fruitfly$lifespan, xlab="lifespan", main="Fruitfly Lifespan")
# the lifespan variable is distrubuted normally with the highest frequency between 40 and 80 days.

# 2. Plot lifespan vs thorax. Does it look like there is a linear relationship?
# What is the correlation coefficient between these two variables?
plot(fruitfly$lifespan, fruitfly$thorax, xlab="Lifespan", ylab="Thorax", main="Lifespan vs. Thorax")
cov(fruitfly$lifespan,fruitfly$thorax)
sd(fruitfly$lifespan)
sd(fruitfly$thorax)
0.8658645/(17.56389*0.07745367) # 0.636

# Check correlation coefficient 
cor(fruitfly$lifespan, fruitfly$thorax, method="pearson")

# 3. Regress lifespan on thorax. Interpret the slope of the fitted model.
regress2 <- lm(fruitfly$lifespan~fruitfly$thorax)
regress2

# Y = α + βX
# Y = -61.05 + 144.33X
# For every 1 unit increase in lifespan, the thorax will inscrease by 144.33 units.

# 4. Test for a significant linear relationship between lifespan and thorax. 
# Provide and interpret your results of your test.
# Find r-squared value
anova(regress2)
15497/(15497+22756) #0.405
# r-squared = 0.405, meaning 40.5% of the variation in lifespan of a fruitfly is explained by the length of the thorax. 

# 5. Provide the 90% confidence interval for the slope of the fitted model.
# Use the formula for typical confidence intervals.
# Use the formula
summary(regress2)
144.33 + 1.657*15.77
144.33 - 1.657*15.77 

# Use the function confint() in R 
confint(regress2, level=.90)
# 90% Confidence Interval: (118.19,170.47)

# 6. Use the predict() function in R to 
# (1) predict an individual fruitfly’s lifespan (Y) when thorax (X) = 0.8 
new_fruitfly <- fruitfly; new_fruitfly$thorax <- .8
predict(lm(lifespan~thorax), newdata=new_fruitfly, se.fit=T)
predict(lm(lifespan~thorax), newdata=new_fruitfly, interval="prediction", level=0.95)
# 54.41478 years. Prediciton interval: (27.37, 81.45)

# (2) predict the average lifespan of fruitflies when thorax = 0.8 by the fitted model. 
predict(lm(lifespan~thorax), newdata=new_fruitfly, interval="confidence", level=0.95)
# 54.41478 years. Confidence interval: (51.91, 56.91)

# This requires that you compute prediction and confidence intervals.
# What are the expected values of lifespan?
# What are the prediction and confidence intervals around the expected values?

# 7. For a sequence of thorax values, draw a plot with their fitted values for lifespan, 
# as well as the prediction intervals and confidence intervals.

ggplot(fruitfly, aes(x=thorax, y=lifespan)) + geom_point() + geom_smooth(method=lm, se=TRUE) # Pl
new_df <- cbind(fruitfly, prediction, row.names = NULL)
ggplot(new_df, aes(thorax, lifespan)) + geom_point() + geom_line(aes(y=lwr), color = "red", linetype = "dashed")+ geom_line(aes(y=upr), color = "red", linetype = "dashed")+ geom_smooth(method=lm, se=TRUE)
