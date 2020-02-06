############################################################
## Lab 5: t-distribution and confidence intervals         ##
############################################################


## Goals:
## 1. Review the probability distribution functions in R
## 2. Introduce the t-distribution functions
## 3. Review confidence intervals and calculate them in R

## Review the help files
?TDist
?Normal


## Recall: t-Distribution is similar to normal distribution, but with fatter tails
## In R, the t-distribution functions are normalized to mean = 0 and sd = 1
## You have to specify degrees of freedom

## Visualize the two different distributions by taking 1 million random draws
## Note: this is different than what you need to do in the homework!
random_normal <- rnorm(1000000)
random_t <- rt(1000000, df = 5)
plot(density(random_normal), col = "blue")
lines(density(random_t), col = "red")
legend("topright", legend = c("Normal", "T"), col = c("blue", "red"), lty = 1)


#### "Distribution" function (cumulative probability)
## Same output because distributions have the same center
pnorm(0, mean = 0, sd = 1)
pt(0, df = 10)

## but t-distribution has more volume in the tails
pnorm(-2, mean = 0, sd = 1)
pt(-2, df = 10)


#### "Quantile" functions, or inverse cumulative probability distribution
## Where is cumulative probability = .025?
## Farther out in the tails of t-distribution 
qnorm(.025, mean = 0, sd = 1)
qt(.025, df = 10)


## "Density" functions
## Very similar density, but t-distribution is smaller in the middle...
dnorm(0, mean = 0, sd = 1)
dt(0, df = 10)

## ... and larger in the tails...
dnorm(-3, mean = 0, sd = 1)
dt(-3, df = 10)

## ... and even larger if we have smaller number of df
dt(-3, df = 5)



#### Using R to calculate CIs

## Load the data: a subset of 2004 American National Election Study
load("anes.Rdata")
View(anes)

## Let's say our confidence coefficient = .95
## Calculate the appropriate confidence interval for the
## mean level of support for how George W. Bush was
## handling the war in Iraq (bushIraq)
z95 <- qnorm((1 - .95)/2, lower.tail = FALSE)## (1-confidence coefficient)/2
n <- length(na.omit(anes$bushiraq))
sample_mean <- mean(anes$bushiraq, na.rm = TRUE)
sample_sd <- sd(anes$bushiraq, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95) ## What does this mean?


## Now let's use a confidence coefficient = .99
z99 <- qnorm((1 - .99)/2, lower.tail = FALSE)
lower_99 <- sample_mean - (z99 * (sample_sd/sqrt(n)))
upper_99 <- sample_mean + (z99 * (sample_sd/sqrt(n)))
confint99 <- c(lower_99, upper_99)


## Do our results make sense?
confint95
confint99


### Exercises

## 1. Load data using the following code
## Read the help file
install.packages("faraway")
library(faraway)
data(africa)
?africa


## 2. Create two subsets of the data
## One with only countries were no military coups have occurred
## One where any military coups have occurred






## 3. Find a 95% confidence interval for the mean percent of voting in the last
## elections for each subset of countries







## 4. What do you learn about voter turnout in African countries from
## these confidence intervals?






## 5. Find the 2.5th and 97.5th percentiles of the t distribution
## with 5 degrees of freedom






## 6. Find the probability that x is at least two standard deviations
## above the mean of a t-distribution with 10 degrees of freedom






## 7. Find the density of the t-distribution with 10 degrees of 
## freedom at x = -1.96 and x = 1.96.  Explain the output given what
## we know about the t-distribution and the standard normal distribution





