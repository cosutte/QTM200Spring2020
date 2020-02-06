############################################################
## Lab 6: Functions in R                                  ##
############################################################


## Goals:
## 1. Learn how to write functions in R
## 2. Write a function that conducts a null hypothesis test



## We know how to construct confidence intervals in R.
set.seed(1234567)
x <- rnorm(100, mean=50, sd=30)

z95 <- qnorm((1 - .95)/2, lower.tail = FALSE) ## (1-confidence coefficient)/2
n <- length(na.omit(x))
sample_mean <- mean(x, na.rm = TRUE)
sample_sd <- sd(x, na.rm = TRUE)
lower_95 <- sample_mean - (z95 * (sample_sd/sqrt(n)))
upper_95 <- sample_mean + (z95 * (sample_sd/sqrt(n)))
confint95 <- c(lower_95, upper_95)
confint95



## Imagine we want to calculate 99%, 95%, 90%, 85%, 80%, ... confidence intervals.
## It is very inefficient to copy/paste the codes above every time. Use a function!
getCI <- function(data, conf.level){
  z <- qnorm((1 - conf.level)/2, lower.tail = FALSE)
  n <- length(na.omit(data))
  sample_mean <- mean(data, na.rm = TRUE)
  sample_sd <- sd(data, na.rm = TRUE)
  lower <- sample_mean - (z * (sample_sd/sqrt(n)))
  upper <- sample_mean + (z * (sample_sd/sqrt(n)))
  confint <- c(lower, upper)
  
  return(confint)
}

getCI(data=x, conf.level=0.95)  # 95%
getCI(data=x, conf.level=0.99)  # 99%
getCI(data=x, conf.level=0.9)   # 90%
getCI(data=x, conf.level=0.8)   # 80%



## We have been using functions without knowing that.
is.function(mean)
is.function(sqrt)
is.function(rnorm)



## This is the skeleton of any functions
FUNCTIONNAME <- function(INPUTNAME1, INPUTNAME2, ...){
  "Type tasks you want to execute here"
} 



## A very simple function to add 1
addOne <- function(number){
  new <- number + 1
  
  return(new)
}

addOne(1)
addOne(100)

# You can simplify
addOne2 <- function(number){
  number + 1
}

addOne2(1)
addOne2(100)



## A function to calculate the sum of the squares of three numbers
superSumSquare <- function(number1, number2, number3){
  out <- number1^2 + number2^2 + number3^2
  
  return(out)
}

superSumSquare(1, 2, 3)  # 1^2 + 2^2 + 3^2
superSumSquare(5, 10, 5)  # 5^2 + 10^2 + 5^2



## A function to calculate mean
fancyMean <- function(vector){
  sum <- sum(vector)
  n <- length(vector)
  mean <- sum/n
  
  return(mean)
}

fancyMean(1:10)
# which is same as
mean(1:10)



## A function to calculate variance
awesomeVariance <- function(vector){
  mean.y <- mean(vector)
  y2 <- (vector - mean.y)^2
  sum.y2 <- sum(y2)
  n <- length(vector)
  return(sum.y2/(n-1))
}

awesomeVariance(1:20)
# which is same as
var(1:20)

# Again, you can simplify
awesomeVariance2 <- function(vector){
  sum((vector - mean(vector))^2)/(length(vector) - 1)
}

awesomeVariance2(1:20)



### Exercises


## 1. Write a function that conducts a one-tail t-test and return a p-value
##    when H0: mu >= mu0 and H1: mu < mu0.
##    Hint 1: You need two inputs: (1) sample (numeric vector) and (2) null hypothesis (mu0).
##    Hint 2: Since the population variance is unknown and our sample is small (Q3), you 
##    should use a t-distribution with n-1 degrees of freedom.




## 2. Using the function above, test whether you can reject the null that (1) mu0 >= 73 and 
##    (2) mu0 >= 80 with 95% confidence level.

our_sample <- c(63, 75, 84, 58, 52, 96, 63, 55, 76, 83)


## 3. Create a function that conducts a two-tail t-test and returns a p-value
##    when H0: mu = mu0 and H1: mu != mu0. Then, test whether you can reject the null that
##    (1) mu0 = 73 and (2) mu0 = 80 using our_sample
