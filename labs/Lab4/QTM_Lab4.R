#############################################
## Lab 4: Sampling Distributions           ##
#############################################


#### Goals
#### a) Working with pnorm and qnorm
#### b) Practical example of a sampling distribution


## Go to the help file
help(Normal)


## 1. To generate random numbers from a normal distribution,
##    use rnorm(n=, mean=, sd=)

vec1 <- rnorm(100000, mean=0, sd=1)  # 100000 random numbers with mean=0 and sd=1
vec1

plot(density(vec1),
     main="Distribution of vec1",
     xlab="")

vec2 <- rnorm(5000, mean=50, sd=6)   # 5000 random numbers with mean=50 and sd=6
vec3 <- rnorm(5000, mean=35, sd=10)  # 5000 random numbers with mean=35 and sd=10

plot(density(vec2), 
     main="Distribution of vec2 and vec3",
     xlab="",
     col="red",
     xlim=c(0,100))
lines(density(vec3), lty=2, col="blue")


## 2: dnorm(x=, mean=, sd=) returns the value of the probability density function, 
##    or the height of a density curve, given x.

dnorm(0, mean=0, sd=1)
dnorm(-1, mean=0, sd=1)
dnorm(1, mean=0, sd=1)

# How to plot a normal curve using dnorm()
x.range <- seq(-4, 4, by=0.0001)                       # specify the range of the x-axis
plot(x.range, dnorm(x=x.range, mean=0, sd=1), 
     type="l",                                         # choose type=line
     main="Normal Distribution with mean=0 and sd=1",
     ylab="density",
     lwd=2,
     xaxt="n")
axis(1, at=-3:3, labels=-3:3)



## 3. pnorm(q=, mean=, sd=) returns a probability of drawing q or smaller from 
##    a normal distribution.

pnorm(0, mean=0, sd=1)
pnorm(-1, mean=0, sd=1)
pnorm(1, mean=0, sd=1)
pnorm(-1.96, mean=0, sd=1)

# What's the probability of drawing a value between -1.96 and 1.96?
pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1)



## 4. qnorm(p=, mean=, sd=) returns a value of the *p*th quantile.
##    This is an inverse of pnorm().

qnorm(0.5, mean=0, sd=1)
qnorm(0.15, mean=0, sd=1)
qnorm(0.85, mean=0, sd=1)
qnorm(0.025, mean=0, sd=1)

qnorm(c(0.025, 0.975), mean=0, sd=1)




## 5. Subsetting data

## == means "equal to"
## >, <, >=, <=

movies <- read.csv("movies.csv")

# Subset by movie genre
dramas <- movies[movies$genre=="Drama",]

table(dramas$genre)

# Subset by runtime
long.movies <- movies[movies$runtime >= 120,]

plot(density(movies$runtime, na.rm=T), 
     lty=2, 
     main="Distrubtion of Runtime",
     xlab="Rutime (minute)",
     col="gray50", 
     ylim=c(0,0.05))

lines(density(long.movies$runtime, na.rm=T))

legend("topright", 
       legend=c("All Movies", "Long Movies"),
       lty=c(2,1),
       col=c("gray50", "black"),
       bty="n",
       cex=0.8)


## Combine multiple conditions
## & means "and"
## | means "or"

# Subset by genre "and" release year
old_comedies <- movies[movies$genre=="Comedy" & movies$thtr_rel_year < 1980,]

# Get Drama "or" Walt Disney Pictures
drama_and_disney <- movies[movies$genre=="Drama" | movies$studio=="Walt Disney Pictures",]

table(drama_and_disney$genre)


## 1. Read in the Trump Job Approval poll data.
##    Variables are as follows:
##    - Approve = Proportion of the respondents who approve Trump
##    - survey_house = Survey company
##    - end_date = Date the survey ended
##    - sample_subpopulation = Sample type
##    - observations = Number of observations
##    - mode = Survey method



## 2. Plot a histogram of the Trump job approval rates. 



## 3. Suppose you only have the "Gallup" poll from "2/19/2017". If we know that
##    the population variance is 0.25, what is your estimate of the sampling 
##    distribution? 
##    Hint: Find this poll using two conditions (survey_house, end_date)



## 4. According to your answer in Q4, what are the 20th and 75th quantiles of 
##    the distribution?



## 5. Suppose a new poll suggests that the Trump approval rate is 47%. 
##    According to your answer in Q4, what is the probability of a poll showing 
##    support for Trump higher than this?



##################### Extra #####################

## We would like to know the long-term trends in Trum approval from
## Gallup, SurveyMonkey, and YouGov/Economist.
## Using "Approve" and "end_date", create a line plot that summarizes overtime 
## changes in Trump approval rates by survey company (draw three separate lines 
## for the three companies). Which one is most supportive of Trump?

# First, you have to run the following line (change the name of the data):
polls$end_date <- as.Date(polls$end_date, "%m/%d/%Y")


