###########################################################
## Lab 8: t-test and chi-squared test                   ##
###########################################################

## Goals:
## 1. Difference in means test in R
## 2. Contingency tables in R


##
## 1. Difference in means test
##
help(t.test)


social <- read.csv("social.csv")
colnames(social)

# Prior to the 2006 primary, randomly selected voters received one of 
# the following "treatment" messages:
#  - Civic Duty (It's your duty to vote)
#  - Hawthorne (You're being studied)
#  - Neighbors (Your neighbors will know if you voted)
# Those who received one of these treatments are coded as "Treatment".

# Q: Do these treatments (social pressures) increase turnout?

# Source: Gerber, Green, and Larimer. 2008. "Social Pressure and Voter 
# Turnout: Evidence from a Large-Scale Field Experiment." 
# American Political Science Review 102 (1).

table(social$messages)
table(social$treatment)

table(social$treatment, social$primary2006)


# Pre-treatment difference in 2004
t.test(primary2004 ~ treatment, data=social,
       alternative="two.sided", var.equal=FALSE) # default options


# Post-treatment difference in 2006
t.test(primary2006 ~ treatment, data=social,
       alternative="two.sided", var.equal=FALSE) # default options


# One-tailed test
t.test(primary2006 ~ treatment, data=social,
       alternative="less")

t.test(primary2006 ~ treatment, data=social,
       alternative="greater")


# If we assume equal variance, use var.equal=TRUE


# Alternatively, we can use two outcome vectors
treat.turnouot <- social$primary2006[social$treatment=="Treatment"]

control.turnout <- social$primary2006[social$treatment=="Control"]

t.test(treat.turnouot, control.turnout) # two-tailed


##
## 2. Contingency tables and Chi-squared test
##
help(chisq.test)


# Let's make a fake contingency table for gender and party ID
fake.tab <- matrix(c(762, 327, 468, 484, 239, 677), nrow=2, byrow=TRUE)
dimnames(fake.tab) <- list(Gender=c("Female", "Male"),
                           Party=c("Democrat", "Independent", "Republican"))
fake.tab <- as.table(fake.tab)
fake.tab

# The null hypothesis we want to test is that people's party identification 
# is independent of their gender.


# Chi-squared test
chisq.test(fake.tab)


### Exercises 

# 1. Prior to the 2004 election, voters were contacted and randomly 
#    given either a positive or negative message on an issue. The messages 
#    differed only in whether the information critiqued the incumbent 
#    Republican administration (negative frame) or extoled the benefits of 
#    Democratic proposals (positive frame).

# Source: Arceneaux, Kevin and DavidW. Nickerson. 2010. "Comparing Negative 
# and Positive CampaignMessages: Evidence From Two Field Experiments." 
# American Politics Research 38:54-83.

negative <- read.csv("negative_ads.csv")
colnames(negative)

# Do negative messages make a difference for voter turnout? Conduct an
# appropriate test and interpret the result.

# treatment: 1 = negative frame (control), 2 = positive frame (treatment)
# voted02p: 0 = did not vote, 1 = voted




# 2. In the movie Titanic, the third class passengers were not allowed 
#    to go up to the deck where the life-boats were boarding because the 
#    first-class passengers would board before them. This led to a sequence 
#    of tragic events.

install.packages("titanic")
library(titanic)
my.titanic <- titanic_train
View(my.titanic)

#    (1) Make a contingency table of the observed frequencies for the survival 
#    of passengers with respect to their class.
#    Hint: use table().
#    (2) Conduct a necessary hypothesis test with the null that survival and 
#    passanger class are independent. Interpret the result.




# 3. Some claimed that the "women and children first" evacuation policy 
#    helped them escape regardless of their class. Using only the women in 
#    the sample, make a contingency table to test this assertion. Do you think 
#    this evacuation policy worked?



