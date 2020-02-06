##############################
## Lab 7: Advanced Plotting ##
##############################

## Goals:
## 1. Refresh basic plotting skills
## 2. Learn methods to make plots more informative (and to help with posters)


## Load data on the 2008 U.S. presidential election by state
install.packages("Stat2Data")
library(Stat2Data)
data("Election08")
?Election08


#### Plot 1 -- Income and Party

## Bad labels, no title, we can't learn anything from this plot!
plot(x = Election08$Income, y = Election08$Dem.Rep) 

## Let's add as much info as we can to help the viewer of our plot

## Good labels
plot(x = Election08$Income, y = Election08$Dem.Rep,
     xlab = "2007 Per Capita Income",
     ylab = "%Dem - %Rep",
     main = "2008 Party Identification and\nPer Capita Income, By State")

## Line at 0 to emphasize break between majority dem/majority rep
abline(h = 0, lty = 2)

## How many above/below the line?
rep_n <- sum(Election08$Dem.Rep < 0) ## sum() how many obs satisfy this condition
dem_n <- sum(Election08$Dem.Rep > 0)

## Add labels
## Notice we pick the coordinates
## Notice the paste0() function
text(x = 55000, y = -10, labels = paste0("N=", rep_n))
text(x = 55000, y = 10, labels = paste0("N=", dem_n))

## Now can we learn something from this plot?



#### Plot 2 -- College Degree, Party, and Election Winner

## bad...
plot(x = Election08$BA, y = Election08$Dem.Rep) 

## Add labels
## Note type = "n" ...this plots everything except data points,
## which is helpful because it sets axes up right
plot(x = Election08$BA, y = Election08$Dem.Rep,
     xlab = "% of Adults with College Degree",
     ylab = "%Dem - %Rep",
     main = "2008 Party Identification and\nCollege Degree, By State",
     type = "n") 

## "lwd" is line width, "lty" is line type
abline(h = 0, lty = 2, lwd = 2)

?points ## Let's look at all pch options (0-25)

## What is this subset of the data?  Why are we doing two
## sets of points?
points(x = Election08$BA[Election08$ObamaWin == 1],
       y = Election08$Dem.Rep[Election08$ObamaWin == 1],
       col = "blue",
       pch = 16)

points(x = Election08$BA[Election08$ObamaWin == 0],
       y = Election08$Dem.Rep[Election08$ObamaWin == 0],
       col = "red",
       pch = 17)

## We added information by using different colors, so we need
## a legend.
legend("topleft", legend = c("2008 Obama Win", "2008 McCain Win"),
       pch = c(16, 17),
       col = c("blue", "red"),
       cex = .75,
       bty = "n")

## Who is that outlier? We should label it
## Note "which.max()" returns the index of the maximum value,
## and we want the state associated with it.
Election08$State[which.max(Election08$BA)] ## D.C.

## text() adds text inside the axes at coordinates we pick
text(x = 45, y = 75, labels = "D.C.", cex = .75)


#### Plot 3 -- More fun with labels

## Same empty plot as above
plot(x = Election08$BA, y = Election08$Dem.Rep,
     xlab = "% of Adults with College Degree",
     ylab = "%Dem - %Rep",
     main = "2008 Party Identification and\nCollege Degree, By State",
     type = "n")


## This time, let's label the points with their state abbreviation
## and assign color to the points in different way

## ifelse()... if TRUE return 1st thing, if FALSE return 2nd thing
my_cols <- ifelse(Election08$ObamaWin == 1, "blue", "red")

## What's going on here?
text(x = Election08$BA, y = Election08$Dem.Rep, ## coordinates for states
     labels = Election08$Abr, ## vector of state labels
     cex = .5,
     col = my_cols) ## vector of colors

abline(h = 0, lty = 2)

legend("topleft", legend = c("2008 Obama Win", "2008 McCain Win"),
       col = c("blue", "red"),
       lty = 1,
       cex = .75,
       bty = "n")


#### Other fun tools

## Jitter overlapping points
x <- y <- rep(1:5, 5)
plot(x = x, y = y) ## but we have 25 values?
plot(x = x, y = jitter(y))
plot(x = jitter(x), y = jitter(y))

## Add grid to background
grid()

## Add margin text
## "line" moves text's position in the margin
## "las" controls if text is horizontal/verticle
mtext(side = 1, text = "Down here!", line = 4) 
mtext(side = 4, text = "Over\nhere!", las = 1) 

## Choose which axes to use
plot(x = 1:10, y = 1:10, axes = FALSE)
axis(side = 1)
axis(side = 2)

## Add a line specifying intercept (a) and slope (b)
abline(a = 0, b = 1)
abline(a = 0, b = 2, col = "blue", lty = 3)



## 1. Load data on the mean lawyers' ratings of state judges
## in the US Superior Court in 1977
install.packages("datasets")
library("datasets")
data("USJudgeRatings")
?USJudgeRatings


## 2. Make an interesting, readable plot to visualize what types of 
## judges are given high ratings as "Worthy of retention." In other words,
## fix the ugly plot below by doing the following:
## - provide a title and label the axes
## - include information from a 3rd variable
##   (HINT: make a continuous variable into a dummy variable... maybe
##    above and below the mean or median... try ifelse())
## - use something other than the default colors and symbols
## - include a legend

plot(x = USJudgeRatings$WRIT, y = USJudgeRatings$RTEN)
mtext(side = 1, "Note: ratings on a scale of 1-10, with 10 as highest rating.",
      cex = .75, ## smaller font
      line = 4) ## below the axis label

