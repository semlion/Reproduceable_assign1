library(readxl)
library(dplyr)
library(ggplot2)

#Load the data (i.e. read.csv() )
unzip("repdata_data_activity.zip")
mydata <- read.csv("activity.csv")

#Process/transform the data (if necessary) into a format suitable for your analysis
str(mydata)
mydata$date <- as.Date(mydata$date)

#Calculate the total number of steps taken per day
stepsbyday <- mydata %>%
      group_by(date) %>%
      summarize(totalsteps = sum(steps))

#Make a histogram of the total number of steps taken each day
hist(stepsbyday$totalsteps, 
     xlab = "Total number of steps each day",
     main = "Histogram of number of steps taken per day")

#Calculate and report the mean and median of the total number of steps taken per day
mean(stepsbyday$totalsteps, na.rm = TRUE)
median(stepsbyday$totalsteps, na.rm = TRUE)

#What is the average daily activity pattern?

#Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of 
#steps taken, averaged across all days (y-axis)

averageinterval <- mydata %>%
                        group_by(interval) %>%
                        summarize(Stepsbyinterval = mean(steps, na.rm = TRUE))

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
averageinterval$interval[which.max(averageinterval$Stepsbyinterval)]

plot(averageinterval$interval, averageinterval$Stepsbyinterval, 
     type = "l",
     xlab = "Interval",
     ylab = "Average Steps taken",
     main = "Average steps taken during 5-minute interval")


# Calculate and report the total number of missing values in the dataset 
#(i.e. the total number of rows with NAs)
sum(is.na(mydata$steps))
table(is.na(mydata$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
# For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

# Strategy will be to take average value for that interval.  This can be obtained by the following formula:
# averageinterval$Stepsbyinterval[which(averageinterval$interval == x)]  // where x == interval value

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
newset <- mapply(function(x,y)
      if (is.na(x)) {x=averageinterval$Stepsbyinterval[which(averageinterval$interval == y)]} else {x=x}, 
      x = mydata$steps, y =  mydata$interval, SIMPLIFY = TRUE)

mynewset <- mydata
mynewset$steps <- unlist(newset, use.names = FALSE)


# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
# number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
# What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Calculate the total number of steps taken per day
newstepsbyday <- mynewset %>%
      group_by(date) %>%
      summarize(totalsteps = sum(steps))

#Make a histogram of the total number of steps taken each day
hist(newstepsbyday$totalsteps, 
     xlab = "Total number of steps each day",
     main = "Histogram of number of steps taken per day")

#Calculate and report the mean and median of the total number of steps taken per day
mean(stepsbyday$totalsteps, na.rm = TRUE)
median(stepsbyday$totalsteps, na.rm = TRUE)

#Calculate and report the mean and median of the new total number of steps taken per day - with NA's filled in 
mean(newstepsbyday$totalsteps, na.rm = TRUE)
median(newstepsbyday$totalsteps, na.rm = TRUE)


#Are there differences in activity patterns between weekdays and weekends?

wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
mynewset$daytype <- as.factor(mapply(function(x)
      ifelse (x %in% wdays, "weekday", "weekend"), x = weekdays(mynewset$date)))

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
# interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

averageintervalbydaytype <- mynewset %>%
      group_by(interval, daytype) %>%
      summarize(Stepsbyinterval = mean(steps, na.rm = TRUE))

ggplot(averageintervalbydaytype, aes(x=interval, y=Stepsbyinterval, color=daytype)) +
      geom_line() +
      facet_wrap(~daytype, nrow=2, ncol = 1) +
      labs(title="Average number of steps taken, averaged across all weekday days and weekend days",
           x = "5-minute interval", y = "Average number of steps taken")

ggplot(averageintervalbydaytype, aes(x=interval, y=Stepsbyinterval, color=daytype)) +
      geom_line() +
      labs(title="Average number of steps taken, averaged across all weekday days and weekend days",
           x = "5-minute interval", y = "Average number of steps taken")
