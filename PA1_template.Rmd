---
title: "Assignment1 - Reproduceable Research"
author: "Semir .A"
date: "February 16, 2019"
output:
  html_document: default
  pdf_document: default
  word_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Load Environment

```{r }
library(readxl)
library(dplyr)
library(ggplot2)
```

## Load the data
```{r }
unzip("repdata_data_activity.zip")
mydata <- read.csv("activity.csv")
str(mydata)
```

## Process/transform the data into a format suitable for your analysis
```{r}
#convert factor type to date type
mydata$date <- as.Date(mydata$date)
str(mydata)
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day
```{r}
stepsbyday <- mydata %>%
      group_by(date) %>%
      summarize(totalsteps = sum(steps))
```


Make a histogram of the total number of steps taken each day
```{r}
hist(stepsbyday$totalsteps, 
     xlab = "Total number of steps each day",
     main = "Histogram of number of steps taken per day")
```

Calculate and report the mean and median of the total number of steps taken per day
```{r}
mean(stepsbyday$totalsteps, na.rm = TRUE)
median(stepsbyday$totalsteps, na.rm = TRUE)
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
averageinterval <- mydata %>%
                        group_by(interval) %>%
                        summarize(Stepsbyinterval = mean(steps, na.rm = TRUE))
plot(averageinterval$interval, averageinterval$Stepsbyinterval, 
     type = "l",
     xlab = "Interval",
     ylab = "Average Steps taken",
     main = "Average steps taken during 5-minute interval")
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
averageinterval$interval[which.max(averageinterval$Stepsbyinterval)]
```


## Inputting Missing Values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
sum(is.na(mydata$steps))
table(is.na(mydata$steps))
```

Strategy used to fill missing values will be to fill missing values in the dataset using the mean value of the corresponding 5-minute interval.

Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
newset <- mapply(function(x,y)
      if (is.na(x)) {x=averageinterval$Stepsbyinterval[which(averageinterval$interval == y)]} else {x=x}, 
      x = mydata$steps, y =  mydata$interval, SIMPLIFY = TRUE)
mynewset <- mydata
mynewset$steps <- unlist(newset, use.names = FALSE)
```


Make a histogram of the total number of steps taken each day with missing data filled in
```{r}
newstepsbyday <- mynewset %>%
      group_by(date) %>%
      summarize(totalsteps = sum(steps))
hist(newstepsbyday$totalsteps, 
     xlab = "Total number of steps each day",
     main = "Histogram of number of steps taken per day")
```


Calculate and report the mean and median of the new total number of steps taken per day - with NA's filled in 
```{r}
mean(newstepsbyday$totalsteps, na.rm = TRUE)
median(newstepsbyday$totalsteps, na.rm = TRUE)
```

##Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
wdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
mynewset$daytype <- as.factor(mapply(function(x)
      ifelse (x %in% wdays, "weekday", "weekend"), x = weekdays(mynewset$date)))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
```{r}
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
```
