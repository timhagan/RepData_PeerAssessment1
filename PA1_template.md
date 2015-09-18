---
title: "Peer Assessment 1"
output: html_document
author: Tim
date: September 18, 2015
---

Introduction
====================
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

Data
===================
The data for this assignment can be downloaded [here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Assignment
=============

**Loading and preprocessing data**

Download the data from the link above and save it to a local folder. Set your working directory using the 'setwd()' command to the folder in which you saved the 'activity.csv' data. Next, read in the data:


```r
# read in data
csv<- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

**What is the mean total number of steps taken per day?**

We will calculate the total number of steps taken per day and plot a histogram of these totals. Then we will look closer at some descriptive statistics, the mean and median of the total number of steps taken per day.


```r
# by aggregating the data, we can sum up the number of steps taken each day
daily.totals<- aggregate(data=csv[is.na(csv$steps)!="NA",], steps~date, FUN=sum)


hist(daily.totals$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(daily.totals$steps)
```

```
## [1] 10766.19
```

```r
median(daily.totals$steps)
```

```
## [1] 10765
```


**What is the average daily pattern?**

Now we will make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis). 


```r
# by aggregating the data, we can take the mean of the number of steps taken at each interval
interval.totals<- aggregate(data=csv[is.na(csv$interval)!="NA",], steps~interval, FUN=mean)

# then we can see what the average number of steps over the course of a day
plot(interval.totals$interval, interval.totals$steps, type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# we can also find the interval with the maximum average of steps taken
interval.totals[interval.totals$steps==max(interval.totals$steps), ]
```

```
##     interval    steps
## 104      835 206.1698
```

**Impute missing values**
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. We will examine the total number of missing values in the dataset, devise a strategy for filling in all of the missing values and create a new dataset where those missing values are filled in.


```r
# return the number of missing values
sum(complete.cases(csv)==F)
```

```
## [1] 2304
```

```r
# create a new dataset with those values filled in
newcsv<-csv
newcsv$steps[is.na(newcsv$steps)]<-mean(newcsv$steps, na.rm=T)

# use the aggregate function to find the total number of steps taken each day
newdaily.totals<- aggregate(data= newcsv, steps~date, FUN = sum)

# make a histogram of the total number steps taken each day with the new, imputed data
hist(newdaily.totals$steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# mean
mean(newdaily.totals$steps)
```

```
## [1] 10766.19
```

```r
# median
median(newdaily.totals$steps)
```

```
## [1] 10766.19
```

**Are there differences in activity patterns between weekdays and weekends?**
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.


```r
# add a column showing the weekday
newcsv$weekday<-weekdays(as.Date(newcsv$date))

# now we are able to subset based on weekend vs weekday
weekend<-newcsv[(grep("Saturday|Sunday", newcsv$weekday)),]
weekday<-newcsv[-(grep("Saturday|Sunday", newcsv$weekday)),]

# now we aggregate based on the intervals during weekends/weekdays
weekendagg<-aggregate(data= weekend, steps~interval, FUN = mean)
weekdayagg<-aggregate(data= weekday, steps~interval, FUN = mean)

#now we can look at a plot of the data
par(mfrow=c(2,1))
plot(weekendagg$interval, weekendagg$steps, type="l", main="Weekend", xlab = "Interval", ylab= "Steps Taken")
plot(weekdayagg$interval, weekdayagg$steps, type="l", main="Weekday", xlab = "Interval", ylab= "Steps Taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
