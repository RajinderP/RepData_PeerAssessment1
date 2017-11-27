# Reproducible Research: Peer Assessment 1






## Loading and preprocessing the data
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. This data can be downloaded from:

https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

This data is read into from a csv file into a data frame with dates converted for the date column.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
library(lattice)

#Code for reading in the dataset and/or processing the data
setwd("C:\\Users\\bhutta\\Desktop\\Coursera\\reproducible research\\wk 2 ass")
df<-read.csv("activity.csv")
df$date <- as.Date(df$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
For each day where data exists, the total number of steps are taken over all the intervals in that day. A histogram is created to look at the distribution of these values. The mean and median values are also calculated - these are fairly close and suggest a near symetrical distribution which is also suggested by the histogram.


```r
#Histogram of the total number of steps taken each day
totstep<-aggregate(steps ~ date, data = df, sum, na.rm = TRUE)
ggplot(data=totstep,aes(steps))+geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
#Mean and median number of steps taken each day
mean(totstep$steps)
```

```
## [1] 10766.19
```

```r
median(totstep$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
For each time interval, the average number of steps are calculated over all the days for which values exist. The interval which gives the maximum value is then also extracted.


```r
#Time series plot of the average number of steps taken
meanstep<-na.omit(tapply(df$steps, df$interval, mean, na.rm = TRUE))
plot(row.names(meanstep), meanstep, type = "l", xlab = "5-min interval", 
     ylab = "Average across days", main = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#The 5-minute interval that, on average, contains the maximum number of steps
max_interval <- which.max(meanstep)
```

## Imputing missing values
Where data is missing, the mean value of the interval is now used to create a full dataset. The histogram above is now recreated with these updated values - and it can be seen that the mean and median values have fallen.


```r
#use interval average for missing values
sum(is.na(df))
```

```
## [1] 2304
```

```r
df2<-df%>%
  mutate(steps=ifelse(is.na(steps)== TRUE,meanstep[interval/5], steps))

#Histogram of the total number of steps taken each day after missing values are imputed
totstep2<-aggregate(steps ~ date, data = df2, sum, na.rm = TRUE)
ggplot(data=totstep2,aes(steps))+geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#Mean and median number of steps taken each day
mean(totstep2$steps)
```

```
## [1] 10285.23
```

```r
median(totstep2$steps)
```

```
## [1] 10395
```

```r
#The mean and median have both fallen when assuming NA values are equal to the average interval value
```

## Are there differences in activity patterns between weekdays and weekends?
The data is split into weekdays and weekends, where the averages of each interval are displayed in the graphs below. It can be seen that during the middle of the day, the average number of steps is higher on a weekend than the weekday - this could perhaps be explained by the fact that many of these people sit down while working.


```r
#Calculate averages given new weekday factor variable
df2<-df2%>%
  mutate(day = weekdays(date))%>%
  mutate(weekday = ifelse((day== "Saturday") |(day=="Sunday"),"Weekend","Weekday"))

meanstep2 <- aggregate(steps ~ weekday + interval, data = df2, mean)

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
xyplot(steps ~ interval | weekday, data = meanstep2, type = "l", lwd = 2,
       layout = c(1, 2), 
       xlab = "5-minute interval", 
       ylab = "Average number of steps",
       main = "Average Number of Steps Taken by weekday type")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
