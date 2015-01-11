# Reproducible Research: Peer Assessment 1
Yading Song  
10 January 2015  

## Summary
This is a short report for "reproducible research" on coursera. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day. 

The main task of this assignment is to transform the raw data into different meaningful result.

## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
raw_data <- read.csv("activity.csv")
raw_data$date <- as.Date(raw_data$date)

# Add dummy variables for weekend and weekdays 
weekend <- c("Saturday", "Sunday")
raw_data$IsWeekend <- weekdays(raw_data$date) %in% weekend

# Aggregate the data by date to find sum steps for each day
steps_sum <- aggregate(raw_data$steps, list(Date = raw_data$date), FUN=sum, na.rm=TRUE)
# Aggregate the data by interval 
steps_by_interval <- aggregate(raw_data$steps, list(Interval = raw_data$interval), na.rm=TRUE, FUN=mean)
#steps_mean <- aggregate(raw_data$steps, list(Date = raw_data$date), FUN=mean, na.rm=TRUE)
#steps_median <- aggregate(raw_data$steps, list(Date = raw_data$date), FUN=median, na.rm=TRUE)
#processed_data <- merge(steps_mean, steps_median, by="Date")
```


## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day


```r
hist(steps_sum$x, xlab="Number of steps per day", ylab="Frequency", main="Histogram of the total number of steps taken each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# mean and median total number of steps taken per day, NA value ignored
print(mean(steps_sum$x, na.rm=TRUE))
```

```
## [1] 9354.23
```

```r
print(median(steps_sum$x, na.rm=TRUE))
```

```
## [1] 10395
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
plot(steps_by_interval$Interval, steps_by_interval$x, type="l", xlab="Interval", ylab="Total steps", main="5-minute interval with the average number of steps taken")
```

![](./PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# which interval contains the maximum number of steps 
print(steps_by_interval[which(steps_by_interval$x==max(steps_by_interval$x)),])
```

```
##     Interval        x
## 104      835 206.1698
```




## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Strategy: the missing values are replaced by the mean for that 5 minute interval 


```r
# total number of missing value
print(sum(is.na(raw_data$steps)))
```

```
## [1] 2304
```

```r
total_points <- nrow(raw_data)
filled_steps <- vector()

# the missing values are replaced by the mean for that 5 minute interval 
for (i in 1:total_points){
    if (is.na(raw_data[i,1])){
        if ((i%%288) == 0){
            filled_steps[i] <- steps_by_interval$x[288] 
        }
        else {
            filled_steps[i] <- steps_by_interval$x[(i%%288)] 
        }
    }
    else{
        filled_steps[i] <- raw_data$steps[i]
    }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# create new dataset with filled data points
filled_raw_data <- data.frame(filled_steps, raw_data$date, raw_data$interval, raw_data$IsWeekend)

# now the number of na values -> 0
print(sum(is.na(filled_raw_data$filled_steps)))
```

```
## [1] 0
```

```r
filled_steps_sum <- aggregate(filled_raw_data$filled_steps, list(Date = filled_raw_data$raw_data.date), FUN=sum)

# Histogram of the total number of steps taken each day
hist(filled_steps_sum$x, xlab="Number of steps per day", ylab="Frequency", main="Histogram of the total number of steps taken each day")
```

![](./PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
print(mean(filled_steps_sum$x))
```

```
## [1] 10766.19
```

```r
print(median(filled_steps_sum$x))
```

```
## [1] 10766.19
```

Imputing missing value makes the result less biased. Because the missing data maybe caused by certain reason, and that information was not considered in the analysis. After imputing the data, the mean and median both increased, and they are more similar.  


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
# change names of the data frame 
names(filled_raw_data) <- c("steps", "date", "interval", "isWeekend")

# dummy variable created. 
filled_raw_data$isWeekend[filled_raw_data$isWeekend == TRUE] <- c("weekend")
filled_raw_data$isWeekend[filled_raw_data$isWeekend == FALSE] <- c("weekday")

week_interval <- aggregate(steps~interval+isWeekend, data=filled_raw_data, sum)
weekday_interval <- subset(week_interval, isWeekend=="weekday")
weekend_interval <- subset(week_interval, isWeekend=="weekend")

# panel plots 
par(mfrow=c(2,1))
plot(weekday_interval$interval, weekday_interval$steps, type="l", xlab="Interval", ylab="number of steps", main="weekday")
plot(weekend_interval$interval, weekend_interval$steps, type="l", xlab="Interval", ylab="number of steps", main="weekend")
```

![](./PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
