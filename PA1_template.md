# Reproducible Research: Peer Assessment 1

Load libraries.


```r
library(ggplot2)
library(lubridate)
```

## Loading and preprocessing the data

1. Load the data

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
data <- read.csv('activity.csv')
```
2. Process the data

The data does not require further processing, though some changes could be beneficial, such as adding datetimes.

## What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day

Apply the sum function on steps by date, and plot this with ggplot. Binwidth is arbitrarily set to 1000.

```r
total.steps <- tapply(data$steps, data$date, sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total Steps", ylab="Frequency")
```

![](./PA1_template_files/figure-html/total_steps_histogram-1.png) 

2. Calculate and report the mean and median total number of steps taken per day

The mean and median are calculated with the standard functions.


```r
mean(total.steps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(total.steps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

This is done by creating a dataframe with steps by interval, using the mean function. Plot this using ggplot.


```r
averages <- aggregate(steps~interval, data, mean, rm.na=TRUE)
ggplot(averages) + aes(x = interval, y = steps) + geom_line() +
  labs(title = "Time Series Plot of Average Number of Steps Per Interval",
       x="5-Minute Intervals", y="Average Number of Steps")
```

![](./PA1_template_files/figure-html/time_series_averages_plot-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
averages$interval[which.max(averages$steps)]
```

```
## [1] 835
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.

The strategy used is the mean, as calculated above. For each missing value, replace with the the mean number of steps for the same interval.


```r
data.imputed <- data

for (i in 1:nrow(data)) {
    if (is.na(data$steps[i])) {
        data.imputed$steps[i] <- averages$steps[which(averages$interval == data$interval[i])]
    }
}
```

3. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

Using the same method as above, apply the sum function across the imputed data (this time not needing to skip over na's). Print out the plot.


```r
total.steps.imputed <- tapply(data.imputed$steps, data.imputed$date, sum)
qplot(total.steps.imputed, binwidth=1000, xlab="Total Steps", ylab="Frequency")
```

![](./PA1_template_files/figure-html/imputed_data_mean_median-1.png) 

```r
mean(total.steps.imputed, na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(total.steps.imputed, na.rm=TRUE)
```

```
## [1] 10766.19
```

The mean and median are both higher than before (see above for previous values). Also, unlike previously, the mean and the median are now the same value.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data.imputed$daytype <- as.factor(ifelse(wday(data.imputed$date, label = TRUE) %in% 
    c("Sat", "Sun"), "weekend", "weekday"))
data.imputed <- split(data.imputed, data.imputed$daytype)
averages.weekday <- aggregate(steps~interval, data.imputed$weekday, mean)
averages.weekend <- aggregate(steps~interval, data.imputed$weekend, mean)
averages.weekday$daytype <- factor(rep("weekday", nrow(averages.weekday)))
averages.weekend$daytype <- factor(rep("weekend", nrow(averages.weekend)))
averages.combined <- rbind(averages.weekday, averages.weekend)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

This uses the facet_grid function from ggplot to split by daytype.


```r
ggplot(averages.combined) + aes(x = interval, y = steps) + facet_grid(daytype ~ 
    .) + geom_line() + labs(title = "Time Series Plot of Average Number of Steps Per Interval", 
    y = "Number of steps")
```

![](./PA1_template_files/figure-html/time_series_by_day_type-1.png) 
