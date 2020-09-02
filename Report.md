---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




```r
library(dplyr)
library(readr)
library(magrittr)
library(Hmisc)
```

## Load Data


```r
df = readr::read_csv("activity.csv")
```

## Data Check


```r
# Structure
str(df)
```

```
## Classes 'spec_tbl_df', 'tbl_df', 'tbl' and 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   steps = col_double(),
##   ..   date = col_date(format = ""),
##   ..   interval = col_double()
##   .. )
```

```r
# Summary
summary(df)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
# Percentage of missing datapoints
sapply(df, function(x) sum(is.na(x))/nrow(df)*100)
```

```
##    steps     date interval 
## 13.11475  0.00000  0.00000
```

```r
# Analyzing Distribution
plot(density(na.omit(df$steps)))
```

![](Report_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
boxplot(na.omit(df$steps))
```

![](Report_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

## Process Data

NA values are remove. A new set of dataframe is created in case of the original needs to be used later.

```r
df_new = df %>% filter(!is.na(steps))
```


## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1.  Calculate the total number of steps taken per day
2.  If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3.  Calculate and report the mean and median of the total number of steps taken per day


```r
steps_per_day = df_new %>% group_by(date) %>% summarise(steps_day = sum(steps))
hist(steps_per_day$steps_day,main = "Histogram of the total number of steps per day",xlab = "Steps")
```

![](Report_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# The mean and Median
mean(steps_per_day$steps_day)
```

```
## [1] 10766.19
```

```r
median(steps_per_day$steps_day)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1.  Make a time series plot __(type = "l")__ of the __5-minute interval__ (x-axis) and the average number of steps taken, averaged across __all days (y-axis)__
2.  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# Average steps per day by interval
avg_steps_interval = df_new %>% group_by(interval) %>% summarise(avg_steps = mean(steps))
# Plot
plot(avg_steps_interval$interval, avg_steps_interval$avg_steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average Number of Steps")
```

![](Report_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
max_avg_interval =avg_steps_interval %>% filter(avg_steps == max(avg_steps))
paste("The time interval that contains the maximum number of steps is",max_avg_interval$interval,"and the average steps recorded are",round(max_avg_interval$avg_steps),"steps")
```

```
## [1] "The time interval that contains the maximum number of steps is 835 and the average steps recorded are 206 steps"
```

## Imputing missing values
Note that there are a number of days/intervals where there are missing values __NA__. The presence of missing days may introduce bias into some calculations or summaries of the data.

1.  Calculate and report the total number of missing values in the dataset 
2.  Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3.  Create a new dataset that is equal to the original dataset but with the missing data filled in.
4.  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
# Total Number of NAs
sapply(df, function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

First, lets see how many NA values are there per day


```r
na_per_day = df %>% filter(is.na(steps)) %>% group_by(date) %>% count(date)
DT::datatable(na_per_day)
```

<!--html_preserve--><div id="htmlwidget-16e1c10c25d6604c6931" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-16e1c10c25d6604c6931">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8"],["2012-10-01","2012-10-08","2012-11-01","2012-11-04","2012-11-09","2012-11-10","2012-11-14","2012-11-30"],[288,288,288,288,288,288,288,288]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>date<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

The NA values are consistent across all 8 days. Nonetheless, we take the average steps per day to be imputed.


```r
df_imputed = df %>% mutate(steps= round(impute(df$steps, fun=mean), digits = 0))

# Summary of before impute
summary(df)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
summary(df_imputed)
```

```
## 
##  2304 values imputed to 37
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.33   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 37.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0
```

```r
hist(na.omit(df$steps))
```

![](Report_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
hist(df_imputed$steps)
```

![](Report_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

Verdict: The comparison between both histograms are almost identical. However, the distribution showed major difference, most notably the third quartile has shifted significantly. The variables supplied isn't sufficient to enable us to do more testing.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.  Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2.  Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
# Add Meta
df_imputed = df_imputed %>% mutate(week_day = weekdays(date)) %>% mutate(day_status = ifelse(!week_day %in% c("Saturday","Sunday"),"Weekdays","Weekend"))
# Correcting the variable class
df_imputed$day_status %<>% as.factor
# Reshape
df_impute_by_typeDay = df_imputed %>% group_by(day_status,interval) %>% summarise(steps = mean(steps))
# Plot
ggplot(df_impute_by_typeDay, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_status)) +
    theme_gray() +
    facet_grid(day_status ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("Number of steps per Interval by day status")
```

![](Report_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

