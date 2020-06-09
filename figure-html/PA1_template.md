---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data



## What is mean total number of steps taken per day?


```r
# Summarise the number of steps by day in a table

activity_day <- activity %>%
  filter(steps != "NA") %>%
  mutate(steps_n = as.numeric(steps))%>%
  group_by(date) %>%
  summarise(steps_n = sum(steps_n, na.rm = TRUE)) %>%
  ungroup()

# Calculate the mean and median

activity_day_mean <- round(mean(activity_day$steps_n),2)
activity_day_median <- round(median(activity_day$steps_n),2)

# Generate a histogram

hist(activity_day$steps_n, 
     breaks=10,
     main="Steps per day", 
     col="blue", 
     xlab="Number of steps")
```

![](figures/PA1_template_files/figure-html/steps-1.png)<!-- -->

The mean steps per day is **10766**  
The median of steps taken per day is **10765**   

## What is the average daily activity pattern?


```r
# Calculate summary table of steps per interval in the day

activity_time <- activity %>%
  filter(steps != "NA") %>%
  mutate(steps_n = as.numeric(steps))%>%
  group_by(interval) %>%
  summarise(steps_n = mean(steps_n, na.rm = TRUE)) %>%
  ungroup()

# Calculate the peak time in the day

peak_interval <- activity_time[which.max(activity_time$steps_n),1]

# Generate a time series graph

plot(activity_time$interval, activity_time$steps_n, type = "l", xlab = "Time in the day",
     ylab = "Average number of steps")
```

![](figures/PA1_template_files/figure-html/daily-1.png)<!-- -->

The interval with the highest average steps across this dataset is **835**

## Imputing missing values


```r
# Calculate the number of missing values in the dataset

activity_NA <- activity %>%
  mutate(NA_flag = ifelse(steps == "NA", "Y", "N")) %>%
  group_by(NA_flag) %>%
  summarise(count = n()) %>%
  ungroup()

# Number of missing values

missing_values <- activity_NA[2,2]

# Calculate a substitute value for the NA

# Rather than excluding NA, I will match in the average number of steps from the average steps by interval table previously generated

activity_time_02 <- activity_time %>%
  transmute(interval,
            avg_steps = steps_n)

activity_replace <- activity %>%
  left_join(activity_time_02, by = "interval") %>%
  mutate(steps_replace = ifelse(steps == "NA", avg_steps, steps),
         steps_replace = as.numeric(steps_replace)) %>%
  group_by(date) %>%
  summarise(steps_replace = sum(steps_replace, na.rm = TRUE)) %>%
  ungroup()
  

# Calculate the mean and median following replacement

activity_replace_day_mean <- round(mean(activity_replace$steps_replace),2)
activity_replace_day_median <- round(median(activity_replace$steps_replace),2)

# Generate a histogram to review

hist(activity_replace$steps_replace, 
     breaks=10,
     main="Avg. steps per day (with NA replacement)", 
     col="red", 
     xlab="Avg. steps (with NA replacement)")
```

![](figures/PA1_template_files/figure-html/replace-1.png)<!-- -->

The number of missing values in the dataset is **2304**  
After replacement, the restated mean steps per day is **10766**  
After replacement, the median of steps taken per day is **10766**  
There is very little difference when compared to the mean & median with NA's removed. Missing data has no impact.



## Are there differences in activity patterns between weekdays and weekends?


```r
# Generate the weekend / weekday flag
# Will use the replace dataset for completeness given the mean & median are comparable

activity_replace_02 <- activity %>%
  left_join(activity_time_02, by = "interval") %>%
  mutate(steps_replace = ifelse(steps == "NA", avg_steps, steps),
         steps_replace = as.numeric(steps_replace))

activity_weekday <- activity_replace_02 %>%
  mutate(
    interval_02 = as.POSIXct(date),
    weekday = ifelse(weekdays(interval_02) == "Saturday" | weekdays(interval_02) == "Sunday", "N", "Y"))

activity_weekday_02 <- activity_weekday %>%
  filter(weekday == "Y") %>%
  group_by(interval) %>%
  summarise(steps_replace = sum(steps_replace)) %>%
  ungroup()

activity_weekend <- activity_weekday %>%
  filter(weekday == "N") %>%
  group_by(interval) %>%
  summarise(steps_replace = sum(steps_replace)) %>%
  ungroup()

# Generate a panel plot histogram to review

par(mfrow=c(2,1))

plot(activity_weekday_02$interval, activity_weekday_02$steps_replace, type = "l", xlab = "Weekday: Average steps per interval",
     ylab = "Avg. steps")

plot(activity_weekend$interval, activity_weekend$steps_replace, type = "l", xlab = "Weekend: Average steps per interval",
     ylab = "Avg. steps")
```

![](figures/PA1_template_files/figure-html/weekday-1.png)<!-- -->
