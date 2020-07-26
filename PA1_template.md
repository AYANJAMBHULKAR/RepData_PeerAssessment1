---
title: '"Reproducible Research: Peer Assessment 1"'
author: "YOU!"
date: "27/07/2020"
output: html_document
---
## Loading and preprocessing the data

```{r}
data <- read.csv("D:/Coursera/ASSIGNMENT/7/activity.csv")
str(data)
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)
```

## Histogram of the total number of steps taken each day

```{r}
NA_index <- is.na(as.character(data$steps))
data_no_NA <- data[!NA_index,]
head(data_no_NA)

steps_each_day <- aggregate(steps ~ date, data = data_no_NA, sum)

colnames(steps_each_day) <- c("date", "steps")

hist(as.numeric(steps_each_day$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")
```

## Mean and median number of steps taken each day

```{r}
mean(steps_each_day$steps)

median(steps_each_day$steps)
```

## Time series plot of the average number of steps taken

```{r}
steps_per_interval <- aggregate(data_no_NA$steps, by=list(interval=data_no_NA$interval), FUN=mean)

colnames(steps_per_interval) <- c("interval", "average_steps")


plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")
```


## The 5-minute interval that, on average, contains the maximum number of steps

```{r}
max_steps <- max(steps_per_interval$average_steps)
max_steps
```

```{r}
intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
intervale_max_steps
```

## Code to describe and show a strategy for imputing missing data
```{r}
sum(is.na(as.character(data$steps)))
```

```{r}
sum(is.na(as.character(data$date)))
```

```{r}
sum(is.na(as.character(data$interval)))
```

```{r}
NA_index <- which(is.na(as.character(data$steps)))
complete_data <- data

complete_data[NA_index, ]$steps<-unlist(lapply(NA_index, FUN=function(NA_index){
                steps_per_interval[data[NA_index,]$interval==steps_per_interval$interval,]$average_steps
                }))

```

```{r}
summary(complete_data)

```

```{r}
str(complete_data)
```
## Histogram of the total number of steps taken each day after missing values are imputed

```{r}
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)

colnames(steps_each_day_complete) <- c("date", "steps")

hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "red", xlab = "Number of Steps", main= "Histogram of the total number of steps taken each day")

```

```{r}
mean(steps_each_day_complete$steps)
```

```{r}
median(steps_each_day_complete$steps)
```

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekend
```{r}
complete_data$day <- as.factor(weekdays(complete_data$date))

complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE)

weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)

weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)

week_data$day <- as.factor(week_data$day)

library(lattice)
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps")
```