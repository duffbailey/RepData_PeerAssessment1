# Code for reading in the dataset and/or processing the data
# load libraries
rm(list=ls())
library(dplyr)
library(tidyr)
x_activity <- read.csv("Activity/Activity.csv", header = TRUE, col.names = c("steps", "date", "interval"))
x_activity_daily <- x_activity %>%
group_by(date)  %>%
  summarise(dailySteps = sum(steps))


#  Histogram of the total number of steps taken each day
hist(x_activity_daily$dailySteps, breaks = 25)

#  Mean and median number of steps taken each day
x_activity_mean <- mean(x_activity_daily$dailySteps, na.rm = TRUE)
x_activity_median <- median(x_activity_daily$dailySteps, na.rm = TRUE)

#Time series plot of the average number of steps taken
x_activity_int <- x_activity %>%
  group_by(interval)  %>%
  summarise(IntSteps = sum(steps, na.rm = TRUE), MeanSteps = mean(steps, na.rm = TRUE))


plot(x_activity_int$interval, x_activity_int$MeanSteps, type="l")



#The 5-minute interval that, on average, contains the maximum number of steps
max(subset(x_activity_int,select = MeanSteps))


#Code to describe and show a strategy for imputing missing data
#  strategy is to fill any interval where steps is NA with the mean steps for that interval
# for the days where that interval has a value.
#

x_activity_filled <- left_join(x_activity, x_activity_int, by= "interval")


z <- 0
i = 1
while (i <= length(x_activity_filled$steps)) {
    if (is.na(x_activity_filled[i,1])) {
     #    z <- z + 1
        x_activity_filled[i,1] <- x_activity_filled[i,5]
    }
  i <- i + 1
}
x_activity_filled <- mutate(x_activity_filled, dtype = ifelse(weekdays(as.Date(date)) == "Saturday" | weekdays(as.Date(date)) == "Sunday", "weekend", "weekday"))


#Histogram of the total number of steps taken each day after missing values are imputed
x_activity_daily_filled <- x_activity_filled %>%
  group_by(date)  %>%
  summarise(dailySteps = sum(steps))


#  Histogram of the total number of steps taken each day
hist(x_activity_daily_filled$dailySteps, breaks = 25)

#  Mean and median number of steps taken each day
x_activity_mean_filled <- mean(x_activity_daily_filled$dailySteps, na.rm = TRUE)
x_activity_median_filled <- median(x_activity_daily_filled$dailySteps, na.rm = TRUE)

#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays
# and weekends

x_activity_int_filled_weekday <- x_activity_filled %>%
  filter(dtype == 'weekday')  %>%
  group_by(interval)  %>%
  summarise(IntSteps = sum(steps, na.rm = TRUE), MeanSteps = mean(steps, na.rm = TRUE))



x_activity_int_filled_weekend <- x_activity_filled %>%
    filter(dtype == 'weekend') %>%
  group_by(interval)  %>%
    summarise(IntSteps = sum(steps, na.rm = TRUE), MeanSteps = mean(steps, na.rm = TRUE))


  par(mfrow=c(2,1))

  plot(x_activity_int_filled_weekday$interval, x_activity_int$MeanSteps, type="l")

  plot(x_activity_int_filled_weekend$interval, x_activity_int$MeanSteps, type="l")

  dev.copy(png, file = "wkday_wkend.png")
  dev.off()

