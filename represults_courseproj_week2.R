# course project for week 2 of reproducible research

# read in libraries to use
library(dplyr)
library(ggplot2)
library(knitr)

# read in the data file
activity <- read.csv("activity.csv", header = TRUE)

# convert interval to a time of day
activity$interval <- sprintf("%04d", activity$interval)
activity$tod <- format(strptime(activity$interval, format = "%H%M"), "%H:%M")

# Question 1: What is mean total number of steps taken per day?
dailysteps <- group_by(activity, date) %>% summarize(totsteps = sum(steps, na.rm = TRUE))
with(dailysteps, {
     hist(totsteps, xlab = "Total Daily Steps", ylab = "# of Days", col = "green",
          main = "Total Daily Steps")})
print(paste("Average Daily Steps is ", mean(dailysteps$totsteps)))
print(paste("Median Daily Steps is ", median(dailysteps$totsteps)))

# Question 2: What is the average daily activity pattern?
activitypattern <- group_by(activity, tod) %>% 
    summarize(intervalsteps = mean(steps, na.rm = TRUE))
with(activitypattern, {
     plot(intervalsteps, type = "l", xlab = "Time Interval", ylab = "Mean # of Steps")})
maxactivity <- round(max(activitypattern$intervalsteps))
print(paste("Maximum average steps in a 5 minute interval is ", maxactivity))

# Imputing missing values
compind <- complete.cases(activity)
# create table to summarize the number of missing cases
table(compind)
# fill in missing values with mean for that interval
# add the missing cases indicator to the data set
activity2 <- cbind(activity, compind)
# create new data set using the interval means to impute missing data
newactivity <- activity2 %>% merge(activitypattern, by.x = "tod") %>%
    mutate(steps = ifelse(is.na(steps)==TRUE, intervalsteps, steps)) %>%
    arrange(date, interval) %>% select(steps, date, interval)

dailysteps2 <- group_by(newactivity, date) %>% summarize(totsteps = sum(steps, na.rm = TRUE))
with(dailysteps2, {
    hist(totsteps, xlab = "Total Daily Steps", ylab = "# of Days", col = "green",
         main = "Total Daily Steps (Revised)")})
print(paste("Revised Average Daily Steps is ", mean(dailysteps2$totsteps)))
print(paste("Revised Median Daily Steps is ", median(dailysteps2$totsteps)))

# Question 3: are there differences in activity patterns b/t weekdays and weekends?
newactivity$day <- weekdays(as.POSIXct(newactivity$date))
newactivity$daytype <- ifelse((newactivity$day=="Sunday" | newactivity$day =="Saturday"), "weekend", "weekday")

compactivity <- group_by(newactivity, daytype, interval) %>%
    summarize(avgstep = mean(steps))
qplot(interval, avgstep, data = compactivity, facets = daytype ~ ., 
      xlab = "Time of Day", ylab = "Average Steps", 
      main = "Average Steps by Time of Day by Day Type")



