# 1. Code for reading in the dataset and/or processing the data
activity <- read.csv("D:/Users/Jason/Documents/R/Research/activity.csv")
# 2. Histogram of the total number of steps taken each day

actStepsPerDay <-  aggregate(steps ~ date, activity, sum)
#with(actStepsPerDay,hist(steps,col="red",
#                    breaks=10,
#                    main="Steps per Day",
#                    xlab="Number of steps"))
# 3. Mean and median number of steps taken each day
medianSteps <- median(actStepsPerDay$steps,
                      na.rm=TRUE)
meanSteps <- mean(actStepsPerDay$steps,
                 na.rm=TRUE)
# 4. Time series plot of the average number of steps taken
actPlotAverageNum <- aggregate(steps ~ interval, 
                               activity, 
                               mean,
                               na.rm=TRUE)

library(ggplot2)

q4Plot = ggplot(actPlotAverageNum,
                aes(x=interval,
                    y=steps,
                    group=0)) +
                geom_line(col="red", 
                          size=2) +
                labs(title="Average Number of Steps",
                     x="Interval",
                     y="Avgerage Steps")
print(q4Plot)
# 5. The 5-minute interval that, on average, contains the maximum number of steps
# Viewing dataset Activity shows Interval increments by 5, ergo 5 minutes.

maxNumber <- round(max(actPlotAverageNum$steps),
                          digits=0)
intervalMax <- actPlotAverageNum[which.max(actPlotAverageNum$steps),]

# 6. Code to describe and show a strategy for imputing missing data
missingData = sum(is.na(activity))
print(summary(activity))
# 7. Histogram of the total number of steps taken each day after missing values are imputed
# 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
# 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report