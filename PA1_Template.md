# Reproducible Research: Peer Assessment 1


### Loading & preprocessing the data.
```{r loaddata}
unzip(zipfile = "repdata-data-activity.zip")
dataRead <- read.csv("activity.csv")
```


- View the loaded data frame.
```{r showdata}
names(dataRead)
str(dataRead)
head(dataRead, 10)
```

- Process/transform the data (if necessary) into a format suitable for analysis.
```{r processdata}
# Subset data frame to values without na for next process.
data_Without_NA <- dataRead[complete.cases(dataRead),]
```


### What is mean total number of steps taken per day?

- Plot a histogram of the total number of steps taken each day.
```{r}
# Find out the total steps taken per day.
totalSteps <- aggregate(steps ~ date, data_Without_NA, sum)

# Put the descriptive variable names in data frame.
names(totalSteps)[2] <- "sum_steps"

# View new created data frame.
head(totalSteps, 10)

# Plot histogram of the total steps taken per day.
hist(
        totalSteps$sum_steps,
        col = "orange",
        main = "Histogram of the Total Number of Steps Taken Each Day",
        xlab = "Total Number of Steps Taken Each Day",
        breaks = 30
)
```


![histogram of the Total Number of Steps Taken Each day] (figure/histogram of the Total Number of Steps Taken Each day.jpg)



- Calculate and report the mean and median of the total number of steps taken per day.
```{r}
mean(totalSteps$sum_steps)
median(totalSteps$sum_steps)
```


### What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
# the average number of steps taken, averaged across all days for each 5-minute
# Find the "interval" dataframe.
interval_dataFrame <- aggregate(steps ~ interval, data_Without_NA, mean)

# Put the descriptive variable names in data frame.
names(interval_dataFrame)[2] <- "mean_steps"

# View new created data frame.
head(interval_dataFrame, 10)

# Format plot margins (bottom, left, top, right) for long text labels.
par( mai = c(1.2, 1.5, 1,1) )

# Plot time series.
plot(
        x = interval_dataFrame$interval,
        y = interval_dataFrame$mean_steps,
        type = "l",
        main = "Time Series Plot of the 5-Minute Interval\n and the Average Number of Steps Taken, Averaged Across All Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken,\n Averaged Across All Days"
)
```


- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
# Find out the maximum steps.
interval_dataFrame[ interval_dataFrame$mean_steps == max(interval_dataFrame$mean_steps), ]
```

### Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r how_many_missing}
missing <- is.na(dataRead$steps)

# How many missing
table(missing)
```



- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
# Use the mean for the 5-minute interval to simulate NA values for a given internval.
```


- Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r}
# First, merge the original activity data frame with interval data frame.
NA_filled <- merge(dataRead, interval_dataFrame, by = 'interval', all.y = F)

# Then, merge NA values with averages rounding up for integers.
NA_filled$steps[is.na(NA_filled$steps)] <- as.integer(round(NA_filled$mean_steps[is.na(NA_filled$steps)]))

# Drop and reorder columns to match original activity data frame.
keeps <- names(dataRead)
NA_filled <- NA_filled[keeps]
head(NA_filled, 10)
```


- Make a histogram of the total number of steps taken each day
```{r}
# Find out the total number of steps taken per day with filled NA value.
newTotal <- aggregate(steps ~ date, NA_filled, sum)

# Put in the descriptive variable names in the newTotal data frame.
names(newTotal)[2] <- "sum_steps"

# Take a glance on this new data frame.
head(newTotal, 10)

# Plot the histogram based on new data frame.
hist(
        newTotal$sum_steps,
        col = "cyan",
        main = "Histogram of the Total Number of Steps Taken Each Day \nwith the missing data filled in",
        xlab = "Total Number of Steps Taken Each Day",
        breaks = 30
)

# Mean of this new filled data frame.
mean(newTotal$sum_steps)

# Median of this new filled data frame.
median(newTotal$sum_steps)
```

- Do these values differ from the estimates from the first part of the assignment?
```{r}
# It is a subtle difference in Mean calculation between two parts of the assignment; 
# Mean = 10766.19 (original data frame) and 10765.64 (data frame filled with NA),

# But quite apparently difference in Median calculation between these two parts of the assignment.
# Median = 10765 (original data frame) and 10762 (data frame filled with NA),
```


- What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
# The impact is depend on the imputing level of the missing data.
# As shown in the experimental value, there was practically no much difference 
# when using the average for a given interval as the averages is basically 
# pulled towards to the inserted average value.
```


### Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
# Create a new data frame.
newDataFrame <- NA_filled

# Prepare for up logical/test vector.
weekend <- weekdays( as.Date(newDataFrame$date)) %in% c("Saturday", "Sunday" )

# Fill in weekday column.
newDataFrame$daytype <- "weekday"

# Subsitute "weekday" with "weekend" where day == Sat/Sun.
newDataFrame$daytype[weekend == TRUE] <- "weekend"

# Convert new character column to factor.
newDataFrame$daytype <- as.factor(newDataFrame$daytype)

# Display the new data frame.
str(newDataFrame)

head(newDataFrame, 10)

# Verify the outcome.
weekdays( as.Date(newDataFrame$date[3]) )
```


### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
# the average number of steps taken, averaged across all days for each 5-minute interval.
new_Interval <- aggregate(steps ~ interval + daytype, newDataFrame, mean)

## Add descriptive variable names for easily understanding.
names(new_Interval)[3] <- "mean_steps"

# Display the new data frame.
head(new_Interval, 10)

# Plot time series based on the new data frame.
library(lattice)
xyplot(
        mean_steps ~ interval | daytype,
        new_Interval,
        type = "l",
        layout = c(1,2),
        main = "Time Series Plot of the 5-Minute Interval\nand the Average Number of Steps Taken,\nAveraged Across All Weekday Days or Weekend Days",
        xlab = "5-Minute Interval",
        ylab = "Average Number of Steps Taken"
)

```
