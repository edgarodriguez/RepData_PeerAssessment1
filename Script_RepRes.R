# Loading packages
library(ggplot2)
library(ggthemes)

# Unzipping the file and reading it
path = getwd()
unzip("activity.zip", exdir = path)
activity <- read.csv("activity.csv")

summary(activity)
head(activity)

# Calculating total steps taken on a day
activityTotalSteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
# Changing col names
names(activityTotalSteps) <- c("Date", "Steps")

# Converting the data set into a data frame to be able to use ggplot2
totalStepsdf <- data.frame(activityTotalSteps)

# Plotting a histogram using ggplot2
g <- ggplot(totalStepsdf, aes(x = Steps)) + 
    geom_histogram(breaks = seq(0, 25000, by = 1000), fill = "#335252", col="#2D3033") + 
   
    xlab("Total Steps") + 
    ylab("Number of days") + 
    ggtitle("Total of Steps Taken by Day")+
    geom_vline(aes(xintercept=mean(Steps)),color="#AA4B41", linetype="dashed", size=1, show.legend=TRUE)+
    geom_vline(aes(xintercept=median(Steps)),color="#F9BA32", linetype="dashed", size=1)
               
print(g)

# Calculating the average number of steps taken, averaged across all days by 5-min intervals.
averageDailyActivity <- aggregate(activity$steps, by = list(activity$interval), 
                                  FUN = mean, na.rm = TRUE)


# Changing col names
names(averageDailyActivity) <- c("Interval", "Mean")

# Converting the data set into a dataframe
averageActivitydf <- data.frame(averageDailyActivity)

# Plotting on ggplot2
da <- ggplot(averageActivitydf, mapping = aes(Interval, Mean)) + 
    geom_line(col = "#335252") +
    xlab("Day period (Intervals of 5 minutes)") + 
    ylab("Average Number of Steps") + 
    ggtitle("Average Number of Steps Per Interval")
    
print(da)

sum(is.na(activity$steps))

# Matching the mean of daily activity with the missing values
imputedSteps <- averageDailyActivity$Mean[match(activity$interval, averageDailyActivity$Interval)]

# Transforming steps in activity if they were missing values with the filled values from above.
activityImputed <- transform(activity, 
                             steps = ifelse(is.na(activity$steps), yes = imputedSteps, no = activity$steps))

# Forming the new dataset with the imputed missing values.
totalActivityImputed <- aggregate(steps ~ date, activityImputed, sum)

# Changing col names
names(totalActivityImputed) <- c("date", "Steps")

# Plotting a histogram using ggplot2

totalImputedStepsdf <- data.frame(totalActivityImputed)
g <- ggplot(totalImputedStepsdf, aes(x = Steps)) + 
    geom_histogram(breaks = seq(0, 25000, by = 1000), fill = "#335252", col="#2D3033") + 
    
    xlab("Total Steps") + 
    ylab("Number of days") + 
    ggtitle("Total of Steps Taken by Day")+
    geom_vline(aes(xintercept=mean(Steps)),color="#AA4B41", linetype="dashed", size=1, show.legend=TRUE)+
    geom_vline(aes(xintercept=median(Steps)),color="#F9BA32", linetype="dashed", size=1)

print(g)

# Updating format of the dates
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

# Creating a function that distinguises weekdays from weekends
activity$dayType <- sapply(activity$date, function(x) {
    if(weekdays(x) == "Saturday" | weekdays(x) == "Sunday")
    {y <- "Weekend"}
    else {y <- "Weekday"}
    y
})

# Creating the data set that will be plotted
activityByDay <-  aggregate(steps ~ interval + dayType, activity, mean, na.rm = TRUE)

# Plotting using ggplot2
dayPlot <-  ggplot(activityByDay, aes(x = interval , y = steps, color = dayType)) + 
    geom_line() + ggtitle("Average Daily Steps by Day Type") + 
    xlab("Interval") + 
    ylab("Average Number of Steps") +
    facet_wrap(~dayType, ncol = 1, nrow=2) +
    scale_color_discrete(name = "Day Type")


print(dayPlot)
