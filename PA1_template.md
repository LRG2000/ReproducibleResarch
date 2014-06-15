# Reproducible Research Peer Assessment 1

## by LRG2000


```r
opts_chunk$set(echo = TRUE)
opts_chunk$set(message = FALSE)
```


First, read in the data:

```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
## noNA <- data[!is.na(data$steps),]
```


Now we can begin to answer questions about this dataset. The data consist of the number of steps taken summed over 5-minute intervals. The first "question"" asks for a histogram of the total number of steps taken each day, the median and mean number of steps taken each day. 


```r
s <- split(data, data$date)
dailyVal <- numeric(length(s))
for (i in 1:length(s)) {
    dailyVal[i] <- sum(s[[i]]$steps[!is.na(s[[i]]$steps)])
}
hbreaks <- c(0, 40, 3000, 6000, 9000, 12000, 15000, 18000, 22000)
plotbreaks <- c(0, 1480, 4500, 7500, 10500, 13500, 16500, 20000)
hist(dailyVal, breaks = hbreaks, freq = TRUE, xaxt = "n", xlab = "number of steps", 
    ylim = c(0, 20))
```

```
## Warning: the AREAS in the plot are wrong -- rather use 'freq = FALSE'
```

```r
axis(side = 1, at = plotbreaks, labels = as.character(plotbreaks))
abline(v = mean(dailyVal), col = "red", lwd = 2)
text(mean(dailyVal) - 1700, y = 20, labels = "mean")
abline(v = median(dailyVal), col = "blue", lwd = 2)
text(median(dailyVal) + 2200, y = 20, labels = "median")
```

![plot of chunk separate_days](figure/separate_days.png) 


We can also look at the average number of steps in each time interval. The time intervals listed below begin at the 24-hour time 
specified.


```r
interval_a <- as.character(10000 + s[[1]]$interval)
interval <- character(288)
intAve <- numeric(288)
int.df <- data.frame(s)
steps <- NULL
for (i in 1:288) {
    ## get the interval number to look like a time
    interval[i] <- paste(substr(interval_a[i], 2, 3), ":", substr(interval_a[i], 
        4, 5), sep = "", collapse = "")
}
for (i in 1:61) {
    steps <- rbind(steps, s[[i]]$steps)
}
means <- colMeans(steps, na.rm = TRUE)
plot(s[[1]]$interval, means, xaxt = "n", type = "l", xlab = "interval", ylab = "averave # of steps")
axis(side = 1, labels = interval[c(1, (1 + 18 * (1:12)))], at = s[[1]]$interval[c(1, 
    (1 + 22 * (1:12)))], lwd.ticks = 1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

In our calculation of mean number of steps in an interval, we have avoided missings values. As can be seen from the first histogram, there are eight days with steps measured the entire day. Now we are going to examine how many missing values there are. The interval with the largest average number of steps occurred at 08:35


```r
ind1 <- is.na(data$steps)
ind2 <- is.na(data$interval[!ind1])
nas <- sum(ind1) + sum(ind2)
```

After checking the numbers of NAs in the individual elements, we found that there are 2304 NAs in the "steps" element, and none in the other two list elements. We're going replace any NA values in an interval with the mean number of steps in that interval. Then we will calculate the total number of steps taken each day with these values.


```r
nsteps <- split(data$steps, data$interval)
for (i in 1:288) {
    ind <- is.na(nsteps[[i]])
    if (sum(ind > 0)) {
        nsteps[[i]][ind] <- means[i]
    }
}
newDailyVal <- numeric(61)
for (i in 1:61) {
    s[[i]]$steps[is.na(s[[i]]$steps)] <- means[is.na(s[[i]]$steps)]
    newDailyVal[i] <- sum(s[[i]]$steps)
}

hist(newDailyVal, breaks = hbreaks, freq = TRUE, xaxt = "n", xlab = "number of steps", 
    ylim = c(0, 28), main = "histogram without NAs")
```

```
## Warning: the AREAS in the plot are wrong -- rather use 'freq = FALSE'
```

```r
axis(side = 1, at = plotbreaks, labels = as.character(plotbreaks))
abline(v = mean(newDailyVal), col = "red", lwd = 2)
text(mean(newDailyVal) - 1700, y = 20, labels = "mean")
abline(v = median(newDailyVal), col = "blue", lwd = 2)
text(median(newDailyVal) + 2200, y = 20, labels = "median")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

We see that the mean and median are now in the same place, at 10766 . The mean before replacing NAs was 9354, while the old median was 10395.

Now we will look for differences in activity on weekends and weekdays.

```r
us <- unsplit(s, data$date)
date <- as.Date(us$date)
days <- weekdays(date)
wkends <- grepl("Sunday", days) | grepl("Saturday", days)
dayType <- character(length(us$date))
dayType[wkends] <- "weekend"
dayType[!wkends] <- "weekday"
us <- cbind(us, dayType)
wks <- split(us, dayType)
dayInts <- split(wks[["weekday"]], wks[["weekday"]]$interval)
endInts <- split(wks[["weekend"]], wks[["weekend"]]$interval)

plotme <- data.frame(aveSteps = numeric(576), dayType = character(576), interval = rep(s[[1]]$interval, 
    2), stringsAsFactors = FALSE)
for (i in 1:288) {
    plotme$aveSteps[i] <- mean(dayInts[[i]]$steps)
    plotme$aveSteps[288 + i] <- mean(endInts[[i]]$steps)
    plotme$dayType[i] <- "weekday"
    plotme$dayType[i + 288] <- "weekend"
}

library(ggplot2)
qplot(interval, aveSteps, data = plotme, geom = "line") + geom_line() + facet_grid(dayType ~ 
    .)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

We find that on weekends, the interval with the average number of steps occurs at 09:15, while on weekdays the interval occurred at 08:35, the same as the whole-week average.

