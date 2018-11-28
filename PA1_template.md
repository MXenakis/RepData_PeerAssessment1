---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading the packages needed for performing the assignment tasks

```r
require(xtable)
require(ggplot2)
require(dplyr)
require(lattice)
```

## Loading and preprocessing the data


```r
unzip("activity.zip")
dataSet <- read.csv("activity.csv")
dataSet$date <- as.Date(dataSet$date)
```


## What is mean total number of steps taken per day?


### 1. Calculate the total number of steps taken per day


```r
totalSteps <- with(na.omit(dataSet),tapply(steps,as.factor(date),sum))
table <- as.data.frame.table(totalSteps)
xt <- xtable(table[1:10,])
names(xt) <- c("Date", "Total Steps")
align(xt) <- "lrr"
print(xt, type = "html")
```

<!-- html table generated in R 3.5.1 by xtable 1.8-3 package -->
<!-- Wed Nov 28 21:07:48 2018 -->
<table border=1>
<tr> <th>  </th> <th> Date </th> <th> Total Steps </th>  </tr>
  <tr> <td> 1 </td> <td align="right"> 2012-10-02 </td> <td align="right"> 126 </td> </tr>
  <tr> <td> 2 </td> <td align="right"> 2012-10-03 </td> <td align="right"> 11352 </td> </tr>
  <tr> <td> 3 </td> <td align="right"> 2012-10-04 </td> <td align="right"> 12116 </td> </tr>
  <tr> <td> 4 </td> <td align="right"> 2012-10-05 </td> <td align="right"> 13294 </td> </tr>
  <tr> <td> 5 </td> <td align="right"> 2012-10-06 </td> <td align="right"> 15420 </td> </tr>
  <tr> <td> 6 </td> <td align="right"> 2012-10-07 </td> <td align="right"> 11015 </td> </tr>
  <tr> <td> 7 </td> <td align="right"> 2012-10-09 </td> <td align="right"> 12811 </td> </tr>
  <tr> <td> 8 </td> <td align="right"> 2012-10-10 </td> <td align="right"> 9900 </td> </tr>
  <tr> <td> 9 </td> <td align="right"> 2012-10-11 </td> <td align="right"> 10304 </td> </tr>
  <tr> <td> 10 </td> <td align="right"> 2012-10-12 </td> <td align="right"> 17382 </td> </tr>
   </table>



### 2. Make a histogram of the total number of steps taken each day


```r
# The binwidth is calculated according to Freedman-Diaconis 
qplot(totalSteps, geom = "histogram", bins = diff(range(totalSteps)) / (2 * IQR(totalSteps) / length(totalSteps)^(1/3)), fill=I("orange"), col=I("black"), xlab = "Steps", ylab = "Frequency", main = "Histogram of total number of steps taken each day")
```

![plot of chunk histogram](figure/histogram-1.png)



### 3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanSteps <- format(round(mean(totalSteps),2), big.mark = ",")
medianSteps <- format(median(totalSteps), big.mark = ",")
```

The mean and the median of the total number of steps taken per day is as follows: 

|             Mean |             Median |
|------------------|--------------------|
|     10,766.19|     10,765|


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
meanIntervalSteps <- with(na.omit(dataSet),tapply(steps,interval,mean))

tablemIS <- as.data.frame.table(meanIntervalSteps)

colnames(tablemIS) <- c("interval","meanSteps")

tablemIS$interval <- as.integer(levels(tablemIS$interval))

ggplot(data = tablemIS, aes(x = interval , y = meanSteps)) + geom_line(color="orange", size=1)  + labs(title = "Average Daily Steps", x = "Interval", y = "Average Number of Steps per Day")
```

![plot of chunk meanIntervalSteps](figure/meanIntervalSteps-1.png)

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
intervalMaxSteps<-tablemIS[which.max(tablemIS$meanSteps),1]
print(intervalMaxSteps)
```

```
## [1] 835
```

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
NAsum <- sum(is.na(dataSet))
print(NAsum)
```

```
## [1] 2304
```

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  


```r
# Filling the NA values according to their corresponding 5-minute interval mean step value

newdataSet <- dataSet

for(i in 1:nrow(newdataSet)){
        if(is.na(newdataSet[i,1])){
                for(j in 1:nrow(tablemIS)){
                        if(newdataSet[i,3] == tablemIS[j,1]){
                                newdataSet[i,1] <- tablemIS[j,2]
                                break;
                        }
                }
        }
        
}
```
### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
# Write the new dataset in a file for future reference 
write.csv(newdataSet,"newTidyActivity.csv")
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
newtotalSteps <- with(newdataSet,tapply(steps,as.factor(date),sum))
table <- as.data.frame.table(newtotalSteps)

# The binwidth is calculated according to Freedman-Diaconis
qplot(newtotalSteps, geom = "histogram", bins = diff(range(newtotalSteps)) / (2 * IQR(newtotalSteps) / length(newtotalSteps)^(1/3)), fill=I("orange"), col=I("black"), xlab = "Steps", ylab = "Frequency", main = "Histogram of total number of steps taken each day")
```

![plot of chunk newHistogram](figure/newHistogram-1.png)


```r
NewmeanSteps <- format(round(mean(newtotalSteps),2), big.mark = ",")
NewmedianSteps <- format(median(newtotalSteps), big.mark = ",")
```

The mean and the median of the total number of steps taken per day is as follows: 

|         Data set |             Mean |             Median |
|------------------|------------------|--------------------|
|         Original |     10,766.19|     10,765|
|         Imputed  |  10,766.19|  10,766.19|


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.  

```r
weekdayORweekend <- sapply(newdataSet$date,function(d) ifelse(weekdays(d) == "Saturday" || weekdays(d) == "Sunday", "weekend", "weekday"))
newdataSet$day<-as.factor(weekdayORweekend)
```

### 2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
newMeandataSet <- newdataSet %>% group_by(day,interval) %>%     summarise(mSteps=mean(steps))
with(newMeandataSet,xyplot(mSteps ~ interval|day,type="l",xlab = "Interval", ylab="Number of steps",layout=c(1,2)))
```

![plot of chunk panelPlot](figure/panelPlot-1.png)
