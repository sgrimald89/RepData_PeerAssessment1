#Reproducible Research Peer Assesment 1

##Loading and preprocessing the data

###Show any code that is needed to:

#####Load the data (i.e. read.csv())


```r
library(ggplot2)
if (!file.exists("activity.csv")) {
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", method="curl")
        unzip("activity.zip")
}
a=read.csv("activity.csv")
```

##What is mean total number of steps taken per day?

#####For this part of the assignment, you can ignore the missing values in the dataset.

#####Make a histogram of the total number of steps taken each day


```r
a$date=as.Date(a$date)
b=aggregate(a$steps,by=list(a$date),FUN=sum)
hist(b$x,breaks=20,labels=unique(b$x[order(b$x)]),main="Histogram of steps by day",xlab="Steps")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

#####Calculate and report the mean and median total number of steps taken per day

Mean:


```r
mean(b$x,na.rm=TRUE)
```

```
## [1] 10766
```
Median:


```r
median(b$x,na.rm=TRUE)
```

```
## [1] 10765
```

##What is the average daily activity pattern?

######Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
c=aggregate(a$steps,by=list(a$interval),FUN=mean,na.rm=TRUE)
plot(c$Group.1,c$x,type="l",ylab="Average Steps in Interval", xlab="Interval")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

#####Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

Maximum Steps:


```r
max(c$x)
```

```
## [1] 206.2
```

Interval with Maximum Steps:


```r
c$Group.1[which.max(c$x)]
```

```
## [1] 835
```

##Imputing missing values

#####Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#####Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(a))
```

```
## [1] 2304
```

#####Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

To impute my missing values, I use the mean for the given 5-minue interval.

#####Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
l=a
for (row in 1:length(l$steps)){
        if(is.na(l[row,1])){
                l[row,1]=c[c$Group.1==l[row,3],2]
        }
}
```

#####Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
m=aggregate(l$steps,by=list(l$date),FUN=sum)
hist(m$x,breaks=20,labels=unique(m$x[order(m$x)]),main="Histogram of steps by day",xlab="Steps")
```

![plot of chunk unnamed-chunk-10](./PA1_template_files/figure-html/unnamed-chunk-10.png) 

Mean:


```r
mean(m$x)
```

```
## [1] 10766
```

Median: 


```r
median(m$x)
```

```
## [1] 10766
```

As we can see, the mean did not change, but as we would expect, the median grew closer to the mean. In fact, in this case, the mean is equal to the median.

##Are there differences in activity patterns between weekdays and weekends?

#####For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#####Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
l$dayOfWeek=weekdays(l$date)
l$type=ifelse(l$dayOfWeek=="Saturday"|l$dayOfWeek=="Sunday","Weekend","Weekday")
l$type=as.factor(l$type)
l$interval=as.factor(l$interval)
```

#####Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
r=aggregate(l$steps,list(as.factor(l$interval),as.factor(l$type)),mean)
p <- ggplot(r, aes(x = as.integer(Group.1), y=x)) + geom_line()
p + facet_grid(Group.2~.)+xlab("Interval")+ylab("Average Steps in Interval")
```

![plot of chunk unnamed-chunk-14](./PA1_template_files/figure-html/unnamed-chunk-14.png) 
