Assignment 1
========================================================
Loading and Processing the data
 

```r
activity<-read.csv("activity.csv")
activity$date<-as.Date(activity$date,format = "%Y-%m-%d")
```
 
Determine the mean number of steps taken per day

```r
sumsteps<-tapply(activity$steps,activity$date,sum)
hist(sumsteps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
mean(sumsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(sumsteps,na.rm=TRUE)
```

```
## [1] 10765
```
 
What is the average daily activity pattern?
 
Make time series plot

```r
intervalsteps<-aggregate(activity$steps,list(activity$interval),mean,na.rm=TRUE)
colnames(intervalsteps)<-c("interval","mean_steps")
plot(intervalsteps$interval,intervalsteps$mean_steps,type="l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
 
Which 5 minute interval, on average accross the days in the dataset, constains the maximum number of steps?

```r
intervalsteps$interval[intervalsteps$mean_steps==max(intervalsteps$mean_steps)]
```

```
## [1] 835
```
 
 Inputing Missing Values
 
 1.  Calculate and Report number of missing Values
 

```r
sum(is.na(activity$steps)) 
```

```
## [1] 2304
```

2.  Devise a plan to fill in NAs
Use the mean for the 5 minute interval rounded to the nearest step

3. Create the new data set using the strategy

```r
activity2<-merge(activity,intervalsteps,by="interval")
activity2<-activity2[order(activity2$date),]
activity2$newsteps<-activity2$steps
activity2$newsteps[is.na(activity2$newsteps)]<-round(activity2$mean_steps)
```

```
## Warning in activity2$newsteps[is.na(activity2$newsteps)] <-
## round(activity2$mean_steps): number of items to replace is not a multiple
## of replacement length
```

4.  Make histogram of new dataset, take mean and median

```r
sumsteps2<-tapply(activity2$newsteps,activity2$date,sum)
hist(sumsteps2)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
mean(sumsteps2)
```

```
## [1] 10765.64
```

```r
median(sumsteps2)
```

```
## [1] 10762
```

Result is that histogram has higher peak, lower mean and lower median

5. Are there different patterns on weekdays and weekends?

add new factor with weekday or weekend


```r
activity$day<-"weekday"
activity$day[weekdays(activity$date)=="Sunday"]<-"weekend"
activity$day[weekdays(activity$date)=="Saturday"]<-"weekend"
activity$day<-as.factor(activity$day)
```

make panel plot with on average steps per interval on weekdays and weekends

```r
library("lattice", lib.loc="~/R/win-library/3.1")
intervalstepsNew<-aggregate(activity$steps,list(Interval = activity$interval, Day =  activity$day),FUN=mean,na.rm=TRUE)
colnames(intervalstepsNew)<-c("Interval","Day","Mean_Steps")
xyplot(intervalstepsNew$Mean_Steps~intervalstepsNew$Interval|intervalstepsNew$Day,type="l",layout=c(1,2))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 
