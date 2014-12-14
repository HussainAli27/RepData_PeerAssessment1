---
title: "Steps Activity over a period of 2 months"
author: "Hussain"
date: "Sunday, December 14, 2014"
---


Reading the Activity Data

```r
data<-read.csv("activity.csv")
```

Number of steps taken every day (Plot)


```r
ad<-aggregate(steps~date ,data=data, FUN=sum)
hist(ad$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Mean, Median Steps taken every day

```r
meana<-mean(ad$steps)
median<-median(ad$steps)
data.frame(mean=meana, median)
```

```
##    mean median
## 1 10766  10765
```

Average daily steps taken at 5-minute intervals (plot)

```r
adt<-aggregate(steps~interval ,data=data, FUN=mean)
plot(adt$interval, adt$steps, type="l", xlab="time interval", ylab="average steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Maximum steps taken at which 5-minute time interval

```r
max<-adt$interval[which(adt$steps==max(adt$steps))]
max
```

```
## [1] 835
```

