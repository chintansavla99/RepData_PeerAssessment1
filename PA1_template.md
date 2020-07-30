---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

```r
knitr::opts_chunk$set(echo = TRUE,warning = FALSE)
library(qdapTools)
library(dplyr)
```

## Loading and preprocessing the data

```r
zipF<-"activity.zip"
outDir<-getwd()
unzip(zipF,exdir = outDir)
dat1<-read.csv("activity.csv")
```
Removing NA's from the dataset

```r
logi<-complete.cases(dat1)
dat<-dat1[logi,]

dat$date<-as.Date(dat$date,"%Y-%m-%d")
```

## What is mean total number of steps taken per day?

```r
totalpd<-tapply(dat$steps, dat$date, sum)
library(qdapTools)
totalpd<-list2df(as.list(totalpd),col1 = "TotalSteps",col2 = "Date")
totalpd$Date<-as.Date(totalpd$Date,"%Y-%m-%d")
library(ggplot2)
p<-ggplot(totalpd,aes(x=Date,y=TotalSteps))
p+geom_histogram(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
meanpd<-round(mean(totalpd$TotalSteps),digits = 2)

medianpd<-median(totalpd$TotalSteps)
```
Mean steps taken per day are 1.076619\times 10^{4} and Median is 10765  

## What is the average daily activity pattern?


```r
meanst<-tapply(dat$steps, dat$interval, mean)

meanst<-list2df(as.list(meanst),col1 = "Steps",col2 = "Interval")
plot(meanst$Interval,meanst$Steps,type = "l",xlab = "Intervals",ylab = "Average Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
maximum=select(filter(meanst, Steps==max(Steps)),Interval)
```
Average maximum number of Steps are in 835 Interval  

## Imputing missing values  
If we run the below we can conclude that NA's are present only in the steps column. 

```r
all(!is.na(dat1$date))
```

```
## [1] TRUE
```

```r
all(!is.na(dat1$interval))
```

```
## [1] TRUE
```

```r
all(!is.na(dat1$steps))
```

```
## [1] FALSE
```

```r
totalna<-table(is.na(dat1$steps))[[2]]
```
Total Number of NA's are 2304  

Now we replace the NA's with the mean value of the interval which we have already calculated earlier

```r
for (i in 1:17568) {
    if(is.na(dat1[i,1])){
        dat1[i,1]<-select(filter(meanst,Interval==as.character(dat1[i,3])),Steps)
    }
    
}
```
Plotting the histogram for new data

```r
totalnpd<-tapply(dat1$steps, dat1$date, sum)

totalnpd<-list2df(as.list(totalnpd),col1 = "TotalSteps",col2 = "Date")
totalnpd$Date<-as.Date(totalnpd$Date,"%Y-%m-%d")
library(ggplot2)
q<-ggplot(totalnpd,aes(x=Date,y=TotalSteps))
q+geom_histogram(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
meannnpd<-ceiling(mean(totalnpd$TotalSteps))

mediannpd<-ceiling(median(totalnpd$TotalSteps))
```
Mean is 1.0767\times 10^{4} and Median is 1.0767\times 10^{4}. As we have replaced the NA's with mean there isn't any difference in the Mean value.  

## Are there differences in activity patterns between weekdays and weekends?

```r
dat1$date<-as.Date(dat1$date,"%Y-%m-%d")

new<-weekdays(dat1$date)
typ<-0
for (i in 1:17568) {
    if(new[i]=='Saturday'|new[i]=='Sunday')
        typ[i]<-"weekend"
    else
        typ[i]<-"weekdays"
    
}
dat1$typ<-typ
dat1$typ<-as.factor(dat1$typ)

meanst<-tapply(dat1$steps, dat1$interval, mean)
meanst<-list2df(as.list(meanst),col1 = "Steps",col2 = "Interval")

for (i in 1:17568) {
    dat1[i,5]<-select(filter(meanst,Interval == as.character(dat1[i,3])),Steps)
    
}
dat1<-rename(dat1,Mean=Steps)
r<-ggplot(dat1,aes(interval,Mean))
r+geom_line()+facet_wrap(~typ,ncol=1)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



