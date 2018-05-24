---
title: "PA1_template.Rmd"
output: 
  html_document: 
    keep_md: yes
---



#Project submission
####*loading data

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
###reading the df
steps <- read.csv("/Users/jomr/Desktop/Coursera/Reproducible Research/activity.csv")
```
####*checking the structure and transforming df

```r
str(steps)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(steps)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
#converting steps variable into date format
steps$date <- ymd(steps$date)
#converting into tbl_df
steps <- tbl_df(steps)
totalbyday <- dplyr::summarise(steps, totalsteps=sum(steps, na.rm=TRUE))
```
####*producing a histogram of the number of steps by day; then mean and then median; without missing values

```r
#grouping by date and summarising
steps <- group_by(steps, date)
totalbyday <- dplyr::summarise(steps, totalsteps=sum(steps, na.rm=TRUE))
histogram1 <- hist(totalbyday$totalsteps, main="Total steps by day", xlab="Number of steps")
```

![](PA1_template_files/figure-html/hist-1.png)<!-- -->

  
*mean and median

```r
mean1 <- mean(totalbyday$totalsteps)
median1 <- median(as.numeric(totalbyday$totalsteps))
rm(totalbyday)
print(paste("mean1=", mean1))  
```

[1] "mean1= 9354.22950819672"

```r
print(paste("median1=", median1))
```

[1] "median1= 10395"



```r
#grouping by interval
part2 <- group_by(steps, interval)
part2 <- dplyr::summarise(part2, intervalmean=mean(steps, na.rm=TRUE))
```
  

####*plotting mean number of steps by interval

```r
plot(part2$interval, part2$intervalmean, type="l", main = "Mean number of steps by 5 minute interval", ylab="Mean number of steps", xlab="interval (min)")
```

![](PA1_template_files/figure-html/plot1-1.png)<!-- -->
  
####*choosing the interval with the highest average  

```r
highest <- part2[which.max(part2$intervalmean),1]
print(highest[[1]])
```

```
## [1] 835
```
  
#####imputation

```r
#getting and summing number of NA's
imputeddf <- steps
#imputing missings with the mean for each interval
c <- as.numeric(which(is.na(imputeddf$steps)))
for (i in c) {
            testo <- subset(imputeddf, interval==as.numeric(imputeddf[i,3]), select=steps)
            imputeddf$steps[[i]] <- mean(testo$steps,na.rm=TRUE)
            rm(testo)
} 
rm(c,i)
```
####*producing histogram of steps by day in imputed df, then getting mean and median

```r
imputeddf <- group_by(imputeddf, date)
histogramdf2 <- dplyr::summarize(imputeddf, totalsteps=sum(steps))
histogram2 <- hist(histogramdf2$totalsteps, main="distribution of steps by day", xlab="total number of steps in a day")
```

![](PA1_template_files/figure-html/histogram2-1.png)<!-- -->

```r
mean2 <- mean(histogramdf2$totalsteps)
median2 <- median(as.numeric(histogramdf2$totalsteps))  
print(paste("mean2=",mean2))
```

[1] "mean2= 10766.1886792453"

```r
print(paste("median2=",median2))
```

[1] "median2= 10766.1886792453"


####*comparing means and medians before and after imputation

```r
library(knitr)
totalbyday <- dplyr::summarise(steps, totalsteps=sum(steps, na.rm=TRUE))
mean1 <- mean(totalbyday$totalsteps)
median1 <- median(as.numeric(totalbyday$totalsteps))
mean2 <- mean(histogramdf2$totalsteps)
median2 <- median(as.numeric(histogramdf2$totalsteps))
compare <- data.frame(c(mean1, mean2), c(median1, median2), row.names=c("original", "imputed"))
colnames(compare)=c("mean", "median")
knitr::kable(compare, caption="mean and median comparison")
```



Table: mean and median comparison

                mean     median
---------  ---------  ---------
original     9354.23   10395.00
imputed     10766.19   10766.19


####*building side by side histograms for comparison

```r
dev.set(which=2)
```

```
## quartz_off_screen 
##                 2
```

```r
par(mfrow=c(1,2))
plot(histogram1, xlab="count of total steps by day", main="Original df")
plot(histogram2, xlab="count of total steps by day", main="Imputed df")
```

![](PA1_template_files/figure-html/histcompare-1.png)<!-- -->

```r
par(mfrow=c(1,1)) #setting original graphing pars
```


```r
####adding weekday column to imputeddf
imputeddf <- mutate(imputeddf, day=weekdays(date))
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

```r
for (i in seq_along(1:length(imputeddf$day)))
if (imputeddf$day[[i]] == "Saturday"){imputeddf$weekend[[i]] <- "weekend"} else 
      if (imputeddf$day[[i]] =="Sunday"){imputeddf$weekend[[i]] <- "weekend"
} else {
            imputeddf$weekend[[i]] <- "weekday"
            }
```

```
## Warning: Unknown or uninitialised column: 'weekend'.
```


```r
###making summary data frame
weekdayweekend <- group_by(imputeddf, weekend, interval)
wdwkaveraged <- dplyr::summarise(weekdayweekend, averagesteps=mean(steps))
```
###last plot :)

```r
p <- ggplot(data=wdwkaveraged, aes(x=interval, y=averagesteps))
p+geom_line()+facet_grid(weekend~.)+theme_classic()+ylab("Average steps per interval")+xlab("interval (min)")
```

![](PA1_template_files/figure-html/lastplot-1.png)<!-- -->
