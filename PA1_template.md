---
output: 
  html_document: 
    keep_md: yes
---
#Reproducible research Assignment 1
##1. code for reading data


```r
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
library(tidyr)
library(ggplot2)

file = read.csv("activity.csv")
```

##2. Histogram of total number of steps taken each day

```r
file1<- file  %>% filter(!is.na(steps)) 
sums <- file1 %>% group_by(date) %>% summarise(dailytotal = sum(steps))

#1 histogram 
ggp <- ggplot(data=sums , aes(x=date, y = dailytotal ))
ggp +  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  labs(y = "Steps", title = "Histogram of daily total steps") + 
  geom_histogram(stat="identity", show.legend = FALSE,  inherit.aes = TRUE, binwidth = 0.5, pad= 0, bins= 61)
```

```
## Warning: Duplicated aesthetics after name standardisation: pad
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

##3. Mean and median for daily total


```r
summary(sums$dailytotal)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

##4. Time series plot for average number of steps taken (averaged on intervals across all days)


```r
file2 <- file1 %>%  select(interval, steps) %>% group_by(interval) 
ave1 <- file2 %>% summarise(averageSteps= mean(steps))
plot( type = "l", ave1$interval, ave1$averageSteps, xaxp =c(0,2400, n= 100))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##5. Interval corresponding to highest average steps 


```r
ave1[ave1$averageSteps == max(ave1$averageSteps),]
```

```
## # A tibble: 1 x 2
##   interval averageSteps
##      <int>        <dbl>
## 1      835         206.
```

##6. Code to describe a strategy for imputing missing data and summary of daily total steps

```r
#impute missing values with averages of intervals which is more accurate than daily averages.
file3 <- left_join(file, ave1, by= c("interval" = "interval")) %>%
  mutate(aveStepsInt =as.integer(round(averageSteps)))
file31 <- file3 %>%  mutate(newSteps = coalesce(file3$steps,file3$aveStepsInt)) %>%
  select (date, interval,newSteps)

sums2 <- file31 %>% group_by(date) %>% summarise(dailytotal = sum(newSteps))

summary(sums2$dailytotal)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10762   10766   12811   21194
```

```r
file31[is.na(file31$newSteps),] # 0 rows . all NA s have been replaced with interval average values
```

```
## [1] date     interval newSteps
## <0 rows> (or 0-length row.names)
```

##7. Histogram of the total number of steps taken each day after missing values are imputed

```r
ggp1 <- ggplot(data=sums2 , aes(x=date, y = dailytotal ))
ggp1 +  theme(axis.text.x = element_text(angle = 90, hjust = 1))  + 
  labs(y = "Steps", title = "Histogram of daily total steps with imputed values") + 
  geom_histogram(stat="identity", show.legend = FALSE,  inherit.aes = TRUE,binwidth = 0.5, pad= 0, bins= 61)
```

```
## Warning: Duplicated aesthetics after name standardisation: pad
```

```
## Warning: Ignoring unknown parameters: binwidth, bins, pad
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends


```r
#3.1 create a new factor variable weekday/weekend

file4 <- file31 %>% mutate(day = weekdays(as.Date(date))) %>%
        mutate(dayofweek =  ifelse((day == "Sunday" | day == "Saturday"),"Weekend", "Weekday"))  %>% 
        group_by(interval, dayofweek)

ave2 <- file4 %>% summarise(averageSteps= mean(newSteps))

ggp2 <- ggplot(data= ave2)
ggp2 + geom_line(aes(x=ave2$interval, y=ave2$averageSteps)) + facet_grid(dayofweek ~ .) + ylab("No of steps")+ xlab(" Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
