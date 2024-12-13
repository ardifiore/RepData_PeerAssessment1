---
title: "Reproducible Research: Peer Assessment 1"
author: "Anthony DiFiore"
date: "2024-12-13"
output:
  html_document:
    keep_md: true
    toc: true              
    toc_depth: 2           
    toc_float: true        
    number_sections: true  
    theme: flatly
---
## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5-minute intervals throughout the day. The data consists of two months of data from an anonymous individual collected during the months of October and November 2012 and include the number of steps taken in 5 minute intervals each day.

**Data Source:**
The dataset was provided as part of the [Reproducible Research](https://www.coursera.org/learn/reproducible-research) course by Johns Hopkins University on Coursera. It can be downloaded from the following URL: [activity.zip](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip).

**Purpose:**
This analysis of the dataset aims to explore daily activity patterns, address missing data, and compare activity across weekdays and weekends.

Before we begin, I'm going to use the following code to set up the global environment for my markdown file. This code chunk will load all required libraries and will configure `knitr` options. These steps ensure that my document will be both clean and reproducible.

``` r
knitr::opts_chunk$set(
  echo = TRUE,        
  message = FALSE,     
  warning = FALSE
)

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

``` r
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```

``` r
library(hrbrthemes)
```

## Loading and Preprocessing the Data

This code imports and preprocesses the activity dataset. It reads the CSV file into `activity_data`, converts the `date` column to the `Date` class, and then uses `str()` and `summary()` to confirm the structure and basic statistics of the data. This step ensures the dataset is properly formatted and ready for further analysis: 

``` r
activity_data <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")

str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
summary(activity_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

The `str()` and `summary()` functions provide a structural overview and summary statistics of the dataset, helping us make initial observations. First, we notice a significant number of missing values (`NA`s), which we will address in a later step to ensure the integrity of our analysis. Second, the dataset charts daily steps across time intervals, offering insight into when people are most active during the day and week. These observations lay the foundation for exploring activity patterns and identifying peak intervals in the next steps.

## What is the Mean Total Number of Steps Taken Per Day?

This code aggregates total steps by date using `aggregate()` and removes missing values. The resulting `daily_steps` dataset contains one row per day, with a sum of all steps taken that day. Converting date to a factor supports categorical analysis and plotting by day.

``` r
daily_steps <- aggregate(steps ~ date, data = activity_data, FUN = sum, na.rm = TRUE)
daily_steps <- daily_steps %>% mutate(date = factor(date))
```

In this next section, we create a histogram to visualize the total number of steps taken each day. The data is grouped into bins of 2,500 steps, and each bar is colored based on the specific day. I want the chart to be clear and easy to read, and so I set the y-axis to show a maximum of 20 days.  This visualization provides a quick overview of the distribution of daily total steps: 

``` r
ggplot(daily_steps, aes(x = steps, fill = date)) +
  geom_histogram(breaks = seq(0, 25000, by = 2500), show.legend = FALSE) + 
  ylim(0, 20) + 
  labs(title = "Distribution of Daily Step Counts Over Two Months.", 
       x = "Total Steps Taken", y = "Number of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

In this part, we calculate the average (`mean`) and the middle value (`median`) of the total steps taken each day using the `daily_steps` dataset. These numbers help us understand the typical daily activity levels by summarizing the central trend of the data: 

``` r
mean_steps <- mean(daily_steps$steps)
median_steps <- median(daily_steps$steps)
mean_steps
```

```
## [1] 10766.19
```

``` r
median_steps
```

```
## [1] 10765
```

This finding indicates that the individual’s activity peaks in the morning, likely reflecting a consistent routine.

## What is the Average Daily Activity Pattern?

This code calculates the average steps per interval across all days and identifies the interval with the highest mean steps. These results help pinpoint when daily activity tends to peak.

``` r
interval_steps <- aggregate(steps ~ interval, data = activity_data, FUN = mean, na.rm = TRUE)
max_interval <- interval_steps[which.max(interval_steps$steps), "interval"]
```

These steps convert numeric intervals into a time-of-day format (HH:MM) by zero-padding the interval values, extracting hours and minutes, and constructing a `POSIXct` datetime. This enables the plotting of intervals as actual times. The maximum interval is similarly processed to produce a human-readable `time_label` for annotations in later visualizations.

``` r
interval_padded <- sprintf("%04d", interval_steps$interval)
hours <- as.integer(substr(interval_padded, 1, 2))
minutes <- as.integer(substr(interval_padded, 3, 4))

interval_steps$time_of_day <- as.POSIXct(
  sprintf("1970-01-01 %02d:%02d:00", hours, minutes),
  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"
)

max_interval_str <- sprintf("%04d", max_interval)
max_hour <- as.integer(substr(max_interval_str, 1, 2))
max_minute <- as.integer(substr(max_interval_str, 3, 4))
max_time <- as.POSIXct(sprintf("1970-01-01 %02d:%02d:00", max_hour, max_minute),
                       format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
time_label <- paste0(max_hour, ":", sprintf("%02d", max_minute))
```

Prior to plotting this data, I want to calculate the overall mean steps across all intervals, providing a baseline reference that can be uses in the next step for annotating the plot: 

``` r
mean_steps_value <- mean(interval_steps$steps)
```

Next, in order to visualize the data and to try and make sense of any average daily step patterns, I created a time-series graph that shows the average number of steps taken at different times of the day. The graph highlights the specific time interval with the highest average steps using both text and a red point marker. I chose to add a horizontal blue line to represent the overall average number of steps across all intervals. Additionally, I customized the appearance of the plot with a clean theme, used vibrant colors to enhance clarity and visual appeal, and set the x-axis to display time in 4-hour increments.

``` r
p <- interval_steps %>%
  ggplot(aes(x = time_of_day, y = steps)) +
  geom_line(color = "#dd00f9", size = 0.5) +  
  annotate("text", x = max_time, y = max(interval_steps$steps) + 5, 
           label = paste("Max interval:", time_label),
           color = "black") +
  annotate("point", x = max_time, y = max(interval_steps$steps),
           size = 3, shape = 21, fill = "transparent", color = "red") +
  geom_hline(yintercept = mean_steps_value, color = "#0099f9", size = 0.75) +
  labs(title = "Average Number of Steps per Time of Day",
       x = "Time of Day",
       y = "Average Number of Steps") +
  theme_ipsum(base_size = 14) +
  theme(
    plot.title = element_text(color = "#0099f9", size = 16, face = "bold"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M", expand = c(0,0))
```

Finally, in order to make the plot more dynamic and to allow for users to more deeply explore the data, I transformed the static plot `p` created with `ggplot2` into an interactive visualization using Plotly's `ggplotly()` function. This modification allows users to engage with the plot through features like tooltips, zooming, and panning.

``` r
fig <- ggplotly(p)
fig
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-5239714bf0b5934ceee2" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-5239714bf0b5934ceee2">{"x":{"data":[{"x":[0,300,600,900,1200,1500,1800,2100,2400,2700,3000,3300,3600,3900,4200,4500,4800,5100,5400,5700,6000,6300,6600,6900,7200,7500,7800,8100,8400,8700,9000,9300,9600,9900,10200,10500,10800,11100,11400,11700,12000,12300,12600,12900,13200,13500,13800,14100,14400,14700,15000,15300,15600,15900,16200,16500,16800,17100,17400,17700,18000,18300,18600,18900,19200,19500,19800,20100,20400,20700,21000,21300,21600,21900,22200,22500,22800,23100,23400,23700,24000,24300,24600,24900,25200,25500,25800,26100,26400,26700,27000,27300,27600,27900,28200,28500,28800,29100,29400,29700,30000,30300,30600,30900,31200,31500,31800,32100,32400,32700,33000,33300,33600,33900,34200,34500,34800,35100,35400,35700,36000,36300,36600,36900,37200,37500,37800,38100,38400,38700,39000,39300,39600,39900,40200,40500,40800,41100,41400,41700,42000,42300,42600,42900,43200,43500,43800,44100,44400,44700,45000,45300,45600,45900,46200,46500,46800,47100,47400,47700,48000,48300,48600,48900,49200,49500,49800,50100,50400,50700,51000,51300,51600,51900,52200,52500,52800,53100,53400,53700,54000,54300,54600,54900,55200,55500,55800,56100,56400,56700,57000,57300,57600,57900,58200,58500,58800,59100,59400,59700,60000,60300,60600,60900,61200,61500,61800,62100,62400,62700,63000,63300,63600,63900,64200,64500,64800,65100,65400,65700,66000,66300,66600,66900,67200,67500,67800,68100,68400,68700,69000,69300,69600,69900,70200,70500,70800,71100,71400,71700,72000,72300,72600,72900,73200,73500,73800,74100,74400,74700,75000,75300,75600,75900,76200,76500,76800,77100,77400,77700,78000,78300,78600,78900,79200,79500,79800,80100,80400,80700,81000,81300,81600,81900,82200,82500,82800,83100,83400,83700,84000,84300,84600,84900,85200,85500,85800,86100],"y":[1.7169811320754718,0.33962264150943394,0.13207547169811321,0.15094339622641509,0.075471698113207544,2.0943396226415096,0.52830188679245282,0.86792452830188682,0,1.4716981132075471,0.30188679245283018,0.13207547169811321,0.32075471698113206,0.67924528301886788,0.15094339622641509,0.33962264150943394,0,1.1132075471698113,1.8301886792452831,0.16981132075471697,0.16981132075471697,0.37735849056603776,0.26415094339622641,0,0,0,1.1320754716981132,0,0,0.13207547169811321,0,0.22641509433962265,0,0,1.5471698113207548,0.94339622641509435,0,0,0,0,0.20754716981132076,0.62264150943396224,1.6226415094339623,0.58490566037735847,0.49056603773584906,0.075471698113207544,0,0,1.1886792452830188,0.94339622641509435,2.5660377358490565,0,0.33962264150943394,0.35849056603773582,4.1132075471698117,0.660377358490566,3.4905660377358489,0.83018867924528306,3.1132075471698113,1.1132075471698113,0,1.5660377358490567,3,2.2452830188679247,3.3207547169811322,2.9622641509433962,2.0943396226415096,6.0566037735849054,16.018867924528301,18.339622641509433,39.452830188679243,44.490566037735846,31.490566037735849,49.264150943396224,53.773584905660378,63.452830188679243,49.962264150943398,47.075471698113205,52.150943396226417,39.339622641509436,44.018867924528301,44.169811320754718,37.358490566037737,49.037735849056602,43.811320754716981,44.377358490566039,50.509433962264154,54.509433962264154,49.924528301886795,50.981132075471699,55.679245283018865,44.320754716981135,52.264150943396224,69.547169811320757,57.849056603773583,56.150943396226417,73.377358490566039,68.20754716981132,129.43396226415095,157.52830188679246,171.15094339622641,155.39622641509433,177.30188679245282,206.16981132075472,195.9245283018868,179.56603773584905,183.39622641509433,167.01886792452831,143.45283018867926,124.0377358490566,109.11320754716981,108.11320754716981,103.71698113207547,95.962264150943398,66.20754716981132,45.226415094339622,24.79245283018868,38.754716981132077,34.981132075471699,21.056603773584907,40.566037735849058,26.981132075471699,42.415094339622641,52.660377358490564,38.924528301886795,50.79245283018868,44.283018867924525,37.415094339622641,34.698113207547166,28.339622641509433,25.09433962264151,31.943396226415093,31.358490566037737,29.679245283018869,21.320754716981131,25.547169811320753,28.377358490566039,26.471698113207548,33.433962264150942,49.981132075471699,42.037735849056602,44.60377358490566,46.037735849056602,59.188679245283019,63.867924528301884,87.698113207547166,94.84905660377359,92.773584905660371,63.39622641509434,50.169811320754718,54.471698113207545,32.415094339622641,26.528301886792452,37.735849056603776,45.056603773584904,67.283018867924525,42.339622641509436,39.886792452830186,43.264150943396224,40.981132075471699,46.245283018867923,56.433962264150942,42.754716981132077,25.132075471698112,39.962264150943398,53.547169811320757,47.320754716981135,60.811320754716981,55.754716981132077,51.962264150943398,43.584905660377359,48.698113207547166,35.471698113207545,37.547169811320757,41.849056603773583,27.509433962264151,17.113207547169811,26.075471698113208,43.622641509433961,43.773584905660378,30.018867924528301,36.075471698113205,35.490566037735846,38.849056603773583,45.962264150943398,47.754716981132077,48.132075471698116,65.320754716981128,82.905660377358487,98.660377358490564,102.11320754716981,83.962264150943398,62.132075471698116,64.132075471698116,74.547169811320757,63.169811320754718,56.905660377358494,59.773584905660378,43.867924528301884,38.566037735849058,44.660377358490564,45.452830188679243,46.20754716981132,43.679245283018865,46.622641509433961,56.301886792452834,50.716981132075475,61.226415094339622,72.716981132075475,78.943396226415089,68.943396226415089,59.660377358490564,75.094339622641513,56.509433962264154,34.773584905660378,37.452830188679243,40.679245283018865,58.018867924528301,74.698113207547166,85.320754716981128,59.264150943396224,67.773584905660371,77.698113207547166,74.245283018867923,85.339622641509436,99.452830188679243,86.584905660377359,85.603773584905667,84.867924528301884,77.830188679245282,58.037735849056602,53.358490566037737,36.320754716981135,20.716981132075471,27.39622641509434,40.018867924528301,30.20754716981132,25.547169811320753,45.660377358490564,33.528301886792455,19.622641509433961,19.018867924528301,19.339622641509433,33.339622641509436,26.811320754716981,21.169811320754718,27.30188679245283,21.339622641509433,19.547169811320753,21.320754716981131,32.301886792452834,20.150943396226417,15.943396226415095,17.226415094339622,23.452830188679247,19.245283018867923,12.452830188679245,8.0188679245283012,14.660377358490566,16.30188679245283,8.6792452830188687,7.7924528301886795,8.1320754716981138,2.6226415094339623,1.4528301886792452,3.6792452830188678,4.8113207547169807,8.5094339622641506,7.0754716981132075,8.6981132075471699,9.7547169811320753,2.2075471698113209,0.32075471698113206,0.11320754716981132,1.6037735849056605,4.6037735849056602,3.3018867924528301,2.8490566037735849,0,0.83018867924528306,0.96226415094339623,1.5849056603773586,2.6037735849056602,4.6981132075471699,3.3018867924528301,0.64150943396226412,0.22641509433962265,1.0754716981132075],"text":["time_of_day: 1970-01-01 00:00:00<br />steps:   1.7169811","time_of_day: 1970-01-01 00:05:00<br />steps:   0.3396226","time_of_day: 1970-01-01 00:10:00<br />steps:   0.1320755","time_of_day: 1970-01-01 00:15:00<br />steps:   0.1509434","time_of_day: 1970-01-01 00:20:00<br />steps:   0.0754717","time_of_day: 1970-01-01 00:25:00<br />steps:   2.0943396","time_of_day: 1970-01-01 00:30:00<br />steps:   0.5283019","time_of_day: 1970-01-01 00:35:00<br />steps:   0.8679245","time_of_day: 1970-01-01 00:40:00<br />steps:   0.0000000","time_of_day: 1970-01-01 00:45:00<br />steps:   1.4716981","time_of_day: 1970-01-01 00:50:00<br />steps:   0.3018868","time_of_day: 1970-01-01 00:55:00<br />steps:   0.1320755","time_of_day: 1970-01-01 01:00:00<br />steps:   0.3207547","time_of_day: 1970-01-01 01:05:00<br />steps:   0.6792453","time_of_day: 1970-01-01 01:10:00<br />steps:   0.1509434","time_of_day: 1970-01-01 01:15:00<br />steps:   0.3396226","time_of_day: 1970-01-01 01:20:00<br />steps:   0.0000000","time_of_day: 1970-01-01 01:25:00<br />steps:   1.1132075","time_of_day: 1970-01-01 01:30:00<br />steps:   1.8301887","time_of_day: 1970-01-01 01:35:00<br />steps:   0.1698113","time_of_day: 1970-01-01 01:40:00<br />steps:   0.1698113","time_of_day: 1970-01-01 01:45:00<br />steps:   0.3773585","time_of_day: 1970-01-01 01:50:00<br />steps:   0.2641509","time_of_day: 1970-01-01 01:55:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:00:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:05:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:10:00<br />steps:   1.1320755","time_of_day: 1970-01-01 02:15:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:20:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:25:00<br />steps:   0.1320755","time_of_day: 1970-01-01 02:30:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:35:00<br />steps:   0.2264151","time_of_day: 1970-01-01 02:40:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:45:00<br />steps:   0.0000000","time_of_day: 1970-01-01 02:50:00<br />steps:   1.5471698","time_of_day: 1970-01-01 02:55:00<br />steps:   0.9433962","time_of_day: 1970-01-01 03:00:00<br />steps:   0.0000000","time_of_day: 1970-01-01 03:05:00<br />steps:   0.0000000","time_of_day: 1970-01-01 03:10:00<br />steps:   0.0000000","time_of_day: 1970-01-01 03:15:00<br />steps:   0.0000000","time_of_day: 1970-01-01 03:20:00<br />steps:   0.2075472","time_of_day: 1970-01-01 03:25:00<br />steps:   0.6226415","time_of_day: 1970-01-01 03:30:00<br />steps:   1.6226415","time_of_day: 1970-01-01 03:35:00<br />steps:   0.5849057","time_of_day: 1970-01-01 03:40:00<br />steps:   0.4905660","time_of_day: 1970-01-01 03:45:00<br />steps:   0.0754717","time_of_day: 1970-01-01 03:50:00<br />steps:   0.0000000","time_of_day: 1970-01-01 03:55:00<br />steps:   0.0000000","time_of_day: 1970-01-01 04:00:00<br />steps:   1.1886792","time_of_day: 1970-01-01 04:05:00<br />steps:   0.9433962","time_of_day: 1970-01-01 04:10:00<br />steps:   2.5660377","time_of_day: 1970-01-01 04:15:00<br />steps:   0.0000000","time_of_day: 1970-01-01 04:20:00<br />steps:   0.3396226","time_of_day: 1970-01-01 04:25:00<br />steps:   0.3584906","time_of_day: 1970-01-01 04:30:00<br />steps:   4.1132075","time_of_day: 1970-01-01 04:35:00<br />steps:   0.6603774","time_of_day: 1970-01-01 04:40:00<br />steps:   3.4905660","time_of_day: 1970-01-01 04:45:00<br />steps:   0.8301887","time_of_day: 1970-01-01 04:50:00<br />steps:   3.1132075","time_of_day: 1970-01-01 04:55:00<br />steps:   1.1132075","time_of_day: 1970-01-01 05:00:00<br />steps:   0.0000000","time_of_day: 1970-01-01 05:05:00<br />steps:   1.5660377","time_of_day: 1970-01-01 05:10:00<br />steps:   3.0000000","time_of_day: 1970-01-01 05:15:00<br />steps:   2.2452830","time_of_day: 1970-01-01 05:20:00<br />steps:   3.3207547","time_of_day: 1970-01-01 05:25:00<br />steps:   2.9622642","time_of_day: 1970-01-01 05:30:00<br />steps:   2.0943396","time_of_day: 1970-01-01 05:35:00<br />steps:   6.0566038","time_of_day: 1970-01-01 05:40:00<br />steps:  16.0188679","time_of_day: 1970-01-01 05:45:00<br />steps:  18.3396226","time_of_day: 1970-01-01 05:50:00<br />steps:  39.4528302","time_of_day: 1970-01-01 05:55:00<br />steps:  44.4905660","time_of_day: 1970-01-01 06:00:00<br />steps:  31.4905660","time_of_day: 1970-01-01 06:05:00<br />steps:  49.2641509","time_of_day: 1970-01-01 06:10:00<br />steps:  53.7735849","time_of_day: 1970-01-01 06:15:00<br />steps:  63.4528302","time_of_day: 1970-01-01 06:20:00<br />steps:  49.9622642","time_of_day: 1970-01-01 06:25:00<br />steps:  47.0754717","time_of_day: 1970-01-01 06:30:00<br />steps:  52.1509434","time_of_day: 1970-01-01 06:35:00<br />steps:  39.3396226","time_of_day: 1970-01-01 06:40:00<br />steps:  44.0188679","time_of_day: 1970-01-01 06:45:00<br />steps:  44.1698113","time_of_day: 1970-01-01 06:50:00<br />steps:  37.3584906","time_of_day: 1970-01-01 06:55:00<br />steps:  49.0377358","time_of_day: 1970-01-01 07:00:00<br />steps:  43.8113208","time_of_day: 1970-01-01 07:05:00<br />steps:  44.3773585","time_of_day: 1970-01-01 07:10:00<br />steps:  50.5094340","time_of_day: 1970-01-01 07:15:00<br />steps:  54.5094340","time_of_day: 1970-01-01 07:20:00<br />steps:  49.9245283","time_of_day: 1970-01-01 07:25:00<br />steps:  50.9811321","time_of_day: 1970-01-01 07:30:00<br />steps:  55.6792453","time_of_day: 1970-01-01 07:35:00<br />steps:  44.3207547","time_of_day: 1970-01-01 07:40:00<br />steps:  52.2641509","time_of_day: 1970-01-01 07:45:00<br />steps:  69.5471698","time_of_day: 1970-01-01 07:50:00<br />steps:  57.8490566","time_of_day: 1970-01-01 07:55:00<br />steps:  56.1509434","time_of_day: 1970-01-01 08:00:00<br />steps:  73.3773585","time_of_day: 1970-01-01 08:05:00<br />steps:  68.2075472","time_of_day: 1970-01-01 08:10:00<br />steps: 129.4339623","time_of_day: 1970-01-01 08:15:00<br />steps: 157.5283019","time_of_day: 1970-01-01 08:20:00<br />steps: 171.1509434","time_of_day: 1970-01-01 08:25:00<br />steps: 155.3962264","time_of_day: 1970-01-01 08:30:00<br />steps: 177.3018868","time_of_day: 1970-01-01 08:35:00<br />steps: 206.1698113","time_of_day: 1970-01-01 08:40:00<br />steps: 195.9245283","time_of_day: 1970-01-01 08:45:00<br />steps: 179.5660377","time_of_day: 1970-01-01 08:50:00<br />steps: 183.3962264","time_of_day: 1970-01-01 08:55:00<br />steps: 167.0188679","time_of_day: 1970-01-01 09:00:00<br />steps: 143.4528302","time_of_day: 1970-01-01 09:05:00<br />steps: 124.0377358","time_of_day: 1970-01-01 09:10:00<br />steps: 109.1132075","time_of_day: 1970-01-01 09:15:00<br />steps: 108.1132075","time_of_day: 1970-01-01 09:20:00<br />steps: 103.7169811","time_of_day: 1970-01-01 09:25:00<br />steps:  95.9622642","time_of_day: 1970-01-01 09:30:00<br />steps:  66.2075472","time_of_day: 1970-01-01 09:35:00<br />steps:  45.2264151","time_of_day: 1970-01-01 09:40:00<br />steps:  24.7924528","time_of_day: 1970-01-01 09:45:00<br />steps:  38.7547170","time_of_day: 1970-01-01 09:50:00<br />steps:  34.9811321","time_of_day: 1970-01-01 09:55:00<br />steps:  21.0566038","time_of_day: 1970-01-01 10:00:00<br />steps:  40.5660377","time_of_day: 1970-01-01 10:05:00<br />steps:  26.9811321","time_of_day: 1970-01-01 10:10:00<br />steps:  42.4150943","time_of_day: 1970-01-01 10:15:00<br />steps:  52.6603774","time_of_day: 1970-01-01 10:20:00<br />steps:  38.9245283","time_of_day: 1970-01-01 10:25:00<br />steps:  50.7924528","time_of_day: 1970-01-01 10:30:00<br />steps:  44.2830189","time_of_day: 1970-01-01 10:35:00<br />steps:  37.4150943","time_of_day: 1970-01-01 10:40:00<br />steps:  34.6981132","time_of_day: 1970-01-01 10:45:00<br />steps:  28.3396226","time_of_day: 1970-01-01 10:50:00<br />steps:  25.0943396","time_of_day: 1970-01-01 10:55:00<br />steps:  31.9433962","time_of_day: 1970-01-01 11:00:00<br />steps:  31.3584906","time_of_day: 1970-01-01 11:05:00<br />steps:  29.6792453","time_of_day: 1970-01-01 11:10:00<br />steps:  21.3207547","time_of_day: 1970-01-01 11:15:00<br />steps:  25.5471698","time_of_day: 1970-01-01 11:20:00<br />steps:  28.3773585","time_of_day: 1970-01-01 11:25:00<br />steps:  26.4716981","time_of_day: 1970-01-01 11:30:00<br />steps:  33.4339623","time_of_day: 1970-01-01 11:35:00<br />steps:  49.9811321","time_of_day: 1970-01-01 11:40:00<br />steps:  42.0377358","time_of_day: 1970-01-01 11:45:00<br />steps:  44.6037736","time_of_day: 1970-01-01 11:50:00<br />steps:  46.0377358","time_of_day: 1970-01-01 11:55:00<br />steps:  59.1886792","time_of_day: 1970-01-01 12:00:00<br />steps:  63.8679245","time_of_day: 1970-01-01 12:05:00<br />steps:  87.6981132","time_of_day: 1970-01-01 12:10:00<br />steps:  94.8490566","time_of_day: 1970-01-01 12:15:00<br />steps:  92.7735849","time_of_day: 1970-01-01 12:20:00<br />steps:  63.3962264","time_of_day: 1970-01-01 12:25:00<br />steps:  50.1698113","time_of_day: 1970-01-01 12:30:00<br />steps:  54.4716981","time_of_day: 1970-01-01 12:35:00<br />steps:  32.4150943","time_of_day: 1970-01-01 12:40:00<br />steps:  26.5283019","time_of_day: 1970-01-01 12:45:00<br />steps:  37.7358491","time_of_day: 1970-01-01 12:50:00<br />steps:  45.0566038","time_of_day: 1970-01-01 12:55:00<br />steps:  67.2830189","time_of_day: 1970-01-01 13:00:00<br />steps:  42.3396226","time_of_day: 1970-01-01 13:05:00<br />steps:  39.8867925","time_of_day: 1970-01-01 13:10:00<br />steps:  43.2641509","time_of_day: 1970-01-01 13:15:00<br />steps:  40.9811321","time_of_day: 1970-01-01 13:20:00<br />steps:  46.2452830","time_of_day: 1970-01-01 13:25:00<br />steps:  56.4339623","time_of_day: 1970-01-01 13:30:00<br />steps:  42.7547170","time_of_day: 1970-01-01 13:35:00<br />steps:  25.1320755","time_of_day: 1970-01-01 13:40:00<br />steps:  39.9622642","time_of_day: 1970-01-01 13:45:00<br />steps:  53.5471698","time_of_day: 1970-01-01 13:50:00<br />steps:  47.3207547","time_of_day: 1970-01-01 13:55:00<br />steps:  60.8113208","time_of_day: 1970-01-01 14:00:00<br />steps:  55.7547170","time_of_day: 1970-01-01 14:05:00<br />steps:  51.9622642","time_of_day: 1970-01-01 14:10:00<br />steps:  43.5849057","time_of_day: 1970-01-01 14:15:00<br />steps:  48.6981132","time_of_day: 1970-01-01 14:20:00<br />steps:  35.4716981","time_of_day: 1970-01-01 14:25:00<br />steps:  37.5471698","time_of_day: 1970-01-01 14:30:00<br />steps:  41.8490566","time_of_day: 1970-01-01 14:35:00<br />steps:  27.5094340","time_of_day: 1970-01-01 14:40:00<br />steps:  17.1132075","time_of_day: 1970-01-01 14:45:00<br />steps:  26.0754717","time_of_day: 1970-01-01 14:50:00<br />steps:  43.6226415","time_of_day: 1970-01-01 14:55:00<br />steps:  43.7735849","time_of_day: 1970-01-01 15:00:00<br />steps:  30.0188679","time_of_day: 1970-01-01 15:05:00<br />steps:  36.0754717","time_of_day: 1970-01-01 15:10:00<br />steps:  35.4905660","time_of_day: 1970-01-01 15:15:00<br />steps:  38.8490566","time_of_day: 1970-01-01 15:20:00<br />steps:  45.9622642","time_of_day: 1970-01-01 15:25:00<br />steps:  47.7547170","time_of_day: 1970-01-01 15:30:00<br />steps:  48.1320755","time_of_day: 1970-01-01 15:35:00<br />steps:  65.3207547","time_of_day: 1970-01-01 15:40:00<br />steps:  82.9056604","time_of_day: 1970-01-01 15:45:00<br />steps:  98.6603774","time_of_day: 1970-01-01 15:50:00<br />steps: 102.1132075","time_of_day: 1970-01-01 15:55:00<br />steps:  83.9622642","time_of_day: 1970-01-01 16:00:00<br />steps:  62.1320755","time_of_day: 1970-01-01 16:05:00<br />steps:  64.1320755","time_of_day: 1970-01-01 16:10:00<br />steps:  74.5471698","time_of_day: 1970-01-01 16:15:00<br />steps:  63.1698113","time_of_day: 1970-01-01 16:20:00<br />steps:  56.9056604","time_of_day: 1970-01-01 16:25:00<br />steps:  59.7735849","time_of_day: 1970-01-01 16:30:00<br />steps:  43.8679245","time_of_day: 1970-01-01 16:35:00<br />steps:  38.5660377","time_of_day: 1970-01-01 16:40:00<br />steps:  44.6603774","time_of_day: 1970-01-01 16:45:00<br />steps:  45.4528302","time_of_day: 1970-01-01 16:50:00<br />steps:  46.2075472","time_of_day: 1970-01-01 16:55:00<br />steps:  43.6792453","time_of_day: 1970-01-01 17:00:00<br />steps:  46.6226415","time_of_day: 1970-01-01 17:05:00<br />steps:  56.3018868","time_of_day: 1970-01-01 17:10:00<br />steps:  50.7169811","time_of_day: 1970-01-01 17:15:00<br />steps:  61.2264151","time_of_day: 1970-01-01 17:20:00<br />steps:  72.7169811","time_of_day: 1970-01-01 17:25:00<br />steps:  78.9433962","time_of_day: 1970-01-01 17:30:00<br />steps:  68.9433962","time_of_day: 1970-01-01 17:35:00<br />steps:  59.6603774","time_of_day: 1970-01-01 17:40:00<br />steps:  75.0943396","time_of_day: 1970-01-01 17:45:00<br />steps:  56.5094340","time_of_day: 1970-01-01 17:50:00<br />steps:  34.7735849","time_of_day: 1970-01-01 17:55:00<br />steps:  37.4528302","time_of_day: 1970-01-01 18:00:00<br />steps:  40.6792453","time_of_day: 1970-01-01 18:05:00<br />steps:  58.0188679","time_of_day: 1970-01-01 18:10:00<br />steps:  74.6981132","time_of_day: 1970-01-01 18:15:00<br />steps:  85.3207547","time_of_day: 1970-01-01 18:20:00<br />steps:  59.2641509","time_of_day: 1970-01-01 18:25:00<br />steps:  67.7735849","time_of_day: 1970-01-01 18:30:00<br />steps:  77.6981132","time_of_day: 1970-01-01 18:35:00<br />steps:  74.2452830","time_of_day: 1970-01-01 18:40:00<br />steps:  85.3396226","time_of_day: 1970-01-01 18:45:00<br />steps:  99.4528302","time_of_day: 1970-01-01 18:50:00<br />steps:  86.5849057","time_of_day: 1970-01-01 18:55:00<br />steps:  85.6037736","time_of_day: 1970-01-01 19:00:00<br />steps:  84.8679245","time_of_day: 1970-01-01 19:05:00<br />steps:  77.8301887","time_of_day: 1970-01-01 19:10:00<br />steps:  58.0377358","time_of_day: 1970-01-01 19:15:00<br />steps:  53.3584906","time_of_day: 1970-01-01 19:20:00<br />steps:  36.3207547","time_of_day: 1970-01-01 19:25:00<br />steps:  20.7169811","time_of_day: 1970-01-01 19:30:00<br />steps:  27.3962264","time_of_day: 1970-01-01 19:35:00<br />steps:  40.0188679","time_of_day: 1970-01-01 19:40:00<br />steps:  30.2075472","time_of_day: 1970-01-01 19:45:00<br />steps:  25.5471698","time_of_day: 1970-01-01 19:50:00<br />steps:  45.6603774","time_of_day: 1970-01-01 19:55:00<br />steps:  33.5283019","time_of_day: 1970-01-01 20:00:00<br />steps:  19.6226415","time_of_day: 1970-01-01 20:05:00<br />steps:  19.0188679","time_of_day: 1970-01-01 20:10:00<br />steps:  19.3396226","time_of_day: 1970-01-01 20:15:00<br />steps:  33.3396226","time_of_day: 1970-01-01 20:20:00<br />steps:  26.8113208","time_of_day: 1970-01-01 20:25:00<br />steps:  21.1698113","time_of_day: 1970-01-01 20:30:00<br />steps:  27.3018868","time_of_day: 1970-01-01 20:35:00<br />steps:  21.3396226","time_of_day: 1970-01-01 20:40:00<br />steps:  19.5471698","time_of_day: 1970-01-01 20:45:00<br />steps:  21.3207547","time_of_day: 1970-01-01 20:50:00<br />steps:  32.3018868","time_of_day: 1970-01-01 20:55:00<br />steps:  20.1509434","time_of_day: 1970-01-01 21:00:00<br />steps:  15.9433962","time_of_day: 1970-01-01 21:05:00<br />steps:  17.2264151","time_of_day: 1970-01-01 21:10:00<br />steps:  23.4528302","time_of_day: 1970-01-01 21:15:00<br />steps:  19.2452830","time_of_day: 1970-01-01 21:20:00<br />steps:  12.4528302","time_of_day: 1970-01-01 21:25:00<br />steps:   8.0188679","time_of_day: 1970-01-01 21:30:00<br />steps:  14.6603774","time_of_day: 1970-01-01 21:35:00<br />steps:  16.3018868","time_of_day: 1970-01-01 21:40:00<br />steps:   8.6792453","time_of_day: 1970-01-01 21:45:00<br />steps:   7.7924528","time_of_day: 1970-01-01 21:50:00<br />steps:   8.1320755","time_of_day: 1970-01-01 21:55:00<br />steps:   2.6226415","time_of_day: 1970-01-01 22:00:00<br />steps:   1.4528302","time_of_day: 1970-01-01 22:05:00<br />steps:   3.6792453","time_of_day: 1970-01-01 22:10:00<br />steps:   4.8113208","time_of_day: 1970-01-01 22:15:00<br />steps:   8.5094340","time_of_day: 1970-01-01 22:20:00<br />steps:   7.0754717","time_of_day: 1970-01-01 22:25:00<br />steps:   8.6981132","time_of_day: 1970-01-01 22:30:00<br />steps:   9.7547170","time_of_day: 1970-01-01 22:35:00<br />steps:   2.2075472","time_of_day: 1970-01-01 22:40:00<br />steps:   0.3207547","time_of_day: 1970-01-01 22:45:00<br />steps:   0.1132075","time_of_day: 1970-01-01 22:50:00<br />steps:   1.6037736","time_of_day: 1970-01-01 22:55:00<br />steps:   4.6037736","time_of_day: 1970-01-01 23:00:00<br />steps:   3.3018868","time_of_day: 1970-01-01 23:05:00<br />steps:   2.8490566","time_of_day: 1970-01-01 23:10:00<br />steps:   0.0000000","time_of_day: 1970-01-01 23:15:00<br />steps:   0.8301887","time_of_day: 1970-01-01 23:20:00<br />steps:   0.9622642","time_of_day: 1970-01-01 23:25:00<br />steps:   1.5849057","time_of_day: 1970-01-01 23:30:00<br />steps:   2.6037736","time_of_day: 1970-01-01 23:35:00<br />steps:   4.6981132","time_of_day: 1970-01-01 23:40:00<br />steps:   3.3018868","time_of_day: 1970-01-01 23:45:00<br />steps:   0.6415094","time_of_day: 1970-01-01 23:50:00<br />steps:   0.2264151","time_of_day: 1970-01-01 23:55:00<br />steps:   1.0754717"],"type":"scatter","mode":"lines","line":{"width":1.8897637795275593,"color":"rgba(221,0,249,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[30900],"y":[211.16981132075472],"text":"Max interval: 8:35","hovertext":"x: 1970-01-01 08:35:00<br />y: 211.1698","textfont":{"size":14.66456692913386,"color":"rgba(0,0,0,1)"},"type":"scatter","mode":"text","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[30900],"y":[206.16981132075472],"text":"x: 1970-01-01 08:35:00<br />y: 206.1698","type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"transparent","opacity":1,"size":11.338582677165356,"symbol":"circle","line":{"width":1.8897637795275593,"color":"rgba(255,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[0,86100],"y":[37.382599580712785,37.382599580712785],"text":"yintercept: 37.3826","type":"scatter","mode":"lines","line":{"width":2.8346456692913389,"color":"rgba(0,153,249,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":93.044416770444172,"r":39.850560398505614,"b":90.992112909921147,"l":84.350352843503558},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":18.596928185969279},"title":{"text":"<b> Average Number of Steps per Time of Day <\/b>","font":{"color":"rgba(0,153,249,1)","family":"Arial Narrow","size":21.253632212536321},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[0,86100],"tickmode":"array","ticktext":["00:00","04:00","08:00","12:00","16:00","20:00"],"tickvals":[0,14400.000000000002,28800.000000000004,43200,57600.000000000007,72000],"categoryorder":"array","categoryarray":["00:00","04:00","08:00","12:00","16:00","20:00"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923198,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":18.596928185969279},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.26567040265670405,"zeroline":false,"anchor":"y","title":{"text":"Time of Day","font":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":11.955168119551681}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[-10.558490566037737,221.72830188679245],"tickmode":"array","ticktext":["0","50","100","150","200"],"tickvals":[0,50,100,150,200],"categoryorder":"array","categoryarray":["0","50","100","150","200"],"nticks":null,"ticks":"","tickcolor":null,"ticklen":4.6492320464923216,"tickwidth":0,"showticklabels":true,"tickfont":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":18.596928185969286},"tickangle":-0,"showline":false,"linecolor":null,"linewidth":0,"showgrid":true,"gridcolor":"rgba(204,204,204,1)","gridwidth":0.26567040265670405,"zeroline":false,"anchor":"x","title":{"text":"Average Number of Steps","font":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":11.955168119551681}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"Arial Narrow","size":14.877542548775427}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"source":"A","attrs":{"7e872402a19c":{"x":{},"y":{},"type":"scatter"},"7e8728c01b3c":{"x":{},"y":{}},"7e875c3c150a":{"x":{},"y":{}},"7e876c8972d4":{"yintercept":{}}},"cur_data":"7e872402a19c","visdat":{"7e872402a19c":["function (y) ","x"],"7e8728c01b3c":["function (y) ","x"],"7e875c3c150a":["function (y) ","x"],"7e876c8972d4":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Imputing Missing Values

In the original dataset, there are a number of days/intervals where there are missing values (coded as `NA`). My goal here is to calculate and report the total number of missing values in the dataset (i.e. the total number of rows with `NA`s), and then to devise a strategy for filling in all of the missing values. I begin by using the following code to first check each entry in the steps column for `NA`, and then to create a lookup vector `interval_mean_map` that associates each interval with its average number of steps. This new vector that I've created simplifies the process of filling in missing steps values (`NA`) by providing a straightforward way to replace them with the average steps for each specific interval.

``` r
total_na <- sum(is.na(activity_data$steps))
total_na
```

```
## [1] 2304
```

``` r
interval_mean_map <- setNames(interval_steps$steps, interval_steps$interval)
```

The next step is to create a copy of `activity_data` named `imputed_data`, which identifies which `steps` entries are `NA`, and assigns each missing value the average steps from `interval_mean_map` based on the interval. This imputation ensures that the dataset is complete, allowing for accurate subsequent analyses without the bias that could result from missing data.

``` r
imputed_data <- activity_data
missing_indices <- is.na(imputed_data$steps)
imputed_data$steps[missing_indices] <- interval_mean_map[as.character(imputed_data$interval[missing_indices])]
```

My next step was to calculate the total number of steps taken each day by summing up all the steps across every interval for each date, and then to create a histogram in order to visualize how these daily steps are distributed. After experimenting with different plot dimensions, I decided to use a bin width of 1,000 steps, and limited the y-axis to 20, so that the plot would more clearly and effectively show the number of days that fall into each step range.

``` r
daily_steps_imputed <- aggregate(steps ~ date, data = imputed_data, FUN = sum)

daily_steps_imputed <- daily_steps_imputed |>
  mutate(date = factor(date))

ggplot(daily_steps_imputed, aes(x = steps, fill = date)) +
  geom_histogram(breaks = seq(0,25000, by = 1000), show.legend = FALSE) + 
  ylim(0, 20) + 
  labs(title = "Total Steps per Day (Imputed Data)", x = "Total Steps", y = "Count of Days")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Next, let's calculate the mean and median of the total daily steps using the imputed dataset, to see if both values undergo a significant change since we first observed the values in Step 2:

``` r
mean_steps_imputed <- mean(daily_steps_imputed$steps)
median_steps_imputed <- median(daily_steps_imputed$steps)

mean_steps_imputed
```

```
## [1] 10766.19
```

``` r
median_steps_imputed
```

```
## [1] 10766.19
```

As you can see, this method for handling missing step counts involved replacing each missing value with the average number of steps for that specific time period. This approach ensured that the overall daily average and median step counts remained the same as in our original analysis. By using representative values for each time interval, we maintained the consistency and reliability of our results. This demonstrates the importance of careful data handling in preserving the accuracy of our findings.

## Are There Differences in Activity Patterns Between Weekdays and Weekends?
Here, we categorize each day as either a “weekday” or a “weekend”. I used the `weekdays()` function to determine the day of the week for each date. If the day is Saturday or Sunday, it’s labeled as “weekend”; otherwise, it’s labeled as “weekday”. Next, I converted the `day_type` column to a factor with the levels ordered as “weekday” and “weekend” to ensure consistent plotting and analysis in later steps.

``` r
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday","Sunday"),
                                "weekend", "weekday")
imputed_data$day_type <- factor(imputed_data$day_type, levels = c("weekday", "weekend"))
```

The next step is to calculate the average number of steps taken during each time interval, distinguishing between weekdays and weekends. I chose the base R function `aggregate()` to group the data by both interval and day type, then compute the average steps for each group. After that, we convert the interval numbers into a standard time format by adding leading zeros and extracting the hour and minute components. This transformation allows us to plot the data on a continuous time axis, making it easier to compare activity patterns between weekdays and weekends.

``` r
interval_daytype <- aggregate(steps ~ interval + day_type, data = imputed_data, FUN = mean)

interval_padded <- sprintf("%04d", interval_daytype$interval)
hours <- as.integer(substr(interval_padded, 1, 2))
minutes <- as.integer(substr(interval_padded, 3, 4))

interval_daytype$time_of_day <- as.POSIXct(
  sprintf("1970-01-01 %02d:%02d:00", hours, minutes),
  format = "%Y-%m-%d %H:%M:%S", tz = "GMT"
)
```

This code plots the average number of steps taken throughout the day, comparing patterns between weekdays and weekends. It maps `time_of_day` to the x-axis and steps to the y-axis, with line colors distinguishing between “weekday” and “weekend” activities. The plot is divided into separate panels for weekdays and weekends using `facet_wrap`, facilitating an easy comparison of activity trends. The x-axis is formatted to display military time in 4-hour intervals for clarity. Custom colors (#FF5733 for weekdays and #00BFFF for weekends) enhance visual distinction, and the `theme_minimal` provides a clean, professional look. Additional theme adjustments ensure that titles and labels are bold and colored appropriately, while removing the legend keeps the focus on the faceted comparisons.

``` r
ggplot(interval_daytype, aes(x = time_of_day, y = steps, color = day_type)) +
  geom_line(size = 1.2) +
  facet_wrap(~ day_type, ncol = 1) +
  scale_x_datetime(date_breaks = "4 hours", date_labels = "%H:%M", expand = c(0,0)) +
  scale_color_manual(values = c("weekday" = "#FF5733",    
                                "weekend" = "#00BFFF")) + 
  labs(title = "Avg. Steps by Time of Day for Weekdays vs. Weekends",
       x = "Time of Day",
       y = "Average Number of Steps") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, color = "#0099f9"),
    strip.text = element_text(face = "bold", size = 14, color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_line(color = "gray80"),
    legend.position = "none"
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

When comparing the average number of steps taken throughout the day, clear differences emerge between weekdays and weekends. On weekdays, there’s a noticeable increase in activity during the morning and evening hours, likely reflecting the routines of getting ready for work or school and winding down afterward. In the middle of the day, activity levels tend to drop, possibly due to periods of sedentary work or study. Conversely, weekends exhibit a more balanced and steady level of activity throughout the day, without the sharp peaks seen on weekdays. This suggests that weekends offer a more relaxed schedule, allowing for consistent movement and a variety of activities without the structured demands of weekday routines.

## Conclusion
In conclusion, this analysis demonstrated the importance of addressing missing data to ensure accurate results. By examining daily and interval-level activity, we uncovered distinct patterns, such as higher activity peaks during weekdays compared to more evenly distributed activity on weekends. These findings offer insights into daily routines and the role of structured schedules in physical activity.

This R Markdown document follows reproducible research principles by providing clear and well-documented code for each analytical step, including data loading, preprocessing, visualization, and interpretation. The organized sections are designed to support replication and verification of the results.

## References
	- Wickham, H. (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York.
	- Hadley Wickham, Romain François, Lionel Henry, and Kirill Müller (2023). dplyr: A Grammar of Data Manipulation. R package version 1.1.2.
	- Carson Sievert and Hadley Wickham (2023). plotly: Create Interactive Web Graphics via ‘plotly.js’. R package version 4.10.0.
	- Jeroen Ooms (2022). hrbrthemes: Extra Themes, Theme Components and Utilities for ‘ggplot2’. R package version 0.8.0.
	- Xie, Y. (2015). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package version 1.22.
