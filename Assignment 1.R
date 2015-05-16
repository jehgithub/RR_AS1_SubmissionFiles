
##---------------------------------------------------------------------------------------
##  For file activity.csv - Claculate the Sum, Mean & Median of the total steps per day
##  and create a Histogram of total number of steps taken each day
##---------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(lattice)
##---------------------------------------------------------------------------------------
##  Loading and preprocessing the data
##---------------------------------------------------------------------------------------

  filepath<- "e:/1RData/activity.csv"
  actdata<- read.csv(filepath,header=T,sep = ",",na.strings = "NA",stringsAsFactors=FALSE)

  str(actdata)
  head(actdata)

##---------------------------------------------------------------------------------------
## What is the mean total number of steps taken per day ? 
##---------------------------------------------------------------------------------------

## aggregate steps for intervals by date  
  sum_actdata <- aggregate (steps ~ date, actdata, FUN = sum)
  
  head(sum_actdata)

## grand total of Steps / Day & Mean & Median

  gtsum_steps <- sum(sum_actdata$steps, na.rm = T)
  gtmean_steps <- mean(sum_actdata$steps, na.rm = T)
  gtmedian_steps <- median(sum_actdata$steps, na.rm = T)

## display total of Steps / Day & Mean & Median

  gtsum_steps
  gtmean_steps
  gtmedian_steps

##----------------------------------------------------------------------------------------
##  Create Histogram of Daily Activity Data

  histdata <- aggregate (steps ~ date, actdata, FUN = sum)
  histdata$plotdate <- as.Date(histdata$date, format="%Y-%m-%d")
  head(histdata)

  ## png("histdata1.png", width=480, height=480)

  ggplot(histdata, aes(x = steps)) +
    geom_histogram(fill = "purple", binwidth = 500) +
    labs(title = "Histogram - Daily Activity Data", x = "Steps / Day", y = "Frequency") 

  ## dev.off()

##-------------------------------------------------------------------------------------------
##  What is the average daily activity pattern?  Make a time series plot of the 5-minute 
##  interval (x) and the average number of steps taken averaged across all of the days (y)
##  Which 5-Minute Interval on average, across all days contains the maximum number of steps? 
##-------------------------------------------------------------------------------------------

##  average intervals across days
    mean_actdata_interval <- aggregate (steps ~ interval, actdata, FUN = mean)
    
    head(mean_actdata_interval)

## Plot time series of Average Daily Activity

  ## png("timeseries1.png", width=480, height=480)

    ggplot(mean_actdata_interval, aes(x=interval, y=steps)) +
      geom_line(color = "purple") + 
      labs(title = "Average Daily Activity", x = "Interval", y = "Steps") 
  
   ## dev.off()


## Determine Max 5 Minute Interval

    max_interval <- mean_actdata_interval$interval[which.max(mean_actdata_interval$steps)]
    max_interval

##-------------------------------------------------------------------------------------------
##  Imputing Missing values
##-------------------------------------------------------------------------------------------

## Calculate and report the total number of missing values in the dataset

   actdata_nas <- sum(is.na(actdata$steps))
   actdata_nas

##-------------------------------------------------------------------------------------------
##  Devise a strategy for filling in all of the missing values in the dataset
##  Missing value strategy:  Set NAs to Population Interval Average and Create New Data Set
##-------------------------------------------------------------------------------------------

##  Create new dataset with imputed values

    actdata_transf <- transform(actdata, ImputedInt = ifelse(is.na(actdata$steps), mean(actdata$steps, na.rm=TRUE), actdata$steps))
    head(actdata_transf)

##  Check For Missing Values

    actdata_transf_nas <- sum(is.na(actdata_transf$ImputedInt))
    actdata_transf_nas

##-------------------------------------------------------------------------------------------
##  Create Histogram of total steps taken each day 
##  Calculate mean & median of total steps per day


  histdata2 <- aggregate (ImputedInt ~ date, actdata_transf, FUN = sum)
  histdata2$plotdate <- as.Date(histdata$date, format="%Y-%m-%d")
  head(histdata2)


  
  ##  png("histdata2.png", width=480, height=480)

  ggplot(histdata2, aes(x = ImputedInt)) +
    geom_histogram(fill = "purple", binwidth = 500) +
    labs(title = "Histogram - Adjusted Daily Activity Data", x = "Steps / Day", y = "Frequency") 

  ##  dev.off()
  

## grand total of Steps / Day & Mean & Median with Adjusted Daily data

  gtsum_tsteps <- sum(histdata2$ImputedInt)
  gtmean_tsteps <- mean(histdata2$ImputedInt)
  gtmedian_tsteps <- median(histdata2$ImputedInt)


## display total of Steps / Day & Mean & Median

  gtsum_tsteps
  gtmean_tsteps
  gtmedian_tsteps


##-------------------------------------------------------------------------------------------
## Are there differences in activity patterns between Weekdays and Weekends?
##-------------------------------------------------------------------------------------------


##--------------------------------------------------------------------------------------------
## Add column daytype using weekend()  


  head(actdata_transf)
  
  actdata_transf$plotdate <- as.Date(actdata_transf$date, format="%Y-%m-%d")

  actdata_daytype <- mutate(actdata_transf, daytype = ifelse(weekdays(actdata_transf$plotdate) == 
    "Saturday" | weekdays(actdata_transf$plotdate) == "Sunday", "weekend", "weekday"))
      actdata_transf$daytype <- as.factor(actdata_transf$plotdate)


  head(actdata_daytype)



##--------------------------------------------------------------------------------------------
## Make a panel plot containing a time series plot of the 5 min interval (x) and the average
## steps taken averaged across all week day days or weekend days (y).
##--------------------------------------------------------------------------------------------




  daytypeplot<- aggregate( ImputedInt ~ daytype * interval, actdata_daytype, FUN =  mean)

  head(daytypeplot)

  ##  png("timeseries2.png", width=480, height=480)

  ggplot(daytypeplot, aes(x=interval, y=ImputedInt, color = daytype)) +
    geom_line() + facet_wrap( ~ daytype, ncol = 1, nrow=2)  +
    labs(title = "Average DayType Activity", x = "Interval", y = "Steps") 

   ## dev.off()




