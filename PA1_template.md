#Reproducible Research: Peer Assessment 1  

##Loading and procesing data:

```{r}
dat<- read.csv("./Coursera/repdata-data-activity/activity.csv")
head(dat)
names(dat)
```
Converting variable "date" from string to data type:  
```{r}
dat$date<-as.Date(dat$date, format = '%Y-%m-%d')
head(dat)

```
##What is mean total number of steps taken per day?  
Aggregrating by date
```{r}
agr<- aggregate(steps~date, data= dat, sum, na.rm=TRUE)
```
Making histogram of the total number of steps taken every day
```{r}
hist<-barplot(agr$steps,names.arg=agr$date, main="Number of steps per day", xlab="Date", ylab="Steps", col="blue")
```

Calculating the mean and median of number of steps take per day
```{r}
averag<- mean(agr$steps, na.rm=TRUE)
med<- median(agr$steps, na.rm=TRUE)
```
The mean of taken steps per day is `r averag` and median of taken steps per day is `r med`  

##What is the average daily activity pattern?

Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days 
```{r}
int_averag<- aggregate(steps ~ interval , dat, mean)

plot(int_averag, type="l", xlab="Intervals", ylab="Average number of steps", main="Average number of steps per 5-minute interval", col="red")
```  
  
5-minute interval, which contains the maximum number of steps
```{r}
int_averag$interval[ which.max(int_averag$steps)]
```

##Imputing missing values  
Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs) 
```{r}
sum(!complete.cases(dat))
```  

Devising a strategy for filling in all of the missing values in the dataset( I chose to fill mean into NAs)  

Creating a new dataset the same as original, but with missing values filled in
```{r}
stepsAver <- aggregate(steps ~ interval, data = dat, mean)
miss <- numeric()
for (i in 1:nrow(dat)) {
    obs <- dat[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(stepsAver, interval == obs$interval)$steps
        } 
        else {
        steps <- obs$steps
        }
        miss <- c(miss, steps)
}

newdat <- dat
newdat$steps<- miss

```  

Creating histogram of the total number of steps taken each day  
```{r}
agr2 <- aggregate(steps ~ date, data=newdat,sum, na.rm=TRUE)
hist(agr2$steps, xlab="Date", ylab="Steps", main= "Total number of steps per day", col="green")
```
Calculating the mean and median total number of steps taken per day:
```{r}
mn<-mean(agr2$steps)
med<-median(agr2$steps)
```  
As we see mean `r mn` and median `r med` are equal  


##Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r}
newdat$week <- ifelse(weekdays(newdat$date) %in%  c("Saturday", "Sunday"),'weekend','weekday')

head(newdat)
table(newdat$week)
```  

Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.
```{r}
library(ggplot2)
qplot(x=interval, y=steps,data=subset(newdat, complete.cases(newdat)),geom='smooth', stat='summary', fun.y=mean) + facet_grid(dayType~.) + facet_wrap(~week,nrow=2) + theme(strip.background = element_rect(fill="#ffe5cc")) + labs(title=' Average steps per days, analyzing weekdays and weekend patterns')
``` 

