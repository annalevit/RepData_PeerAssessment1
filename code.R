library(lattice)
library(dplyr)
dt <- read.csv("activity.csv", colClasses = "character")
dt$date<-as.Date(dt$date)
dt$steps<-as.numeric(dt$steps)
dt$interval<-as.numeric(dt$interval)


temp<-group_by(dt,date)
total<-summarise(temp, total=sum(steps,na.rm=TRUE))
hist(total$total,col="blue", main="Histogram of the total number of steps taken each day",xlab="Total number of steps taken each day")
meannoim<-mean(total$total,na.rm=TRUE)
mediannoim<-median(total$total,na.rm=TRUE)

temp1<-group_by(dt,interval)
mean<-summarise(temp1, mean=mean(steps,na.rm=TRUE))
qqplot(mean$interval,mean$mean, type="l",xlab="interval",ylab="mean")
interval_max<-mean[which.max(mean$mean),1]

sum(is.na(dt$steps))
dt1<-dt
for (i in 1:length(dt1$steps)){
        if (is.na(dt1$steps[i])){
                x<-dt1$interval[i]
                y<-subset(mean, interval== x)
                dt1$steps[i]<-y[1,2]
        }
}

dt1$steps<-as.numeric(dt1$steps)
temp2<-group_by(dt1,date)
total1<-summarise(temp2, total=sum(steps))
#mean=mean(steps,na.rm=TRUE), median )
hist(total1$total,col="blue", main="Histogram imputing missing data",xlab="Total number of steps taken each day")
meanim<-mean(total1$total,na.rm=TRUE)
medianim<-median(total1$total,na.rm=TRUE)

weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
dt1$wday <- factor((weekdays(dt1$date) %in% weekdays),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
temp3<-group_by(dt1,wday,interval)
mean1<-summarise(temp3, mean=mean(steps))

p<-xyplot(mean ~ interval|wday, data=mean1, layout = c(1, 2),type="l",xlab="Interval", ylab="Number of steps")
print(p)

