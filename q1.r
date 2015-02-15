
##1. read the data
q1data <- read.csv("activity.csv")
## clean the data
cleandata<-na.omit(q1data)
## 2. calculate the data
stepsperday <- aggregate(steps ~ date, data=cleandata,sum)
## make the histogram
hist (stepsperday$steps, xlab = "Steps", col = "lightgreen", main = "Steps per day")
print (mean(stepsperday$steps))
print (median(stepsperday$steps))

##3.STEPS per interval
stepsperinterval <- aggregate(steps ~ interval, data=cleandata,mean)
## make the plot
with(stepsperinterval, plot(steps ~ interval, main = "Steps per interval", type = "l"))
## maximum steps. What interval?
maxsteps<- which.max(stepsperinterval$steps)
maxint<-stepsperinterval[maxsteps, "interval"]
print(paste("An interval for maximum steps (average per day) is", maxint))

##4. Imputing missing lvels
## Bad rows
badrows <- nrow(q1data) - nrow(cleandata)
print(paste("The total numbers of bad data rows are", badrows))
##filling the empty values
q2data<-q1data
nr <- nrow(q2data)
for (i in 1:nr) {
    if (is.na(q2data[i,"steps"])) {
        int <- q2data[i,"interval"]
        q2data[i,"steps"] <- stepsperinterval[stepsperinterval$interval== int, "steps"]
    }
}
stepsperday2 <- aggregate(steps ~ date, data=q2data,sum)
## make the histogram
hist (stepsperday2$steps, xlab = "Steps", col = "lightgreen", main = "Steps per day")
print (mean(stepsperday2$steps))
print (median(stepsperday2$steps))
##5/ Are there differences in activity patterns between weekdays and weekends?
wkd <-factor(c("weekday", "weekend"))
for (i in 1:nrow(q2data)) {
    wd <- as.POSIXlt(q2data[i, "date"])$wday
    if ((wd !=0) &(wd !=6)) {
        q2data[i, "Wkd"] <- wkd[1]
    } else{
        q2data[i, "Wkd"] <- wkd[2]
    }
}
stepsperinterval2 <- aggregate(steps ~ interval+Wkd, data=q2data, mean)
## make a panel plot
library(lattice)
xyplot(steps ~ interval | Wkd, data = stepsperinterval2, layout = c(1,2), 
       type = 'l', ylab ="Number of steps")