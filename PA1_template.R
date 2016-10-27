# Download and save the file in working directory
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile = "Activity monitoring data.zip")

# Unzip the downloaded file in working directory
unzip("Activity monitoring data.zip",exdir = "./Activity monitoring data")

# Read data from downloaded dataset
dat <- read.csv("./Activity monitoring data/activity.csv")

# Calculate total no of steps per day
StepsPerDay <- aggregate(steps~date,dat,sum)

# Open png graphic device 
png(filename="Histogram - Total number of steps taken each day.png")

# Draw histogram of the total number of steps taken each day
hist(StepsPerDay$steps,main = "Histogram - Total number of steps taken each day",
     xlab = "Total No of Steps", ylab = "Frequency(No of Days)")

#Turn off the graphic device
dev.off()

# Mean and median number of steps taken each day
MeanSteps <- mean(StepsPerDay$steps, na.rm = TRUE)
MedianSteps <- median(StepsPerDay$steps, na.rm=TRUE)

# Average steps for each interval for entire data
MeanStepsPerInterval <- aggregate(steps~interval,dat,mean)

# Open png graphic device 
png(filename="Time series plot of the average number of steps.png")

# Draw time series plot of the average number of steps
plot(MeanStepsPerInterval$interval,MeanStepsPerInterval$steps,type = "l",
     main = "Time series plot of the average number of steps ",
     xlab = "Interval", ylab = " Average number of steps")

#Turn off the graphic device
dev.off()

# Calculate the 5-minute interval that, on average, contains the maximum number of steps
MaxStepInterval <- MeanStepsPerInterval$interval[which.max(MeanStepsPerInterval$steps)]

# Calculate no of incomplete rows
IncompleteDat<- sum(!complete.cases(dat))

# Merge original data and mean interval data
NewDat <- merge(dat, MeanStepsPerInterval, by="interval")

# Assign mean interval value to missing data 
NewDat$steps.x[is.na(NewDat$steps.x)] <- NewDat$steps.y[is.na(NewDat$steps.x)]

# Calculate total no of steps per day for new imputed data
NewStepsPerDay <- aggregate(steps.x~date,NewDat,sum)

# Open png graphic device 
png(filename="Histogram comparing total number of steps taken each day for imputed & original data.png")

# Histogram of total no. of steps taken each day for imputed data comparing with original data 

hist(NewStepsPerDay$steps.x,col="red",main = "Histogram - Total number of steps taken each day",
     xlab = "Total No of Steps", ylab = "Frequency(No of Days)")

hist(StepsPerDay$steps,col="blue",add=TRUE)

legend("topright", c("Imputed Data", "Original Data"), col=c("red", "blue"),lwd=6,bty = "n")

#Turn off the graphic device
dev.off()

# Mean and median number of steps taken each day for new imputed data
NewMean <- mean(NewStepsPerDay$steps.x)
NewMedian <- median(NewStepsPerDay$steps.x)

# Difference between mean and median for imputed and original data
DiffMean <- NewMean - MeanSteps
DiffMedian <- NewMedian - MedianSteps

# Assign type(weekday or weekend) in new imputed data
type <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {"weekend"} 
  else {"weekday"}
}
NewDat$type <- as.factor(sapply(NewDat$date, type))

# Segregate weekend and weekdays data
WeekendDat<- NewDat[NewDat$type=="weekend",]
WeekdayDat<- NewDat[NewDat$type=="weekday",]

# Average steps for each interval for new data by type(weekend,weekdays)
WeekendStepsInterval <- aggregate(steps.x~interval,WeekendDat,mean)
WeekdayStepsInterval <- aggregate(steps.x~interval,WeekdayDat,mean)

# Open png graphic device 
png(filename="Time series plot of steps across weekdays and weekends.png")

par(mfrow=c(2,1))

# Draw time series plot of the average number of steps
plot(WeekendStepsInterval$interval,WeekendStepsInterval$steps.x,type = "l",
     main = "Weekend ",
     xlab = "Interval", ylab = " Steps")
plot(WeekdayStepsInterval$interval,WeekdayStepsInterval$steps.x,type = "l",
     main = "Weekday ",
     xlab = "Interval", ylab = " Steps")

#Turn off the graphic device
dev.off()