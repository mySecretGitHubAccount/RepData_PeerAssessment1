## Unzip and Read the Data
## 1. Code for reading in the dataset and/or processing the data
## No further analysis is required in this stage
data <- read.csv(unz("activity.zip", "activity.csv"))

## Calc steps Per Day and remove NA
stepsPerDay <- tapply(data$steps,data$date,sum,na.rm = TRUE)

## Create histogram
## 2. Histogram of the total number of steps taken each day
par(mfrow=c(1,1))
hist(stepsPerDay,20)

## Calc mean per day and remove NA
## 3. Mean and median number of steps taken each day
meanStepsPerDay <- mean(stepsPerDay)

## Calc median per day and remove NA
medianStepsPerDay <- median(stepsPerDay)

## 4. Time series plot of the average number of steps taken
meanStepPerInterval <- tapply(data$steps,data$interval,mean,na.rm = TRUE)
plot(as.numeric( names(meanStepPerInterval)),meanStepPerInterval)

# 5 The 5-minute interval that, on average, contains the maximum number of steps
print(max(meanStepPerInterval))

#6. Code to describe and show a strategy for imputing missing data
dataCln <- data[!is.na( data$steps),]

#7. Histogram of the total number of steps taken each day after missing values are imputed
stepsPerDay <- tapply(data$steps,data$date,sum,na.rm = TRUE)
hist(stepsPerDay,20)

#8. Panel plot comparing the average number of steps taken per 
#   5-minute interval across weekdays and weekends
day <- weekdays(as.Date(data$date,'%Y-%m-%d'))
weekendDays <- (day=="Sunday" | day=="Saturday")
meanStepPerIntervalWeekend <- tapply(data[weekendDays,]$steps,data[weekendDays,]$interval,mean,na.rm = TRUE)
meanStepPerIntervalWeekdays <- tapply(data[!weekendDays,]$steps,data[!weekendDays,]$interval,mean,na.rm = TRUE)

## Create Figures
par(mfrow=c(2,1))
plot(as.numeric( names(meanStepPerIntervalWeekend)),meanStepPerIntervalWeekend, main="umber of steps taken per 5-minute interval - weeends")
plot(as.numeric( names(meanStepPerIntervalWeekdays)),meanStepPerIntervalWeekdays, main="umber of steps taken per 5-minute interval - weekdays")


