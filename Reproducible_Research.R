
library("data.table")
library(ggplot2)

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
DataActivity <- data.table::fread(input = "data/activity.csv")
summary(DataActvity)
Steps_Day <- DataActivity[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(Steps_Day, 10)
ggplot(Steps_Day, aes(x = steps)) +
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
Steps_Day[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
IntervalDay <- DataActivity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 
ggplot(IntervalDay, aes(x = interval , y = steps)) + geom_line(color="red", size=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Avg. Steps per day")
IntervalDay[steps == max(steps), .(max_interval = interval)]
DataActivity[is.na(steps), .N ]
DataActivity[is.na(steps), "steps"] <- DataActivity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
data.table::fwrite(x = DataActivity, file = "data/tidyData.csv", quote = FALSE)
Steps_Day <- DataActivity[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 
Steps_Day[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
ggplot(Steps_Day, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
DataActivity <- data.table::fread(input = "data/activity.csv")
DataActivity[, date := as.POSIXct(date, format = "%Y-%m-%d")]
DataActivity[, `Day of Week`:= weekdays(x = date)]
DataActivity[grepl(pattern = "lunes|martes|miércoles|jueves|viernes", x = `Day of Week`), "weekday or weekend"] <- "weekday"
DataActivity[grepl(pattern = "sábado|domingo", x = `Day of Week`), "weekday or weekend"] <- "weekend"
DataActivity[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(DataActivity, 10)
DataActivity[is.na(steps), "steps"] <- DataActivity[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDay <- DataActivity[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 
ggplot(IntervalDay, aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Average Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)

