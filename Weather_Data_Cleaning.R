#Author: Abidaan Joshua Nagawkar.

#CLear environment variables.
rm(list=ls())

#Load libraries.
library(xts)

#Function to aggregate time series data. Minutes to hours.
aggregateToPeriod <- function(data_frame, period="hours"){
  xts_object <- xts(data_frame, data_frame[,1])
  ends <- endpoints(xts_object, on=period)
  aggregated_data <- period.apply(xts_object[,2:ncol(xts_object)], ends, mean)
  aggregated_data <- as.data.frame(aggregated_data)
  aggregated_data <- unique(aggregated_data)
  Date_Time <- rownames(aggregated_data)
  aggregated_data_frame <- cbind(Date_Time, aggregated_data)
  aggregated_data_frame <- aggregated_data_frame[complete.cases(aggregated_data_frame),]
  rownames(aggregated_data_frame) <- NULL
  aggregated_data_frame[,1] <- as.POSIXct(aggregated_data_frame[,1], format="%Y-%m-%d %H:%M:%S")
  return (aggregated_data_frame)
}

#Function to remove missing and duplicate records.
cleanData <- function(data_frame) {
  new_data_frame <- data_frame[complete.cases(data_frame),]
  new_data_frame <- new_data_frame[!duplicated(new_data_frame[,1]),]
  rownames(new_data_frame) <- NULL
  return (new_data_frame)
}

#Read data from file.
weather_data <- read.table("weather.csv", header=T, sep=",", fill=TRUE)

#Drop unwanted columns.
weather_data <- weather_data[,-c(7,13,14)]

#Remove missing values from the data. There are 1299 records with missing values.
weather_data <- weather_data[complete.cases(weather_data),]
rownames(weather_data) <- NULL

#Combine Date and Time columns into POSIXct formate for time series data.
weather_data[,1] <- as.Date(weather_data[,1], "%m/%d/%Y")
weather_data[,"Date_Time"] <- as.POSIXct(paste(weather_data$Date, weather_data$Time), format="%Y-%m-%d %H:%M:%S")
weather_data <- weather_data[,-c(1,2)]
weather_data <- weather_data[,c(11,1:10)]

#Aggregate data (Hours and Days).
#Use the aggregateToPeriod function from Power_Data_Cleaning.R.
weather_data_hourly <- aggregateToPeriod(weather_data)
weather_data_daily <- aggregateToPeriod(weather_data, "days")

#Modify the Date_Time column.
#Display only date and hour for hourly data.
#Display only date for daily data.
weather_data_hourly[,1] <- as.POSIXct(strptime(weather_data_hourly[,1], "%Y-%m-%d %H"))
weather_data_daily[,1] <- as.POSIXct(strptime(weather_data_daily[,1], "%Y-%m-%d"))

#Remove missing and duplicate records.
#Use the cleanData function from Power_Data_Cleaning.R.
weather_data_hourly <- cleanData(weather_data_hourly)
weather_data_daily <- cleanData(weather_data_daily)

#Write data to CSV File.
write.table(weather_data_hourly, file="weather_data_hourly", sep=",", row.names=F)
write.table(weather_data_daily, file="weather_data_daily", sep=",", row.names=F)
