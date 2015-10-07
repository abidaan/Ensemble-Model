#Author: Abidaan Joshua Nagawkar.

#Clear environment variables.
rm(list=ls())

#Install libraries.
#Source: https://stat.duke.edu/~mc301/datafest/
#install.packages('devtools', dependencies=TRUE)
library(devtools)
#install_github("hadley/readr")
#install_github("hadley/tidyr")
#install_github("hadley/dplyr")
#install_github("hadley/lubridate")
#install.packages('xts', dependencies=TRUE)

#Load libraries
library(readr)
library(dplyr)
library(xts)
library(lubridate)

#Read the data from file and construct data frame.
#The NA values are represented as ? in the original file and hence the na.strings="?" argument is added.
power_consumption <- read.table("household_power_consumption.txt", header=T, sep=";", na.strings="?")

#Drop unwanted columns
power_consumption <- power_consumption[,1:4]

#Rename columns
power_consumption <- rename(power_consumption, Active_Power = Global_active_power)
power_consumption <- rename(power_consumption, Reactive_Power = Global_reactive_power)

#Remove records with missing values.
#Missing Values are 1.25% of the entire data or approximately 25000.
power_consumption <- power_consumption[complete.cases(power_consumption),]

#Calculate Apparent Power
power_consumption <- mutate(power_consumption, Apparent_Power = sqrt(Active_Power^2 + Reactive_Power^2))
power_consumption <- mutate(power_consumption, Power_Factor = (Active_Power/Apparent_Power))

#Check that there are no rows with incorrect values for power factor. 
#The incorrect data frame should contain zero rows.
#Uncomment if required.
#test <- subset(power_consumption, power_consumption$Power_Factor < 0 | power_consumption$Power_Factor > 1, select=names(power_consumption))

#Convert the Date column into Date format.
power_consumption[,1] <- as.Date(power_consumption[,1], "%d/%m/%Y")

#Drop records before the year 2008.
power_consumption <- filter(power_consumption, Date > "2007/12/31")

#Combine Date and Time columns into POSIXct formate for time series data.
power_consumption[,"Date_Time"] <- as.POSIXct(paste(power_consumption$Date, power_consumption$Time), format="%Y-%m-%d %H:%M:%S")
power_consumption <- power_consumption[,-c(1:2)]
power_consumption <- power_consumption[,c(5,1:4)]

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

#Function to convert the Date into type of day (Weekday/Weekend).
dayTypeConverter <- function(day){
  ifelse((wday(day) == 1 | wday(day) == 7), "Weekend", "Weekday")
}

#Aggregate data (Hours and Days).
power_consumption_hourly <- aggregateToPeriod(power_consumption)
power_consumption_daily <- aggregateToPeriod(power_consumption, "days")

#Add column for type of the day (Weekday/Weekend).
power_consumption_hourly <- mutate(power_consumption_hourly, Day_Type = sapply(power_consumption_hourly[,1], dayTypeConverter))
power_consumption_daily <- mutate(power_consumption_daily, Day_Type = sapply(power_consumption_daily[,1], dayTypeConverter))

#Rearrange columns.
power_consumption_hourly <- power_consumption_hourly[,c(1,6,2:5)]
power_consumption_daily <- power_consumption_daily[,c(1,6,2:5)]

#Modify the Date_Time column.
#Display only date and hour for hourly data.
#Display only date for daily data.
power_consumption_hourly[,1] <- as.POSIXct(strptime(power_consumption_hourly[,1], "%Y-%m-%d %H"))
power_consumption_daily[,1] <- as.POSIXct(strptime(power_consumption_daily[,1], "%Y-%m-%d"))

#Function to remove missing and duplicate records.
cleanData <- function(data_frame) {
  new_data_frame <- data_frame[complete.cases(data_frame),]
  new_data_frame <- new_data_frame[!duplicated(new_data_frame[,1]),]
  rownames(new_data_frame) <- NULL
  return (new_data_frame)
}

#Remove missing and duplicate records.
power_consumption_hourly <- cleanData(power_consumption_hourly)
power_consumption_daily <- cleanData(power_consumption_daily)

#Write data to CSV File.
write.table(power_consumption_hourly, file="power_consumption_hourly", sep=",", row.names=F)
write.table(power_consumption_daily, file="power_consumption_daily", sep=",", row.names=F)