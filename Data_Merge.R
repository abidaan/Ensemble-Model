#Author: Abidaan Joshua Nagawkar.

#Clear environment variables.
rm(list=ls())

#Read data from files.
power_data_hourly <- read.table("power_consumption_hourly", header=T, sep=",")
power_data_daily <- read.table("power_consumption_daily", header=T, sep=",")
weather_data_hourly <- read.table("weather_data_hourly", header=T, sep=",")
weather_data_daily <- read.table("weather_data_daily", header=T, sep=",")

#Merge hourly data from power_data_hourly and weather_data_hourly into one data frame.
#Merge daily data from power_data_daily and weather_data_daily into one data frame.
power_weather_hourly <- merge(weather_data_hourly, power_data_hourly)
power_weather_daily <- merge(weather_data_daily, power_data_daily)

#Write data to CSV File.
write.table(power_weather_hourly, file="power_weather_data_hourly", sep=",", row.names=F)
write.table(power_weather_daily, file="power_weather_data_daily", sep=",", row.names=F)
