# Ensemble-Model
This repository contains the R programs for the project - A Comparison of Ensemble Methods for Predicting Power Consumption with Variation in Weather Parameters

###Project Details
Name of the Project: A comparison of Ensemble Methods for predicting power consumption with variation in weather parameters.   

###Contents 
1) household_power_consumption.txt - The text file containing data about power consumption. This file is not provided as part of the package. It can be downloaded from the link: https://archive.ics.uci.edu/ml/machine-learning-databases/00235/      
2) weather.csv - The text file containing data about weather: https://www.dropbox.com/s/bcc7cbdkztldkui/weather.csv?dl=0   
3) Power_Data_Cleaning.R - The R script used to pre-process the power data.   
4) Weather_Data_Cleaning.R - The R script used to pre-process the weather data.   
5) Data_Merge.R - The R script used to merge the two data sources.   
6) Modeling_and_Visualizations.R - The R script used for modeling and visualizations.   
7) Ensemble.R - The R script that will run all the given scripts.   

###Instructions 
1) Please download the file containing power data from the given link.   
2) Please set the current working directory to the one which contains the project files.   
3) Please run the following command in the R terminal: source(“Project_ajnagawk.R”)   

###Files Generated 
1) power_consumption_daily and power_consumption_hourly - The CSV files of the cleaned and aggregated power data.   
2) weather_data_daily and weather_data_hourly - The CSV files of the cleaned and aggregated weather data.   
3) power_weather_data_daily and power_weather_data_daily - The CSV files of the merged data. The power_weather_data_daily file will be used for modeling and visualizations.
