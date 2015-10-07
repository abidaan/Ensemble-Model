#Author: Abidaan Joshua Nagawkar.

#Clear environment variables.
rm(list=ls())

#Install and load the required packages. Uncomment if required.
#install.packages('gtools', dependencies=TRUE)
#install.packages('caret', dependencies=TRUE)
#install.packages('caretEnsemble', dependencies=TRUE)
#install.packages('gbm', dependencies=TRUE)
#install.packages('randomForest', dependencies=TRUE)
#install.packages('devtools', dependencies=TRUE)
library(devtools)
#install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
library(gtools)
library(caret)
library(caretEnsemble)
library(kernlab)
library(rpart)
library(gbm)
library(randomForest)
library(ggplot2)
library(plyr)
library(dplyr)

#Function to calculate the Mean Squared Error.
meanSquaredError <- function(true_values, predicted_values) {
  mse <- mean((true_values - predicted_values)^2)
  return (mse)
}

#Define vectors for saving mean squared errors.
iterations <- c(1:25)
linear_regression_error <- vector(mode="numeric", length=25)
svm_regression_error <- vector(mode="numeric", length=25)
regression_tree_error <- vector(mode="numeric", length=25)
ensemble_error <- vector(mode="numeric", length=25)
random_forests_error <- vector(mode="numeric", length=25)
gbm_error <- vector(mode="numeric", length=25)

#Read Data from file and drop unwanted columns.
power_weather_data_daily <- read.table("power_weather_data_daily", header=T, sep=",")
power_weather_data_daily <- power_weather_data_daily[,c(1:2,4:11,16)]

#Run 25 iterations to test all models.
for(i in iterations) {
  #Create the training and test data sets.
  set.seed(i)
  training_data <- power_weather_data_daily[sample(1:nrow(power_weather_data_daily), ceiling(0.5*nrow(power_weather_data_daily))),]
  test_data <- subset(power_weather_data_daily, !(power_weather_data_daily[,1] %in% training_data[,1]), select=names(power_weather_data_daily))

  #Create a caretList of Linear Regression, SVM and Regression Trees for use in caretEnsemble.
  options(warn=-1)
  modelList <- caretList(Power_Factor ~ TemperatureF + DewpointF + PressureIn + WindDirectionDegrees 
                       + WindSpeedMPH + WindSpeedGustMPH + Humidity + HourlyPrecipIn + dailyrainin,
                       data = training_data, methodList=c("lm", "svmRadial", "rpart2"))
  options(warn=0)

  #Check the performance of Linear Regression on test data.
  linear_regression_model_predictions <- predict(modelList$lm, test_data)
  linear_regression_error[i] <- meanSquaredError(test_data[,"Power_Factor"], linear_regression_model_predictions)

  #Check the performance of SVM on test data.
  svm_regression_model_predictions <- predict(modelList$svmRadial, test_data)
  svm_regression_error[i] <- meanSquaredError(test_data[,"Power_Factor"], svm_regression_model_predictions)

  #Check the performance of the Regression Tree on test data.
  regression_tree_predictions <- predict(modelList$rpart2, test_data)
  regression_tree_error[i] <- meanSquaredError(test_data[,"Power_Factor"], regression_tree_predictions)

  #Create the ensemble model and check its performance on test data.
  ensemble_model <- caretEnsemble(modelList)
  ensemble_predictions <- ((ensemble_model$weights[1]*linear_regression_model_predictions) + 
                           (ensemble_model$weights[2]*svm_regression_model_predictions)+ 
                           (ensemble_model$weights[3]*regression_tree_predictions))
  ensemble_error[i] <- meanSquaredError(test_data[,"Power_Factor"], ensemble_predictions)

  #Model a Random Forest on the trianing data and check its performance on test data.
  random_forests_fit <- randomForest(Power_Factor ~ TemperatureF + DewpointF + PressureIn + WindDirectionDegrees + WindSpeedMPH + WindSpeedGustMPH + Humidity + HourlyPrecipIn + dailyrainin, data = training_data)
  random_forests_predictions <- predict(random_forests_fit, test_data)
  random_forests_error[i] <- meanSquaredError(test_data[,"Power_Factor"], random_forests_predictions)

  #Model a modified version of AdaBoost on the training data and check its performance on test data.
  gbm_fit <- gbm(Power_Factor ~ TemperatureF + DewpointF + PressureIn + WindDirectionDegrees + WindSpeedMPH + WindSpeedGustMPH + Humidity + HourlyPrecipIn + dailyrainin, data = training_data, n.trees=500)
  gbm_predictions <- predict.gbm(gbm_fit, test_data, n.trees=500)
  gbm_error[i] <- meanSquaredError(test_data[,"Power_Factor"], gbm_predictions)
}

###---###---###---###---###---###---###---###---###---###---###---###---###---###---###---###
#Visualization.
#Create a data frame for use in ggplot.
predictionsPlotFrame <- data.frame(test_data[,"Power_Factor"], linear_regression_model_predictions,
                             svm_regression_model_predictions, regression_tree_predictions,
                             ensemble_predictions, random_forests_predictions, gbm_predictions)
row.names(predictionsPlotFrame) <- NULL
predictionsPlotFrame <- rename(predictionsPlotFrame, Actual_Values=test_data....Power_Factor..)
predictionsPlotFrame <- rename(predictionsPlotFrame, Linear_Regression_Predictions=linear_regression_model_predictions)
predictionsPlotFrame <- rename(predictionsPlotFrame, SVM_Predictions=svm_regression_model_predictions)
predictionsPlotFrame <- rename(predictionsPlotFrame, Regression_Tree_Predictions=regression_tree_predictions)
predictionsPlotFrame <- rename(predictionsPlotFrame, Ensemble_Predictions=ensemble_predictions)
predictionsPlotFrame <- rename(predictionsPlotFrame, Random_Forests_Predictions=random_forests_predictions)
predictionsPlotFrame <- rename(predictionsPlotFrame, GBM_Predictions=gbm_predictions)

#Plot the predictions made by the Linear Regression Model in comparison to the actual values.
lrp <- ggplot(predictionsPlotFrame) + geom_point(aes(c(1:511), Actual_Values))
lrp <- lrp + geom_point(aes(c(1:511), Linear_Regression_Predictions),color="green3", shape=17)
lrp <- lrp + labs(title="Performance of Linear Regression") + xlab("Test Instances") + ylab("Power Factor")
lrp <- lrp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                   axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
lrp <- lrp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                   axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                   panel.border=element_rect(fill=NA, color="black"))
lrp

#Plot the predictions made by the SVM Model in comparison to the actual values.
svmp <- ggplot(predictionsPlotFrame) +geom_point(aes(c(1:511), Actual_Values))
svmp <- svmp + geom_point(aes(c(1:511), SVM_Predictions), color="skyblue1", shape=17)
svmp <- svmp + labs(title="Performance of SVM") + xlab("Test Instances") + ylab("Power Factor")
svmp <- svmp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                   axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
svmp <- svmp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                   axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                   panel.border=element_rect(fill=NA, color="black")) 
svmp

#Plot the predictions made by the Regression Tree Model in comparison to the actual values.
rtp <- ggplot(predictionsPlotFrame) +geom_point(aes(c(1:511), Actual_Values))
rtp <- rtp + geom_point(aes(c(1:511), Regression_Tree_Predictions), color="gold", shape=17)
rtp <- rtp + labs(title="Performance of Regression Trees") + xlab("Test Instances") + ylab("Power Factor")
rtp <- rtp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                     axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
rtp <- rtp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                     axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                     panel.border=element_rect(fill=NA, color="black")) 
rtp

#Plot the predictions made by the Ensemble Model in comparison to the actual values.
emp <- ggplot(predictionsPlotFrame) +geom_point(aes(c(1:511), Actual_Values))
emp <- emp + geom_point(aes(c(1:511), Ensemble_Predictions), color="firebrick3", shape=17)
emp <- emp + labs(title="Ensemble Model") + xlab("Test Instances") + ylab("Power Factor")
emp <- emp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                   axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
emp <- emp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                   axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                   panel.border=element_rect(fill=NA, color="black")) 
emp

#Plot a graph of the errors of all models
errors <- c(mean(linear_regression_error), mean(svm_regression_error), 
            mean(regression_tree_error), mean(ensemble_error), mean(random_forests_error), 
            mean(gbm_error))
models <- c("Linear Regression", "SVM", "Regression Tree", "Ensemble", "Random Forests", "AdaBoost")
errorPlotFrame <- data.frame(models, errors)
errorPlotFrame[,2] <- round(errorPlotFrame[,2], 5)
errorPlotFrame <- transform(errorPlotFrame, models=reorder(models,-errors))
errorPlot <- ggplot(errorPlotFrame, aes(x=models, y=errors, fill=factor(models))) + geom_bar(stat="identity", width=0.40) 
errorPlot <- errorPlot + scale_fill_manual(values=c( "darkorange", "green", "blue", "gold", "firebrick3", "darkorchid"))
errorPlot <- errorPlot + labs(title="Mean Square Error Comparison") + xlab("Models") + ylab("Mean Square Error")
errorPlot <- errorPlot + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                               axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), 
                               axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
errorPlot <- errorPlot + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black", face="bold"),
                               axis.text.y=element_text(size=12.5, vjust=0.5, color="black", face="bold"),
                               panel.border=element_rect(fill=NA, color="black")) 
errorPlot <- errorPlot + theme(legend.position="bottom", legend.title=element_blank()) + geom_text(aes(label=errors), vjust=-1.0)
errorPlot

#Plot a graph of the variable importance
variableNames <- row.names(random_forests_fit$importance)
variableImportanceFrame <- data.frame(variableNames, random_forests_fit$importance)
row.names(variableImportanceFrame) <- NULL
variableImportanceFrame <- rename(variableImportanceFrame, Importance=IncNodePurity)
variableImportanceFrame <- transform(variableImportanceFrame, variableNames=reorder(variableNames,Importance))
variableImportancePlot <- ggplot(variableImportanceFrame, aes(x=variableNames, y=Importance, fill=factor(variableNames))) + geom_bar(stat="identity", width=0.5) + coord_flip()
variableImportancePlot <- variableImportancePlot + labs(title="Feature Importance") +xlab("Features") +ylab("Importance")
variableImportancePlot <- variableImportancePlot + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                                                         axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), 
                                                         axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
variableImportancePlot <- variableImportancePlot + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black", face="bold"),
                                                         axis.text.y=element_text(size=12.5, vjust=0.5, color="black", face="bold"),
                                                         panel.border=element_rect(fill=NA, color="black")) 
variableImportancePlot <- variableImportancePlot + theme(legend.position="right", legend.title=element_blank())
variableImportancePlot

#Plot the predictions made by Random Forests in comparison to the actual values.
rfp <- ggplot(predictionsPlotFrame) +geom_point(aes(c(1:511), Actual_Values))
rfp <- rfp + geom_point(aes(c(1:511), Random_Forests_Predictions), color="firebrick3", shape=17)
rfp <- rfp + labs(title="Random Forests") + xlab("Test Instances") + ylab("Power Factor")
rfp <- rfp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                   axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
rfp <- rfp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                   axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                   panel.border=element_rect(fill=NA, color="black")) 

#Plot the predictions made by AdaBoost in comparison to the actual values.
gbmp <- ggplot(predictionsPlotFrame) +geom_point(aes(c(1:511), Actual_Values))
gbmp <- gbmp + geom_point(aes(c(1:511), GBM_Predictions), color="firebrick3", shape=17)
gbmp <- gbmp + labs(title="AdaBoost") + xlab("Test Instances") + ylab("Power Factor")
gbmp <- gbmp + theme(plot.title=element_text(size=20, face="bold", vjust=2),
                   axis.title.x=element_text(size=17.5, face="bold", vjust=0.50), axis.title.y=element_text(size=17.5, face="bold", vjust=0.50))
gbmp <- gbmp + theme(panel.background=element_rect(fill="white"), axis.text.x=element_text(size=12.5, vjust=0.5, color="black"),
                   axis.text.y=element_text(size=12.5, vjust=0.5, color="black"),
                   panel.border=element_rect(fill=NA, color="black"))

#Plot for comparison of our Ensemble Method, Random Forests and AdaBoost.
ggplot2.multiplot(emp, rfp, gbmp, cols=3)