#-------------------------------------------------------------------------------
# Script Name: eventcount_forecasting.R
# Author: NYZ
# Description: out-of-sample forecasting using event count DV. Yields AUC box-
#              plots and t-tests.
#-------------------------------------------------------------------------------


#Import libraries
library(ROCR)
library(MASS)
library(dplyr)


#Function
runNBPredictions <- function(mydata){
  
  #Initialize variable to store predictions
  mydata$predictions.main <- NA
  mydata$predictions.lit <- NA
  mydata$predictions.base <- NA
  
  #Correct date format
  mydata$Year <- as.numeric(mydata$Year)
  
  for(rolling.year in 2010:2017){
    
    #Define learning and test sets
    learningSet <- mydata[mydata$Year < rolling.year,]
    testSet <- mydata[mydata$Year == rolling.year,]
    
    #Run regressions on learning set  
    glm.main <- glm(CountPiracyIncidents ~ TotalViolentIncidents + TotalCasualties + InlandCasualties + InlandIncidents + CoastalCasualties + CoastalIncidents + GDPpc + Population + Unemployment + FishStocks + Military + Trade + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 + 
                      Chokepoints + OngoingCivilWar + StateFragility + StateReach + MajorShippingLane + Monsoon + CountPiracyLag, data = learningSet, na.action = na.exclude, family = "poisson")
    
    
    glm.lit <- glm(CountPiracyIncidents ~ GDPpc + Population + Unemployment + FishStocks + Military + Trade + CoastlineLength + Ports + ChokepointDistance1 + 
                     ChokepointDistance2 + Chokepoints + OngoingCivilWar + StateFragility + StateReach + CountPiracyLag, data = learningSet, na.action = na.exclude, family = "poisson")
    
    glm.base <- glm(CountPiracyIncidents ~ GDPpc + Population + Unemployment + Military + Trade, data = learningSet, na.action = na.exclude, family = "poisson")
    
    #Use coefficients to predict on test set (rolling.year)
    mydata$predictions.main[mydata$Year == rolling.year] <- as.numeric(predict(glm.main, newdata = testSet, type = "response"))
    mydata$predictions.lit[mydata$Year == rolling.year] <- as.numeric(predict(glm.lit, newdata = testSet, type = "response"))
    mydata$predictions.base[mydata$Year == rolling.year] <- as.numeric(predict(glm.base, newdata = testSet, type = "response"))
  }
  
  
  #Subset to only include 2010-2017 values
  mydata <- mydata %>%
    filter(Year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
  
  #Calculate MSE
  rmse.main <- sqrt(mean((mydata$predictions.main - mydata$CountPiracyIncidents)^2))
  rmse.lit <- sqrt(mean((mydata$predictions.lit - mydata$CountPiracyIncidents)^2))
  rmse.base <- sqrt(mean((mydata$predictions.base - mydata$CountPiracyIncidents)^2))
  
  #Store results
  return(c(rmse.main, rmse.lit, rmse.base))
}


### AUC Confidence (are differences statistically significant?) ###

#Initialize variables
rmse.rolling.mainmodel <- NA
rmse.rolling.litmodel <- NA
rmse.rolling.basemodel <- NA


#Re-sample many times to calculate confidence
for(i in 1:100){
  sampleObs <- sample(1:nrow(master12lag_dvlag_imputed), size = nrow(master12lag_dvlag_imputed), replace=T)
  alternate.world.data <- master12lag_dvlag_imputed[sampleObs,]
  
  this.auc.rolling <- runNBPredictions(mydata = alternate.world.data)
  rmse.rolling.mainmodel <- c(rmse.rolling.mainmodel, this.auc.rolling[1])
  rmse.rolling.litmodel <- c(rmse.rolling.litmodel, this.auc.rolling[2])
  rmse.rolling.basemodel <- c(rmse.rolling.basemodel, this.auc.rolling[3])
}


#Plot
op <- par(family = "serif")
aucboxplot <- boxplot(rmse.rolling.basemodel, rmse.rolling.litmodel, rmse.rolling.mainmodel, main = "RMSE Distributions", names = c("Baseline", "Literature", "Main"))


#T-test
t.test(rmse.rolling.litmodel, rmse.rolling.mainmodel)


