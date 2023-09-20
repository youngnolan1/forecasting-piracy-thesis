#-------------------------------------------------------------------------------
# Script Name: binary_forecasting.R
# Author: NYZ
# Description: out-of-sample forecasting using binary DV. Yields AUC boxplots 
#              and t-tests.
#-------------------------------------------------------------------------------


#Import libraries
library(ROCR)
library(dplyr)


#Function
runPredictionsTest <- function(mydata, reportPlots){
  
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
    glm.main <- glm(BinaryPiracyIncident ~ TotalViolentIncidents + TotalCasualties + InlandIncidents + InlandCasualties + CoastalIncidents + CoastalCasualties + Monsoon + MajorShippingLane +
                      GDPpc + Population + Unemployment + FishStocks + Military + Trade + CoastlineLength + Ports + ChokepointDistance1 + ChokepointDistance2 +
                      Chokepoints + OngoingCivilWar + StateFragility + StateReach, family = 'binomial', data = learningSet)
    
    glm.lit <- glm(BinaryPiracyIncident ~ GDPpc + Population + Unemployment + FishStocks + Military + Trade + CoastlineLength + Ports + ChokepointDistance1 +
                     ChokepointDistance2 + Chokepoints + OngoingCivilWar + StateFragility + StateReach, family = 'binomial', data = learningSet)
    
    glm.base <- glm(BinaryPiracyIncident ~ GDPpc + Population + Unemployment + Military + Trade, family = 'binomial', data = learningSet)
    
    #Use coefficients to predict on test set (rolling.year)
    mydata$predictions.main[mydata$Year == rolling.year] <- as.numeric(predict(glm.main, newdata = testSet))
    mydata$predictions.lit[mydata$Year == rolling.year] <- as.numeric(predict(glm.lit, newdata = testSet))
    mydata$predictions.base[mydata$Year == rolling.year] <- as.numeric(predict(glm.base, newdata = testSet))
  }
  
  
  #Subset to only include 2010-2017 values
  mydata <- mydata %>%
    filter(Year %in% c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017))
  
  #Calculate metrics
  pred.main <- prediction(mydata$predictions.main, mydata$BinaryPiracyIncident)
  pred.lit <- prediction(mydata$predictions.lit, mydata$BinaryPiracyIncident)
  pred.base <- prediction(mydata$predictions.base, mydata$BinaryPiracyIncident)
  
  #ROC curve and plot
  roc.main <- performance(pred.main, measure = "tpr", x.measure = "fpr")
  roc.lit <- performance(pred.lit, measure = "tpr", x.measure = "fpr")
  roc.base <- performance(pred.base, measure = "tpr", x.measure = "fpr")
  
  
  if(reportPlots == TRUE){
    op <- par(family = "serif")
    plot(roc.main, col=2)
    abline(a=0, b=1)
    
    plot(roc.lit, col=3, add=T)
    plot(roc.base, col=1, add=T)
    
    legend("bottomright", c("Main", "Literature", "Baseline"), lty=1, 
           col = c("red", "green", "black"), bty="n")
    par(op)
  }
  

  #Precision-recall curve
  pr.main <- performance(pred.main, measure = "prec", x.measure = "rec")
  pr.lit <- performance(pred.lit, measure = "prec", x.measure = "rec")
  pr.base <- performance(pred.base, measure = "prec", x.measure = "rec")
  
  if(reportPlots == TRUE){
    op <- par(family = "serif")
    plot(pr.main, col=2)
    plot(pr.lit, col=3, add = T)
    plot(pr.base, col=1, add = T)
    
    legend("topright", c("Main", "Literature", "Baseline"), lty=1, 
           col = c("red", "green", "black"), bty="n")
    par(op)
    }
  
  #Calculate AUC
  auc.main <- performance(pred.main, measure = "auc")
  this.auc.main <- auc.main@y.values[[1]]
  
  auc.lit <- performance(pred.lit, measure = "auc")
  this.auc.lit <- auc.lit@y.values[[1]]
  
  auc.base <- performance(pred.base, measure = "auc")
  this.auc.base <- auc.base@y.values[[1]]
  
  #Store results
  return(c(this.auc.base, this.auc.lit, this.auc.main))
  
}

runPredictionsTest(mydata = master_imputed, reportPlots = TRUE)


### AUC Confidence (are differences statistically significant?) ###

#Initialize variables
AUCs.rolling.basemodel <- NA
AUCs.rolling.litmodel <- NA
AUCs.rolling.mainmodel <- NA

#Re-sample many times to calculate confidence
for(i in 1:100){
  sampleObs <- sample(1:nrow(master_imputed), size = nrow(master_imputed), replace=T)
  alternate.world.data <- master_imputed[sampleObs,]
  
  this.auc.rolling <- runPredictionsTest(mydata = alternate.world.data, reportPlots = FALSE)
  AUCs.rolling.basemodel <- c(AUCs.rolling.basemodel, this.auc.rolling[1])
  AUCs.rolling.litmodel <- c(AUCs.rolling.litmodel, this.auc.rolling[2])
  AUCs.rolling.mainmodel <- c(AUCs.rolling.mainmodel, this.auc.rolling[3])
}


#Plot
op <- par(family = "serif")
aucboxplot <- boxplot(AUCs.rolling.basemodel, AUCs.rolling.litmodel, AUCs.rolling.mainmodel, main = "AUC Distributions", names = c("Baseline", "Literature", "Main"))

#T-test
t.test(AUCs.rolling.litmodel, AUCs.rolling.mainmodel)

