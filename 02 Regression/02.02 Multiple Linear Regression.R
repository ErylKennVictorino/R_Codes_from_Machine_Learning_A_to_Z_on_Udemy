### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    MULTIPLE LINEAR REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/19/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
library(caTools)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Regression")
dataset = read.csv('50_Startups.csv')

#   ENCODE CATEGORICAL VARIABLES
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))

#   TRAINING/TEST SET SPLIT
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

#   FEATURE SCALING
# we don't need to feature scale since the package used automatically does it for us

#   MULTIPLE LINEAR REGRESSION FIT ON TRAINING SET
regressor = lm(formula = Profit ~ .,
               data = training_set)
summary(regressor)

#   MULTIPLE LINEAR REGRESSION PREDICTION ON TRAINING SET AND TEST SET
y_train_pred = predict(regressor, newdata = training_set)
y_test_pred = predict(regressor, newdata = test_set)

#   FINDING OPTIMAL MODEL USING MANUAL BACKWARDS ELIMINATION
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend,
               data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend,
               data = dataset)
summary(regressor)

#   FINDING OPTIMAL MODEL USING AUTOMATIC BACKWARDS ELIMINATION
backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = dataset[, c(1,2,3,4,5)]
backwardElimination(training_set, SL)