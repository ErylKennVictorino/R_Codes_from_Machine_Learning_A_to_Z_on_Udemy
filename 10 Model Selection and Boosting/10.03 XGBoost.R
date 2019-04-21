### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    XGBOOST TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("xgboost")
#install.packages("caret")
#install.packages("pROC")
library(caTools)
library(xgboost)
library(caret)
library(pROC)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Model Selection")
dataset <- read.csv("Churn_Modelling.csv")
dataset <- dataset[4:14]

#   ENCODE CATEGORICAL VARIABLES
dataset$Geography <- as.numeric(factor(dataset$Geography,
                                      levels = c("France", "Spain", "Germany"),
                                      labels = c(1, 2, 3)))
dataset$Gender <- as.numeric(factor(dataset$Gender,
                                   levels = c("Female", "Male"),
                                   labels = c(1, 2)))

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
# since XGBoost is gradient boosting mode with decision trees, feature scaling is unnecessary

#   XGBOOST FIT ON TRAINING SET
classifier <- xgboost(data = as.matrix(training_set[ , -11]),
                      label = training_set$Exited,
                      nrounds = 10)

#   XGBOOST PREDICTION ON TEST SET
y_prob <- predict(classifier, newdata = as.matrix(test_set[ , -11]))
y_pred <- (y_prob >= 0.5)

#   CONFUSION MATRIX
cm <- table(test_set[, 11], y_pred)

#   ROC AND AUC
xgb_roc <- roc(factor(test_set[, 11]), as.vector(y_prob))
xgb_auc <- auc(factor(test_set[, 11]), as.vector(y_prob))
plot(xgb_roc, col = "red", main = "Receiver Operating Characteristic (ROC)",
     xaxs = "i", yaxs = "i")
legend("bottomright", title = "AUC",
       paste("XGBoost = ", round(xgb_auc, 2)),
       fill = "red")

#   APPLY K-FOLD CROSS VALIDATION
# 10 folds is standard
folds = createFolds(training_set$Exited, k = 10)
cv <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- xgboost(data = as.matrix(training_set[-11]), label = training_set$Exited, nrounds = 10)
  y_pred <- predict(classifier, newdata = as.matrix(test_fold[-11]))
  y_pred <- (y_pred >= 0.5)
  cm <- table(test_fold[, 11], y_pred)
  accuracy <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))