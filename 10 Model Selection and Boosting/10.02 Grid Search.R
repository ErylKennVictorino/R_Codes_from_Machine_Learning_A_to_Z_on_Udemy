### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    GRID SEARCH TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("e1071")
#install.packages("caret")
#install.packages("ElemStatLearn")
#install.packages("pROC")
library(caTools)
library(e1071)
library(caret)
library(ElemStatLearn)
library(pROC)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Model Selection")
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[3:5]

#   ENCODE CATEGORICAL VARIABLES
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
training_set[ , -3] <- scale(training_set[ , -3])
test_set[ , -3] <- scale(test_set[ , -3])

#   APPLY GRID SEARCH
classifier <- train(form = Purchased ~ .,
                    data = training_set,
                    method = "svmRadial")
classifier
classifier$bestTune

#   KERNEL SVM FIT ON TRAINING SET
classifier <- svm(formula = Purchased ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "radial",
                  sigma = 1.560428, 
                  C = 1,
                  probability = T)

#   KERNEL SVM PREDICTION ON TEST SET
y_pred <- predict(classifier, newdata = test_set[ , -3], prob = T)
y_prob <- attr(y_pred, "prob")[ , 1]

#   CONFUSION MATRIX
cm <- table(test_set[ , 3], y_pred)

#   ROC AND AUC
ksvm_roc <- roc(factor(test_set[, 3]), as.vector(y_prob))
ksvm_auc <- auc(factor(test_set[, 3]), as.vector(y_prob))
plot(ksvm_roc, col = "red", main = "Receiver Operating Characteristic (ROC)",
     xaxs = "i", yaxs = "i")
legend("bottomright", title = "AUC",
       paste("Kernel SVM with Grid Search = ", round(ksvm_auc, 2)),
       fill = "red")

#   APPLY K-FOLD CROSS VALIDATION
# 10 folds is standard
folds <- createFolds(training_set$Purchased, k = 10)
cv <- lapply(folds, function(x) {
  training_fold <- training_set[-x, ]
  test_fold <- training_set[x, ]
  classifier <- svm(formula = Purchased ~ .,
                    data = training_fold,
                    type = "C-classification",
                    kernel = "radial")
  y_pred <- predict(classifier, newdata = test_fold[ , -3])
  cm <- table(test_fold[, 3], y_pred)
  accuracy <- (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[1, 2] + cm[2, 1])
  return(accuracy)
})
accuracy <- mean(as.numeric(cv))
accuracy

#   VISUALIZE TRAINING SET
set <- training_set
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[ , -3],
     main = "Kernel SVM (Training set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[ , 3] == 1, "green4", "red3"))

#   VISUALIZE TEST SET
set <- test_set
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("Age", "EstimatedSalary")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[ , -3], main = "Kernel SVM (Test set)",
     xlab = "Age", ylab = "Estimated Salary",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[ , 3] == 1, "green4", "red3"))