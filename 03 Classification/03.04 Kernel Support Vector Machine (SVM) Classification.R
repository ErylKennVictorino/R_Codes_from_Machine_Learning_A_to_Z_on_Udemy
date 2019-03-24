### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    KERNEL SUPPORT VECTOR MACHINE (SVM) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/23/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("e1071")
#install.packages("ElemStatLearn")
library(caTools)
library(e1071)
library(ElemStatLearn)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Classification")
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

#   ENCODE CATEGORICAL VARIABLES
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

#   TRAINING/TEST SPLIT
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

#   FEATURE SCALING
training_set[ , 1:2] = scale(training_set[ , 1:2])
test_set[ , 1:2] = scale(test_set[ , 1:2])

#   KERNEL SVM FIT ON TRAINING SET
classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

#   KERNEL SVM PREDICTION ON TEST SET
y_pred = predict(classifier, newdata = test_set[1:2])

#   CONFUSION MATRIX
cm = table(test_set[, 3], y_pred)

#   VISUALIZE TRAINING SET
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, 1:2],
     main = 'Kernel SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#   VISUALIZE TEST SET
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, 1:2], main = 'Kernel SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))