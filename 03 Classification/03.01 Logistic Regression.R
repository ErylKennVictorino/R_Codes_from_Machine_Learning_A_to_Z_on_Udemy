### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    LOGISTIC REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/23/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("ElemStatLearn")
library(caTools)
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

#   LOGISTIC REGRESSION FIT ON TRAINING SET
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

#   LOGISTIC REGRESSION PREDICTION ON TEST SET
prob_pred = predict(classifier, type = 'response', newdata = test_set[ , 1:2])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

#   CONFUSION MATRIX
cm = table(test_set[, 3], y_pred > 0.5)
cm

#   VISUALIZE TRAINING SET
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, 1:2],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#   VISUALIZE TEST SET
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, 1:2],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))