### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    RANDOM FOREST REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/23/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("randomForest")
#install.packages("ggplot2")
library(randomForest)
library(ggplot2)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Regression")
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#   TRAINING/TEST SET SPLIT
# the dataset is too small to split into a train and test set

#   FEATURE SCALING
# not necessary for decision trees because the model is built based on the conditions on
# the independent variable and not euclidean distances

#   RANDOM FOREST REGRESSION FIT ON DATASET
set.seed(1234)
regressor = randomForest(x = dataset[1],
                         y = dataset$Salary,
                         ntree = 500)

#   RANDOM FOREST REGRESSION PREDICTION AT POSITION LEVEL "6.5"
y_pred = predict(regressor, data.frame(Level = 6.5))
y_pred

#   VISUALIZE RANDOM FOREST REGRESSION (HIGH RES)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')