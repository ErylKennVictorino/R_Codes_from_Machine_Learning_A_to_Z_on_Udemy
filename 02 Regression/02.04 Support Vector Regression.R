### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    SUPPORT VECTOR REGRESSION (SVR) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/22/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("e1071")
#install.packages("ggplot2")
library(e1071)
library(ggplot2)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Regression")
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#   TRAINING/TEST SET SPLIT
# the dataset is too small to split into a train and test set

#   FEATURE SCALING
# we don't need to feature scale since the package used below automatically does it for us

#   SVR FIT ON DATASET
regressor = svm(formula = Salary ~ .,
                data = dataset,
                type = 'eps-regression',
                kernel = 'radial')

#   SVR PREDICTION AT POSITION LEVEL "6.5"
y_pred = predict(regressor, data.frame(Level = 6.5))
y_pred

#   VISUALIZE SVR
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')

#   VISUALIZE SVR (HIGH RES/SMOOTH CURVE)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR)') +
  xlab('Level') +
  ylab('Salary')