### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    SIMPLE LINEAR REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/19/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("ggplot2")
library(caTools)
library(ggplot2)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Regression")
dataset = read.csv('Salary_Data.csv')

#   TRAINING/TEST SET SPLIT
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

#   FEATURE SCALING
# we don't need to feature scale since the package used automatically does it for us

#   SIMPLE LINEAR REGRESSION FIT ON TRAINING SET
regressor = lm(formula = Salary ~ YearsExperience,
               data = training_set)
summary(regressor)

#   SIMPLE LINEAR REGRESSION PREDICTION ON TRAINING SET AND TEST SET
y_train_pred = predict(regressor, newdata = training_set)
y_test_pred = predict(regressor, newdata = test_set)

#   VISUALIZE TRAINING SET
ggplot() +
  geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = y_train_pred),
            color = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Years of experience') +
  ylab('Salary')

#   VISUALIZE TEST SET
ggplot() +
  geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
             color = 'red') +
  geom_line(aes(x = training_set$YearsExperience, y = y_train_pred),
            color = 'blue') +
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')