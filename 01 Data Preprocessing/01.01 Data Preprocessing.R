### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    DATA PREPROCESSING TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/19/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
library(caTools)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Data Preprocessing")
dataset = read.csv('Data.csv')

#   HANDLE MISSING DATA (REPLACE WITH MEAN)
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = T)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = T)),
                        dataset$Salary)

#   ENCODE CATEGORICAL VARIABLES
dataset$Country = factor(dataset$Country,
                         levels = c("France", "Spain", "Germany"),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                         levels = c("No", "Yes"),
                         labels = c(0, 1))

#   TRAINING/TEST SET SPLIT
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set = subset(dataset, split == T)
test_set = subset(dataset, split == F)

#   FEATURE SCALING
training_set[, 2:3] = scale(training_set[, 2:3])
test_set[, 2:3] = scale(test_set[, 2:3])