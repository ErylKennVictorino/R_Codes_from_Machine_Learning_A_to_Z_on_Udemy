### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    DECISION TREE REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/22/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("rpart")
#install.packages("ggplot2")
library(rpart)
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

#   DECISION TREE REGRESSION FIT ON DATASET
regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))

#   DECISION TREE REGRESSION PREDICTION AT POSITION LEVEL "6.5"
y_pred = predict(regressor, data.frame(Level = 6.5))
y_pred

#   VISUALIZE DECISION TREE REGRESSION (HIGH RES)
# you must reshape the X because this is non-linear AND non-continuous
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')

#   PLOT DECISION TREE
# re-execute the code wihtout feature scaling to plot the decision tree
plot(regressor)
text(regressor)