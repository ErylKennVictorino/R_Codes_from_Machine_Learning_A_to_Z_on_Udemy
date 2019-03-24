### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    POLYNOMIAL REGRESSION TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/21/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("ggplot2")
library(ggplot2)

#   SET THE DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Regression")
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#   TRAINING/TEST SET SPLIT
# the dataset is too small to split into a train and test set

#   FEATURE SCALING
# we don't need to feature scale since the package used below automatically does it for us

#   LINEAR REGRESSION FIT ON DATASET (FOR COMPARISON)
lin_reg = lm(formula = Salary ~ .,
             data = dataset)
summary(lin_reg)

#   2ND DEGREE POLYNOMIAL REGRESSION FIT ON DATASET
dataset$Level2 = dataset$Level^2
poly_reg2 = lm(formula = Salary ~ .,
               data = dataset)
summary(poly_reg2)

#   3RD DEGREE POLYNOMIAL REGRESSION FIT ON DATASET
dataset$Level3 = dataset$Level^3
poly_reg3 = lm(formula = Salary ~ .,
               data = dataset)
summary(poly_reg3)

#   4TH DEGREE POLYNOMIAL REGRESSION FIT ON DATASET
dataset$Level4 = dataset$Level^4
poly_reg4 = lm(formula = Salary ~ .,
              data = dataset)
summary(poly_reg4)

#   VISUALIZE LINEAR REGRESSION (FOR COMPARISON)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Linear Regression)') +
  xlab('Level') +
  ylab('Salary')

# #   VISUALIZE 2ND DEGREE POLYNOMIAL REGRESSION (HIGH RES/SMOOTH CURVE)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg2,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (2nd Degree Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#   VISUALIZE 3RD DEGREE POLYNOMIAL REGRESSION (HIGH RES/SMOOTH CURVE)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg3,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (3rd Degree Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#   VISUALIZE 4TH DEGREE POLYNOMIAL REGRESSION (HIGH RES/SMOOTH CURVE)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(poly_reg4,
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (4th Degree Polynomial Regression)') +
  xlab('Level') +
  ylab('Salary')

#   LINEAR REGRESSION PREDICTION AT POSITION LEVEL "6.5"
predict(lin_reg, data.frame(Level = 6.5))

#   2ND DEGREE POLYNOMIAL REGRESSION PREDICTION AT POSITION LEVEL "6.5"
predict(poly_reg2, data.frame(Level = 6.5,
                             Level2 = 6.5^2))

#   3RD DEGREE POLYNOMIAL REGRESSION PREDICTION AT POSITION LEVEL "6.5"
predict(poly_reg3, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3))

#   4TH DEGREE POLYNOMIAL REGRESSION PREDICTION AT POSITION LEVEL "6.5"
predict(poly_reg4, data.frame(Level = 6.5,
                             Level2 = 6.5^2,
                             Level3 = 6.5^3,
                             Level4 = 6.5^4))