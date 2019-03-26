### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    ECLAT TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/26/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages('arules')
library(arules)

#   SET THE DIRECTORY AND IMPORT DATASET
setwd("C:/Users/Kennv/Desktop/Machine Learning/Association Rule Learning")
dataset = read.csv('Market_Basket_Optimisation.csv', header = F)
# arules package does not take this dataset as an input, we need to make a sparse matrix

#   SPARSE MATRIX
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

#   ECLAT TRAINING ON DATASET
# to get the support, we choose items bought 3 times a day
# support = (3 buys)*(7 days in a week)/(7500 total sales)
rules = eclat(data = dataset, parameter = list(support = 0.004, minlen = 2))

#   VISUALIZE RULES
inspect(sort(rules, by = 'support')[1:10])