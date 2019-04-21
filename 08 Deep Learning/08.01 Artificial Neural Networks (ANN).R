### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    ARTIFICIAL NEURAL NETWORKS (ANN) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("h2o")
#install.packages("pROC")
library(caTools)
library(h2o)
library(pROC)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Deep Learning")
dataset <- read.csv("Churn_Modelling.csv")
dataset <- dataset[ , 4:14]

#   ENCODE CATEGORICAL VARIABLES
dataset$Geography <- as.numeric(factor(dataset$Geography,
                                       levels = c("France", "Spain", "Germany"),
                                       labels = c(1, 2, 3)))
dataset$Gender <- as.numeric(factor(dataset$Gender,
                                    levels = c("Female", "Male"),
                                    labels = c(1, 2)))

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
training_set[ , -11] <- scale(training_set[ , -11])
test_set[ , -11] <- scale(test_set[ , -11])

#   ANN FIT ON TRAINING SET
h2o.init(nthreads = -1)
# to get number of nodes for hidden layers,
# take the average of the number of nodes in the input layer (11)
# and output layer(1) = (11+1)/2 = 6
# use a "Rectifier" activation function for the hidden layers
model <- h2o.deeplearning(y = "Exited",
                         training_frame = as.h2o(training_set),
                         activation = "Rectifier",
                         hidden = c(6,6),
                         epochs = 100,
                         train_samples_per_iteration = -2)

#   PREDICTION ON TEST SET
y_prob <- h2o.predict(model, newdata = as.h2o(test_set[ , -11]))
y_pred <- (y_prob > 0.5)
y_pred <- as.vector(y_pred)


#   CONFUSION MATRIX
cm <- table(test_set[ , 11], y_pred)

#   ROC AND AUC
ann_roc <- roc(factor(test_set[ , 11]), as.vector(y_prob))
ann_auc <- auc(factor(test_set[ , 11]), as.vector(y_prob))
plot(ann_roc, col = "red", main = "Receiver Operating Characteristic (ROC)",
     xaxs = "i", yaxs = "i")
legend("bottomright", title = "AUC",
       paste("Artificial Neural Network = ", round(ann_auc, 2)),
       fill = "red")

h2o.shutdown()