### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    LINEAR DISCRIMINANT ANALYSIS (LDA) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
# install.packages("caTools")
# install.packages("e1071")
# install.packages("ElemStatLearn")
# install.packages("MASS")
library(e1071)
library(caTools)
library(ElemStatLearn)
library(MASS)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Dimensionality Reduction")
dataset <- read.csv("Wine.csv")

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Customer_Segment, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
training_set[ , -14] <- scale(training_set[ , -14])
test_set[ , -14] <- scale(test_set[ , -14])

#   APPLY LDA
lda <- lda(formula = Customer_Segment ~ .,
           data = training_set)
training_set <- as.data.frame(predict(lda, training_set))
training_set <- training_set[ , c(5, 6, 1)]
test_set <- as.data.frame(predict(lda, test_set))
test_set <- test_set[ , c(5, 6, 1)]

#   SVM FIT ON TRAINING SET
classifier <- svm(formula = class ~ .,
                  data = training_set,
                  type = "C-classification",
                  kernel = "linear")

#   SVM PREDICTION ON TEST SET
y_pred <- predict(classifier, newdata = test_set[ , -3])

#   CONFUSION MATRIX
cm <- table(test_set[ , 3], y_pred)
cm

#   VISUALIZE TRAINING SET
set <- training_set
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("x.LD1", "x.LD2")
y_grid <- predict(classifier, newdata = grid_set)
plot(set[ , -3],
     main = "SVM with LDA (Training set)",
     xlab = "LD1", ylab = "LD2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 2, "deepskyblue", ifelse(y_grid == 1, "springgreen3", "tomato")))
points(set, pch = 21, bg = ifelse(set[ , 3] == 2, "blue3", ifelse(set[ , 3] == 1, "green4", "red3")))

#   VISUALIZE TEST SET
set <- test_set
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('x.LD1', 'x.LD2')
y_grid <- predict(classifier, newdata = grid_set)
plot(set[ , -3],
     main = "SVM with LDA (Test set)",
     xlab = "LD1", ylab = "LD2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = 19, col = ifelse(y_grid == 2, "deepskyblue", ifelse(y_grid == 1, "springgreen3", "tomato")))
points(set, pch = 21, bg = ifelse(set[, 3] == 2, "blue3", ifelse(set[, 3] == 1, "green4", "red3")))