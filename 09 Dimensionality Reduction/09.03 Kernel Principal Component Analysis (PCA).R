### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    KERNEL PRINCIPAL COMPONENT ANALYSIS (KERNEL SVM) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("caTools")
#install.packages("kernlab")
#install.packages("ElemStatLearn")
#install.packages("pROC")
library(caTools)
library(kernlab)
library(ElemStatLearn)
library(pROC)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Dimensionality Reduction")
dataset <- read.csv("Social_Network_Ads.csv")
dataset <- dataset[ , 3:5]

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
training_set[ , 1:2] <- scale(training_set[ , 1:2])
test_set[ , 1:2] <- scale(test_set[ , 1:2])

#   APPLY KERNEL PCA
kpca = kpca(~., data <- training_set[ , -3], kernel = 'rbfdot', features = 2)
training_set_pca <- as.data.frame(predict(kpca, training_set))
training_set_pca$Purchased <- training_set$Purchased
test_set_pca <- as.data.frame(predict(kpca, test_set))
test_set_pca$Purchased <- test_set$Purchased

#   LOGISTIC REGRESSION FIT ON TRAINING SET     
classifier <- glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set_pca)

#   LOGISTIC REGRESSION PREDICTION ON TEST SET
y_prob <- predict(classifier, type = "response", newdata = test_set_pca[ , -3])
y_pred <- ifelse(y_prob > 0.5, 1, 0)

#   CONFUSION MATRIX
cm <- table(test_set_pca[ , 3], y_pred)
cm

#   ROC AND AUC
lgr_roc <- roc(factor(test_set_pca[ , 3]), as.vector(y_prob))
lgr_auc <- auc(factor(test_set_pca[ , 3]), as.vector(y_prob))
plot(lgr_roc, col = "red", main = "Receiver Operating Characteristic (ROC)",
     xaxs = "i", yaxs = "i")
legend("bottomright", title = "AUC",
       paste("Logistic Regression with Kernel PCA = ", round(lgr_auc, 2)),
       fill = "red")

#   VISUALIZE TRAINING SET
set <- training_set_pca
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("V1", "V2")
prob_set <- predict(classifier, type = "response", newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[ , -3],
     main = "Logistic Regression with Kernel PCA (Training set)",
     xlab = "PC1", ylab = "PC2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[ , 3] == 1, "green4", "red3"))

#   VISUALIZE TEST SET
set <- test_set_pca
X1 <- seq(min(set[ , 1]) - 1, max(set[ , 1]) + 1, by = 0.01)
X2 <- seq(min(set[ , 2]) - 1, max(set[ , 2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("V1", "V2")
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5, 1, 0)
plot(set[ , -3],
     main = "Logistic Regression with Kernel PCA (Test set)",
     xlab = "PC1", ylab = "PC2",
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = T)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, "springgreen3", "tomato"))
points(set, pch = 21, bg = ifelse(set[ , 3] == 1, "green4", "red3"))