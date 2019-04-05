### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    NATURAL LANGUAGE PROCESSING (NLP) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/4/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("caTools")
#install.packages("class")
#install.packages("e1071")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("ROCR")
library(tm)
library(SnowballC)
library(caTools)
library(class)
library(e1071)
library(rpart)
library(randomForest)
library(ROCR)

#   SET DIRECTORY AND IMPORT DATASET
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Natural Language Processing")
dataset_original <- read.delim('Restaurant_Reviews.tsv',
                              quote = '',
                              stringsAsFactors = FALSE)

#   CLEAN TEXT
corpus <- VCorpus(VectorSource(dataset_original$Review))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords())
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)

#   BAG OF WORDS MODEL
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.999)
dataset <- as.data.frame(as.matrix(dtm))
dataset$Liked <- dataset_original$Liked

#   ENCODE CATEGORICAL VARIABLES
dataset$Liked <- factor(dataset$Liked, levels = c(0, 1))

#   TRAINING/TEST SPLIT
set.seed(123)
split <- sample.split(dataset$Liked, SplitRatio = 0.8)
training_set <- subset(dataset, split == T)
test_set <- subset(dataset, split == F)

#   FEATURE SCALING
# unnecessary since the values are mostly zeroes and ones

#   PERFORMANCE CALCULATORS
accuracy_calculator <- function(cm) {
  TP = cm[2, 2]
  TN = cm[1, 1]
  FP = cm[1, 2]
  FN = cm[2, 1]
  accuracy = (TP + TN) / (TP + TN + FP + FN)
  return(accuracy)
}
precision_calculator <- function(cm) {
  TP = cm[2, 2]
  FP = cm[1, 2]
  precision = TP / (TP + FP)
  return(precision)
}
recall_calculator <- function(cm) {
  TP = cm[2, 2]
  FN = cm[2, 1]
  recall = TP / (TP + FN)
  return(recall)
}
f1_calculator <- function(cm) {
  TP = cm[2, 2]
  FP = cm[1, 2]
  FN = cm[2, 1]
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  f1 = 2 * precision * recall / (precision + recall)
  return(f1)
}

#   CREATE PERFORMANCE TABLE
performance <- data.frame(matrix(ncol = 4, nrow = 6))
colnames(performance) <- c('accuracy', 'precision', 'recall', 'f1')
rownames(performance) <- c('Logistic Regression', 'K-Nearest Neighbors',
                          'Support Vector Machine', 'Naive Bayes',
                          'Decision Tree', 'Random Forest')

#   LOGISTIC REGRESSION FIT, ROC, AND AUC
lg_classifier <- glm(formula = Liked ~ .,
                    family = binomial,
                    data = training_set)
lg_prob_pred <- predict(lg_classifier, type = 'response', newdata = test_set[ , -692])
lg_y_pred <- ifelse(lg_prob_pred > 0.5, 1, 0)
lg_cm <- as.matrix(table(test_set[ , 692], lg_y_pred > 0.5))
performance['Logistic Regression', 'accuracy'] <- accuracy_calculator(lg_cm)
performance['Logistic Regression', 'precision'] <- precision_calculator(lg_cm)
performance['Logistic Regression', 'recall'] <- recall_calculator(lg_cm)
performance['Logistic Regression', 'f1'] <- f1_calculator(lg_cm)
lg_prediction <- prediction(lg_y_pred, test_set[ , 692])
lg_perf <- performance(lg_prediction,"tpr","fpr")
lg_auc <- auc(test_set[ , 692], as.numeric(lg_y_pred))

#   KNN FIT, ROC, AND AUC
knn_y_pred <- knn(train = training_set[ , -692],
                 test = test_set[ , -692],
                 cl = training_set[ , 692],
                 k = 5,
                 prob = TRUE)
knn_cm <- table(test_set[ , 692], knn_y_pred)
performance['K-Nearest Neighbors', 'accuracy'] <- accuracy_calculator(knn_cm)
performance['K-Nearest Neighbors', 'precision'] <- precision_calculator(knn_cm)
performance['K-Nearest Neighbors', 'recall'] <- recall_calculator(knn_cm)
performance['K-Nearest Neighbors', 'f1'] <- f1_calculator(knn_cm)
knn_prediction <- prediction(as.numeric(knn_y_pred), test_set[ , 692])
knn_perf <- performance(knn_prediction,"tpr","fpr")
knn_auc <- auc(test_set[ , 692], as.numeric(knn_y_pred))

#   SVM FIT, ROC, AND AUC
svm_classifier <- svm(formula = Liked ~ .,
                     data = training_set,
                     type = 'C-classification',
                     kernel = 'linear')
svm_y_pred <- predict(svm_classifier, newdata = test_set[ , -692])
svm_cm <- table(test_set[ , 692], svm_y_pred)
performance['Support Vector Machine', 'accuracy'] <- accuracy_calculator(svm_cm)
performance['Support Vector Machine', 'precision'] <- precision_calculator(svm_cm)
performance['Support Vector Machine', 'recall'] <- recall_calculator(svm_cm)
performance['Support Vector Machine', 'f1'] <- f1_calculator(svm_cm)
svm_prediction <- prediction(as.numeric(svm_y_pred), test_set[ , 692])
svm_perf <- performance(svm_prediction,"tpr","fpr")
svm_auc <- auc(test_set[ , 692], as.numeric(svm_y_pred))

#   NAIVE BAYES FIT, ROC, AND AUC
nb_classifier <- naiveBayes(x = training_set[ , -692],
                           y = training_set$Liked)
nb_y_pred <- predict(nb_classifier, newdata = test_set[ , -692])
nb_cm <- table(test_set[ , 692], nb_y_pred)
performance['Naive Bayes', 'accuracy'] <- accuracy_calculator(nb_cm)
performance['Naive Bayes', 'precision'] <- precision_calculator(nb_cm)
performance['Naive Bayes', 'recall'] <- recall_calculator(nb_cm)
performance['Naive Bayes', 'f1'] <- f1_calculator(nb_cm)
nb_prediction <- prediction(as.numeric(nb_y_pred), test_set[ , 692])
nb_perf <- performance(nb_prediction,"tpr","fpr")
nb_auc <- auc(test_set[ , 692], as.numeric(nb_y_pred))

#   DECISION TREE FIT, ROC, AND AUC
dt_classifier <- rpart(formula = Liked ~ .,
                   data = training_set)
dt_y_pred <- predict(dt_classifier, newdata = test_set[, -692], type = 'class')
dt_cm <- table(test_set[ , 692], dt_y_pred)
performance['Decision Tree', 'accuracy'] <- accuracy_calculator(dt_cm)
performance['Decision Tree', 'precision'] <- precision_calculator(dt_cm)
performance['Decision Tree', 'recall'] <- recall_calculator(dt_cm)
performance['Decision Tree', 'f1'] <- f1_calculator(dt_cm)
dt_prediction <- prediction(as.numeric(dt_y_pred), test_set[ , 692])
dt_perf <- performance(dt_prediction,"tpr","fpr")
dt_auc <- auc(test_set[ , 692], as.numeric(dt_y_pred))

#   RANDOM FOREST FIT, ROC, AND AUC
rf_classifier <- randomForest(x = training_set[ , -692],
                             y = training_set$Liked,
                             ntree = 10)
rf_y_pred <- predict(rf_classifier, newdata = test_set[ , -692])
rf_cm <- table(test_set[ , 692], rf_y_pred)
performance['Random Forest', 'accuracy'] <- accuracy_calculator(rf_cm)
performance['Random Forest', 'precision'] <- precision_calculator(rf_cm)
performance['Random Forest', 'recall'] <- recall_calculator(rf_cm)
performance['Random Forest', 'f1'] <- f1_calculator(rf_cm)
rf_prediction <- prediction(as.numeric(rf_y_pred), test_set[ , 692])
rf_perf <- performance(rf_prediction,"tpr","fpr")
rf_auc <- auc(test_set[ , 692], as.numeric(rf_y_pred))

#   PERFORMANCE TABLE
performance

#   ROC AND AUC COMPARISONS FOR ALL CLASSIFIERS
plot(lg_perf, col = "red", main = "Receiver Operating Characteristic (ROC)",
     xaxs = "i", yaxs = "i")
abline(c(0, 0), c(1, 1), lty = 3)
plot(knn_perf, col = "darkorange", add = T)
plot(svm_perf, col = "darkviolet", add = T)
plot(nb_perf, col = "blue", add = T)
plot(dt_perf, col = "lightgreen", add = T)
plot(rf_perf, col = "darkgreen", add = T)
legend("bottomright", title = "AUC",
       c(paste('Logistic Regression = ', lg_auc),
         paste('K-Nearest Neighbors = ', knn_auc),
         paste('Support Vector Machine = ', svm_auc),
         paste('Naive Bayes = ', nb_auc),
         paste('Decision Tree = ', dt_auc),
         paste('Random Forest = ', rf_auc)),
       fill = c("red", "darkorange", "darkviolet", "blue", "lightgreen", "darkgreen"))