### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    HEIRARCHICAL CLUSTERING TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   3/24/2019

rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("cluster")
library(cluster)

#   SET THE DIRECTORY AND IMPORT DATASET
setwd("C:/Users/Kennv/Desktop/Machine Learning/Clustering")
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5]

#   TRAINING/TEST SET SPLIT
# since outcomes are unknown in unsupervised learning like clusters,
# the dataset does not need a training/test data

#   FEATURE SCALING
# training_set = scale(training_set)
# test_set = scale(test_set)

#   FINDING OPTIMAL NUMBER OF CLUSTERS USING ELBOW METHOD
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean Distances')

#   HEIRARCHICAL CLUSTERING FIT ON DATASET
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

#   VISUALIZE CLUSTERS
# this is only useable for visualizing in 2D
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')