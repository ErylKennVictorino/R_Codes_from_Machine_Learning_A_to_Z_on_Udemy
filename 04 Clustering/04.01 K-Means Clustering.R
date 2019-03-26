### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    K-MEANS CLUSTERING TUTORIAL
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
# we don't need to feature scale since the regression library we use below automatically does it for us

#   FINDING OPTIMAL NUMBER OF CLUSTERS USING ELBOW METHOD
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

#   K-MEANS CLUSTERING FIT ON DATASET
set.seed(29)
kmeans = kmeans(x = dataset, centers = 5, iter.max = 300, nstart = 10)
y_kmeans = kmeans$cluster

#   VISUALIZE CLUSTERS
# this is only useable for visualizing in 2D
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of Customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')