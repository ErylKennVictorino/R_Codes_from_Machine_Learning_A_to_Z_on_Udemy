### AUTHOR:     ERYL KENN VICTORINO
### PURPOSE:    CONVOLUTIONAL NEURAL NETWORKS (CNN) TUTORIAL
###             from 'Machine Learning A-Z: Hands-On Python & R In Data Science' on Udemy
###             by Kirill Eremenko, Hadelin de Ponteves, and the SuperDataScience Team
### MOD DATE:   4/20/2019

#   CLEAR WORKSPACE
rm(list = ls())

#   INSTALL PACKAGES AND LOAD LIBRARIES
#install.packages("tensorflow")
#install.packages("keras")
library(tensorflow)
install_tensorflow()
library(keras)
install_keras()


#   SET DIRECTORY
#setwd("C:/Users/Kennv/Desktop/Machine Learning/Deep Learning")

#   IMPORT DATASET
#pull training directory and rescale and format image 
training_set <- flow_images_from_directory("dataset/training_set",
                                           generator = image_data_generator(rescale = 1./255,
                                                                            shear_range = 0.2,
                                                                            zoom_range = 0.2,
                                                                            horizontal_flip = T),
                                           target_size = c(64, 64), 
                                           class_mode = "binary", 
                                           batch_size = 32,
                                           seed = 123)
#pull test directory and rescale and format image
test_set <- flow_images_from_directory("dataset/test_set",
                                       generator = image_data_generator(rescale = 1./255,
                                                                        shear_range = 0.2,
                                                                        zoom_range = 0.2,
                                                                        horizontal_flip = T),
                                       target_size = c(64, 64), 
                                       classes = NULL,
                                       class_mode = "binary", 
                                       batch_size = 32, 
                                       seed = 123)

#   INITIALIZE CNN

#   SEQUENTIAL MODEL
model <- keras_model_sequential() %>%
  #   FIRST 2D CONVOLUTION
  layer_conv_2d(filter = 32,
                kernel_size = c(3, 3),
                input_shape = c(64, 64, 3),
                activation = "relu") %>%
  #   FIRST POOLING
  layer_max_pooling_2d(pool_size = c(2, 2))%>% 
  #   SECOND CONVOLUTION
  layer_conv_2d(filter = 32,
                kernel_size = c(3, 3),
                activation = "relu") %>% 
  #   SECOND POOLING
  layer_max_pooling_2d(pool_size = c(2, 2))%>%
  #Flatten
  layer_flatten()%>% 
  #   FULL CONNECTION
  layer_dense(units = 128,
              activation = "relu") %>%  
  layer_dense(units = 1,
              activation = "sigmoid") %>%
  #   COMPILE CNN
  compile(optimizer = "adam",
          loss = "binary_crossentropy",
          metrics = c('accuracy')) 

#   SUMMARY
summary(model) 

#   CNN FIT ON TRAINING SET
fit_model <- model %>% fit_generator(
  training_set,
  validation_data = test_set,
  epochs = 25,
  steps_per_epoch = 250
)