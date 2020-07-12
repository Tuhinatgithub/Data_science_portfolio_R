
#Loading required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(kernlab)
library(gridExtra)
install.packages('e1071', dependencies=TRUE)

# Loading the data

xponent <- read_csv("physician_prescriptions.csv",col_names = TRUE, set.seed(1))

#Summarise the data to get an overview
summary(xponent)

#Change necessary column types
as.numeric(xponent$nrx)
as.numeric(xponent$trx)
as.numeric(xponent$physician_id)

#Checking for NA values
sapply(xponent, function(x) sum(is.na(x)))

#K-means clustering for Customer/Doctor segmentation
physicians_clusters <- kmeans(xponent, centers = 3, nstart = 1)

#Loading clusters into xponent dataframe
xponent$cluster <- physicians_clusters$cluster

#Get the latest summary of Data
summary(xponent)

#Cluster wise new prescription count
xponent %>%
  group_by(cluster) %>%
  summarise(mean_nrx = mean(nrx))

#Cluster wise total prescription count
xponent %>%
  group_by(cluster) %>%
  summarise(mean_trx = mean(trx))

