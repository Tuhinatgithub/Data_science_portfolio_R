
#------------------------------------- Author - Tuhin Pal --------------------------------------------#

#---------- Handwritten Digit Recognition -----------#

# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Exploratory Data Analysis
# 5. Model Building & Evaluation
#   5.1 Linear Kernel Model
#     5.1.1 Using C Value = 1
#     5.1.2 Using C Value = 10
#     5.1.3 Cross validation to optimise C
#     5.1.4 Observation : Best Accuracy at C=0.1
#   5.2 RBF kernel Model
#     5.2.1 RBF kernel with default values
#     5.2.2 RBF kernel with higher sigma
#     5.2.3 Cross validation to optimise C and sigma
#     5.2.4 Observation : Best Accuracy at C=3 and Sigma=0.01
#   5.3 Polynomial Modelling
#     5.3.1 Polynomial Model with degree=2, default scale=1 and offset=1
#     5.3.2 Polynomial Model with degree=2, scale=-2 and offset=1
#     5.3.3 Polynomial Model with degree=2, scale=1 and offset=10
#     5.3.4 Polynomial Model with degree=2, scale=1, offset=1, C=3
#     5.3.5 Grid search to optimise hyperparameters
#     5.3.6 Observation : Best Accuracy at C = 0.01, degree = 2, scale = 1
#   5.4 Optmised polynomial model
# 6. Conclusion

#================ Business Understanding ====================# 

#A classic problem in the field of pattern recognition is that of handwritten digit recognition.
#Suppose that you have an image of a digit submitted by a user via a scanner, 
#a tablet, or other digital devices.
#The goal is to develop a model that can correctly identify the digit (between 0-9) 
#written in an image.

#============= Data Understanding ==============#

#Loading libraries

library(ggplot2)
library(kernlab)
library(caret)
library(caTools)
library(gridExtra)

# Loading data

mnist_train <- read.csv("mnist_train.csv", stringsAsFactors = F, header = F)
mnist_test <- read.csv("mnist_test.csv", stringsAsFactors = F, header = F)

View(mnist_train) # Data has no column names
View(mnist_test) # Data has no column names

names(mnist_test)[1] <- "label"
names(mnist_train)[1] <- "label"

# Checking missing value
sapply(mnist_train, function(x) sum(is.na(x))) # No missing values
sapply(mnist_test, function(x) sum(is.na(x))) # No missing values


#============= Data preparation ==============#

# Convert label variable into factor

mnist_train$label <- factor(mnist_train$label)
summary(mnist_train$label)

mnist_test$label <- factor(mnist_test$label)
summary(mnist_test$label)

# Sampling training dataset

set.seed(100)
sample_indices <- sample(1: nrow(mnist_train), 5000) # extracting subset of 5000 samples for modelling
train <- mnist_train[sample_indices, ]

# Scaling data 

max(train[ ,2:ncol(train)]) # max pixel value is 255, lets use this to scale data
train[ , 2:ncol(train)] <- train[ , 2:ncol(train)]/255

test <- cbind(label = mnist_test[ ,1], mnist_test[ , 2:ncol(mnist_test)]/255)


#=========== Exploratory Data Analysis ===========#


## Distribution of digits across all data sets

plot1 <- ggplot(mnist_train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "mnist_train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot2 <- ggplot(train, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

plot3 <- ggplot(test, aes(x = label, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "test dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))

grid.arrange(plot1, plot2, plot3, nrow = 3)

# Relative frequencies of the digits has been retained while sampling to create the reduced train data set
# Similar frequency in test dataset also observed


#=============== Model Building & Evaluation =================#

#=========== Linear model using C=1 =============#

## Linear kernel model using default parameters

model1_linear <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 1)
print(model1_linear) 

eval1_linear <- predict(model1_linear, newdata = test, type = "response")
confusionMatrix(eval1_linear, test$label) 

#-------------- Observations ----------------#

# Overall accuracy of 91.3%
# Specificities quite high > 99%
# Sensitivities good > 84%


#============== Linear model using C=10 ==============#

model2_linear <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "vanilladot", C = 10)
print(model2_linear) 

eval2_linear <- predict(model2_linear, newdata = test, type = "response")
confusionMatrix(eval2_linear, test$label) 

#------------ Observations ----------------#
# Overall accuracy of 91%
# Model performance has slightly decreased, model may be overfitting


## Using cross validation to optimise C value

grid_linear <- expand.grid(C= c(0.001, 0.1 ,1 ,10 ,100)) # defining range of C

fit.linear <- train(label ~ ., data = train, metric = "Accuracy", method = "svmLinear",
                    tuneGrid = grid_linear, preProcess = NULL,
                    trControl = trainControl(method = "cv", number = 5))

# Results of 5 cross validation
print(fit.linear) 
plot(fit.linear)

#------------ Observations --------------#

# C = 0.1 gives best accuracy of 92% 
# For higher values of C model is overfitting and for lower values model is simple

eval_cv_linear <- predict(fit.linear, newdata = test)
confusionMatrix(eval_cv_linear, test$label)

#-------------- Observations -----------------#

# Overall accuracy of 92.4%, slightly imporved
# Specificities quite high > 99%
# Sensitivities > 86%, improved from model1 by making model more generic i.e. lower C 


#============= RBF Modelling =============#

#=========== RBF kernel with default values ================#

model1_rbf <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot", C = 1, kpar = "automatic")
print(model1_rbf) 

eval1_rbf <- predict(model1_rbf, newdata = test, type = "response")
confusionMatrix(eval1_rbf, test$label) 

#------------- Observations ------------#

# Overall accuracy of 95%
# Specificities quite high > 99%
# Sensitivities high > 92%
# Increase in overall accuracy and sensitivty from linear kernel using C = 1, sigma = 0.0107
# data seems to have non linearity to it


#=========== RBF kernel with higher sigma ================#

model2_rbf <- ksvm(label ~ ., data = train, scaled = FALSE, kernel = "rbfdot",
                   C = 1, kpar = list(sigma = 1))
print(model2_rbf) 

eval2_rbf <- predict(model2_rbf, newdata = test, type = "response")
confusionMatrix(eval2_rbf, test$label) 

#-------------- Observations -----------------#

# Accuracy drops to 11% and class wise results are very poor
# sigma = 1 is too much non linearity and the model is overfitting


## Cross validation to optimise C and sigma

# Defining ranges of C and sigma
grid_rbf = expand.grid(C= c(0.01, 0.1, 1, 5, 10), sigma = c(0.001, 0.01, 0.1, 1, 5)) 

# Using only 2 folds to optimise run time
fit.rbf <- train(label ~ ., data = train, metric = "Accuracy", method = "svmRadial",tuneGrid = grid_rbf,
                 trControl = trainControl(method = "cv", number = 2), preProcess = NULL)

# Results of 2 cross validation
print(fit.rbf) 
plot(fit.rbf)

#-------------- Observations ---------------#

# Best sigma value is ~ 0.01
# Higher sigma values are overfitting and lower sigma values are not capturing non linearity adequately
# Accuracy increases with C until 5 and then decreases again, can be further optimised

## Optimising C further
grid_rbf = expand.grid(C= c(1,2, 3, 4, 5, 6 ,7, 8, 9, 10), sigma = 0.01)

fit.rbf2 <- train(label ~ ., data = train, metric = "Accuracy", method = "svmRadial",tuneGrid = grid_rbf,
                  trControl = trainControl(method = "cv", number = 5), preProcess = NULL)

# Results of cross validation
print(fit.rbf2) 
plot(fit.rbf2)

eval_cv_rbf <- predict(fit.rbf2, newdata = test)
confusionMatrix(eval_cv_rbf, test$label)

#-------------- Observations ---------------#

# Accuracy is highest at C = 3 and sigma = 0.01
# Higher C values are overfitting and lower C values have too much bias
# Accuracy of 96%
# High Sensitivities > 92%
# Very High Specificities > 99%


#=============== Polynomial Modelling ================#

#========= Polynomial Model with degree=2, default scale=1 and offset=1 ==============#

model1_poly <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(model1_poly)

eval1_poly <- predict(model1_poly, newdata = test)
confusionMatrix(eval1_poly, test$label)

#---------------- Observations ----------------#

# Good accuracy of 95.24%
# High Sensitivities > 92% and specificities > 99%
# Similar performance to radial kernel


#======= Polynomial Model with degree=2, scale=-2 and offset=1 =======#

model2_poly <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = -2, offset = 1))
print(model2_poly)

eval2_poly <- predict(model2_poly, newdata = test)
confusionMatrix(eval2_poly, test$label)

#-------------- Observations ----------#
# Slight reduction in accuracy but similar perfromance


#======= Polynomial Model with degree=2, scale=1 and offset=10 =======#

model3_poly <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 10))
print(model3_poly)

eval3_poly <- predict(model3_poly, newdata = test)
confusionMatrix(eval3_poly, test$label)

#--------- Observations ----------#

# similar perfromance as before, scale and offset seem to have little effect on performance


#======= Polynomial Model with degree=2, scale=1, offset=1, C=3 =======#

model4_poly <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 3, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(model4_poly)

eval4_poly <- predict(model4_poly, newdata = test)
confusionMatrix(eval4_poly, test$label)

#---------- Observations ----------#
# similar perfromance as before


#============ Grid search to optimise hyperparameters =============#

grid_poly = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4, 5), 
                        scale = c(-100, -10, -1, 1, 10, 100))

fit.poly <- train(label ~ ., data = train, metric = "Accuracy", method = "svmPoly",tuneGrid = grid_poly,
                  trControl = trainControl(method = "cv", number = 2), preProcess = NULL)

# Results of cross validation
print(fit.poly) 
plot(fit.poly)

eval_cv_poly <- predict(fit.poly, newdata = test)
confusionMatrix(eval_cv_poly, test$label)

#---------------- Observations ------------------#
# Best model obtained for C = 0.01, degree = 2, scale = 1
# as data has been scaled already scale = 1 is optimum
# C has little to no effect on perfomance, C = 0.01 generic model has been picked as optimum
# degrees higher than 2 are overfitting
# Accuracy of 95.24%, sensitivities > 92%, specificities > 99%


#========== Optmised polynomial model ==========#
model5_poly <- ksvm(label ~ ., data = train, kernel = "polydot", scaled = FALSE, C = 0.01, 
                    kpar = list(degree = 2, scale = 1, offset = 0.5))
print(model5_poly)

eval5_poly <- predict(model5_poly, newdata = test)
confusionMatrix(eval5_poly, test$label)

#----------- Observations --------------#

# offset of 0.5 used as independent variables are in the range of 0 to 1
# best accuracy of polynomial kernels 95.25%


#=============== Conclusion ==================#

# Final model
final_model = fit.rbf2


# Accuracy = 95.46%
# Sensitivites > 92%
# Specificities > 99%
# Polynomial kernel (C = 0.01, degree = 2, scale = 1, offset = 0.05) also perfromed equally well
# performance metrics are only marginally lesser than radial kernel
# Run time is better than that of radial kernel

#-------------------------------------------- End -------------------------------------------------#