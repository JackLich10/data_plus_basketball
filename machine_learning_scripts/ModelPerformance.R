#Read in Data Files
library(readr)
library(tidyverse)
library(rlang)
training <- read_csv("data/TrainingData.csv")
testing <- read_csv("data/TestingData.csv")

training <- training %>%
  dplyr::select(-shooterID)
testing <- testing %>%
  dplyr::select(-shooterID)

training <- as.data.frame(training)
testing <- as.data.frame(testing)

training[, 14] <- as.factor(training[, 14])
training[, 15] <- as.factor(training[, 15])

testing[, 14] <- as.factor(testing[, 14])
testing[, 15] <- as.factor(testing[, 15])

means <- apply(training[,1:13], 2, mean)
stdevs <- apply(training[,1:13], 2, sd)
training[,1:13] <- scale(training[,1:13], means, stdevs)

means <- apply(testing[,1:13], 2, mean)
stdevs <- apply(testing[,1:13], 2, sd)
testing[,1:13] <- scale(testing[,1:13], means, stdevs)

#Logistic Regression Model
library(nnet)
mymodel <- multinom(result~., data = training)

#Misclassification
p <- predict(mymodel, training)
tab <- table(p, training$result)
tab

1-sum(diag(tab))/sum(tab)

table(training$result)
563/1059

#Model Performance Evaluation
library(ROCR)
pred <- predict(mymodel, training, type = "prob")
head(pred)

head(training$result)

hist(pred)

pred <- prediction(pred, training$result)
eval <- performance(pred, "acc")
plot(eval)
