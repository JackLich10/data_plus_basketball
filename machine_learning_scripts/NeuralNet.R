#Read in Data Files
training <- read_csv("data/TrainingData.csv")
testing <- read_csv("data/TestingData.csv")

#Clean up Data
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

training <- training %>%
  mutate(value = case_when(
    shot_distance > 20.75 ~ 3,
    shot_distance <= 20.75 ~ 2))

means <- apply(training[, 1:13], 2, mean)
stdevs <- apply(training[, 1:13], 2, sd)
training[, 1:13] <- scale(training[, 1:13], means, stdevs)

training_for_nn <- training %>%
  dplyr::select(-value)

training_for_nn <- as.data.frame(training_for_nn)

str(training_for_nn)

testing <- testing %>%
  mutate(value = case_when(
    shot_distance > 20.75 ~ 3,
    shot_distance <= 20.75 ~ 2))

means <- apply(testing[, 1:13], 2, mean)
stdevs <- apply(testing[, 1:13], 2, sd)
testing[, 1:13] <- scale(testing[, 1:13], means, stdevs)

testing_for_nn <- testing %>%
  dplyr::select(-value)

testing_for_nn <- as.data.frame(testing_for_nn)

#Neural Network
library(neuralnet)
set.seed(1234)
nn = neuralnet(result ~ ., data = training_for_nn, hidden = c(5, 2), act.fct = "logistic",
               err.fct = "ce", linear.output = FALSE, lifesign = "full", stepmax = 500000)
plot(nn,
     information = F,
     fontsize = 0,
     col.hidden = "#001A57",
     col.hidden.synapse = "#001A57",
     show.weights = F,
     fill = "lightblue")

#Predict
predict <- compute(nn, training %>% dplyr::select(-result), rep = 1)
predict$net.result
head(predict$net.result)
head(training$result)

#Confusion Matrix and Misclassification Error - training data
output <- compute(nn, training %>% dplyr::select(-result))
p1 <- output$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, training$result)
tab1

1 - sum(diag(tab1))/sum(tab1)

a <- cbind(p1, pred1, training$result)
a

#Confusion Matrix and Misclassification Error - testing data
output2 <- compute(nn, testing_for_nn[, -15])
p2 <- output2$net.result
pred2 <- ifelse(p2 > 0.5, 1, 0)
tab2 <- table(pred2, testing$result)
tab2

1 - sum(diag(tab2))/sum(tab2)

b <- cbind(p2, pred2, testing$result)
b

hist(predict$net.result)

detach(package:neuralnet)

pred <- prediction(predict$net.result, training$result)
eval <- performance(pred, "acc")
plot(eval)

roc <- performance(pred, "tpr", "fpr")
plot(roc,
     colorize = T)
abline(a = 0, b = 1)

#Area Under Curve
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc <- round(auc, 4)
legend(0.6, 0.2, auc)


c <- cbind(training, p1)

c <- c %>%
  dplyr::mutate(exp_PPS = p1 * value) %>%
  dplyr::select(exp_PPS)

colSums(c)

Duke201415teamstats %>%
  filter(game_number %in% c(1:7, 9, 11:13, 16, 18, 22, 23, 26, 27, 29, 30, 32:35)) %>%
  mutate(pts_min_ft = pts - ft) %>%
  dplyr::select(pts_min_ft) %>%
  colSums()
