library(keras)
install_keras()

#Read Data
training <- read_csv("data/TrainingData.csv")
testing <- read_csv("data/TestingData.csv")

training <- training %>%
  dplyr::select(-shooterID, -distance_ten_seconds, -distance_closest_team, -angle_closest_team, )
testing <- testing %>%
  dplyr::select(-shooterID)


#Change to Matrix
training <- as.matrix(training)
dimnames(training) <- NULL

testing <- as.matrix(testing)
dimnames(testing) <- NULL

training[, 11] <- as.factor(training[, 11])

#Normalize
training[, 1:10] <- normalize(training[, 1:10])
summary(training)

testing[, 1:13] <- normalize(testing[, 1:13])
summary(testing)

trainingtarget <- training[, 12]
testingtarget <- testing[, 15]

trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testingtarget)

#Create Sequential Model
model <- keras_model_sequential()
model %>%
  layer_dense(units = 30, activation = "relu", input_shape = c(11)) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 20, activation = "relu") %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 2, activation = "softmax")
summary(model)

#Compile
model %>%
  compile(loss = "binary_crossentropy", optimizer = "adam", metrics = "accuracy")

#Fit Model
history <- model %>%
  fit(training[, 1:11], trainLabels, epoch = 200, batch_size = 32, validation_split = 0.2)
plot(history)

#Evaluate Model with Test Data
model1 <- model %>%
  evaluate(testing[, 1:14], testLabels)

#Prediction $ Confusion Matrix - Test Data
prob <- model %>%
  predict_proba(testing[, 1:14])

pred <- model %>%
  predict_classes(testing[, 1:14])

table1 <- table(Predicted = pred, Actual = testingtarget)

cbind(prob, pred, testingtarget)

#Fine-Tune

table1

