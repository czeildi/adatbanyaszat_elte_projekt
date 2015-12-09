source('src/load.R')

library(caret)
library(class)
library(rpart)
library(e1071)

# SVM ---------------------------------------------------------------------

train <- train_news(news)
train <- train[,"shares":= log(shares)]
small_train <- small_data(train)
test <- test_news(news)
test <- test[,"shares":= log(shares)]
small_test <- small_data(test)

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
linsvmFit <- train(shares~., data=small_train, method = "svmLinear",
                   trControl = ctrl, preProc = c("center", "scale"))
linsvmFit

small_test$linsvm <- predict(linsvmFit, newdata = small_test)
sqrt(mean((small_test$linsvm - small_test$shares)*(small_test$linsvm - small_test$shares)))


train <- train_news(news)
train <- train[,"shares":= log(shares)]
small_train <- small_data(train)
test <- test_news(news)
test <- test[,"shares":= log(shares)]
small_test <- small_data(test)

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
radialsvmFit <- train(shares~., data=small_train, method = "svmRadial",
                   trControl = ctrl, preProc = c("center", "scale"))

radialsvmFit

small_test$radialsvm <- predict(radialsvmFit, newdata = small_test)
sqrt(mean((small_test$radialsvm - small_test$shares)*(small_test$radialsvm - small_test$shares)))


# train <- train_news(news)
# train <- train[,"shares":= log(shares)]
# small_train <- small_data(train)
# test <- test_news(news)
# test <- test[,"shares":= log(shares)]
# small_test <- small_data(test)
# 
# ctrl <- trainControl(method = "repeatedcv", repeats = 3)
# ploysvmFit <- train(shares~., data=small_train, method = "svmPoly",
#                       trControl = ctrl, preProc = c("center", "scale"))
# ploysvmPredict <- predict(ploysvmFit, newdata = test)
# 
# ploysvmFit


# random forest -----------------------------------------------------------

train <- train_news(news)
train <- train[,"shares":= log(shares)]
small_train <- small_data(train)
test <- test_news(news)
test <- test[,"shares":= log(shares)]
small_test <- small_data(test)

ctrl <- trainControl(method = "repeatedcv", repeats = 3)
rfFit <- train(shares ~ .,data = small_train, method = "rf",
                trControl=ctrl,
                preProc = c("center", "scale"), prox = TRUE)

rfFit

small_test$rf <- predict(rfFit, newdata = small_test)
sqrt(mean((small_test$rf - small_test$shares)*(small_test$rf - small_test$shares)))

# neural network ----------------------------------------------------------
train <- train_news(news)
train <- train[,"shares":= log(shares)]
small_train <- small_data(train)
test <- test_news(news)
test <- test[,"shares":= log(shares)]
small_test <- small_data(test)

nnFit <- train(shares~., data = small_train, method = "nnet",
               algorithm = 'backprop', learningrate = 0.25, maxit = 50)

nnFit

small_test$nn <- predict(nnFit, newdata = small_test)
sqrt(mean((small_test$nn - small_test$shares)*(small_test$nn - small_test$shares)))

