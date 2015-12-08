source('src/load.R')

library(caret)
library(class)
library(rpart)
library(e1071)

# SVM ---------------------------------------------------------------------

train <- train[sample(.N, 1000)]
test <- test[sample(.N, 1000)]
ctrl <- trainControl(method = "repeatedcv", repeats = 10)
set.seed(1500)
mod <- train(shares~., data=train, method = "svmLinear", trControl = ctrl)
svmPredict <- predict(mod, newdata = test)

ctrl <- trainControl(method = "repeatedcv", repeats = 10)
set.seed(1500)
mod <- train(shares~., data=train, method = "svmRadial", trControl = ctrl, preProc = c("center", "scale"))

svmPredict <- predict(mod, newdata = test)

ctrl <- trainControl(method = "repeatedcv", repeats = 2)
set.seed(1500)
mod <- train(shares~., data=train, method = "svmPoly", trControl = ctrl, preProc = c("center", "scale"))

svmPredict <- predict(mod, newdata = test)

# random forest -----------------------------------------------------------

rf_model<-train(is_popular~.,data=popular_train, method="rf",
                trControl=trainControl(method="cv",number=5),
                preProc = c("center", "scale"), prox = TRUE)

rfPred <- predict(rf_model, newdata = popular_test)

# neural network ----------------------------------------------------------
nnModel <- train(shares~., data = test, method = "neuralnet",
                 algorithm = 'backprop', learningrate = 0.25,
                 trControl = trainControl(method = "repeatedcv", repeats = 2), linout = TRUE)

nnModel <- neuralnet(shares ~ kw_avg_avg + timedelta, train, hidden=5, threshold=0.1)
