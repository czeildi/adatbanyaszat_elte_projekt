source('src/load.R')

library(caret)
library(class)
library(rpart)
library(e1071)

# knn ---------------------------------------------------------------------



# preProcValues <- preProcess(x = popular_train_wo_target,method = c("center", "scale"))
ctrl <- trainControl(method="repeatedcv",repeats = 3)
popular_train <- popular_train[sample(.N, 1000)]
popular_test <- popular_test[sample(.N, 1000)]
knnFit <- train(is_popular ~ ., data = popular_train, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)

knnPredict <- predict(knnFit,newdata = popular_test)
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, popular_test$is_popular)


#knn with kw_avg_avg
ctrl <- trainControl(method="repeatedcv",repeats = 3)
popular_train <- popular_train[sample(.N, 1000), c("kw_avg_avg", "is_popular"), with = F]
popular_test <- popular_test[sample(.N, 1000), c("kw_avg_avg", "is_popular"), with = F]
knnFit <- train(is_popular ~ ., data = popular_train, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)

knnPredict <- predict(knnFit,newdata = popular_test)
#Get the confusion matrix to see accuracy value and other parameter values
confusionMatrix(knnPredict, popular_test$is_popular)


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

# naive bayes -------------------------------------------------------------

set.seed(3456)
fit3 <- train(is_popular ~ ., 
              data=popular_train,
              method = "nb",
              trControl = trainControl(method="cv", number = 5),
              tuneGrid = data.frame(fL=0, usekernel=FALSE))

pred3 <- predict(fit3, popular_test, type="raw")
#Warnings that probability is 0 for some cases
confusionMatrix(pred3, popular_test$is_popular)


# neural network ----------------------------------------------------------
nnModel <- train(shares~., data = test, method = "neuralnet",
                 algorithm = 'backprop', learningrate = 0.25,
                 trControl = trainControl(method = "repeatedcv", repeats = 2), linout = TRUE)

nnModel <- neuralnet(shares ~ kw_avg_avg + timedelta, train, hidden=5, threshold=0.1)
