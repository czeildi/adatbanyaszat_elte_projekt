source('src/load.R')

library(caret)
library(class)
library(rpart)
library(e1071)
library(ROCR)

# knn ---------------------------------------------------------------------

popular_50 <- get_popular_data(news)
popular_train_50 <- train_news(popular_50)
popular_test_50 <- test_news(popular_50)
small_popular_train_50 <- small_data(popular_train_50)
small_popular_test_50 <- small_data(popular_test_50)

ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 3)
knnFit <- train(is_popular ~ ., data = small_popular_train_50, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)

knnFit

knnPredict <- predict(knnFit,newdata = small_popular_test_50)
confusionMatrix(knnPredict, small_popular_test_50$is_popular)



pos_probs <- predict(knnFit, newdata = small_popular_test_50, "prob")$popular
predictions <- prediction(predict(knnFit, small_popular_test_50, "prob")$popular, small_popular_test_50$is_popular)
perf <- performance(predictions, measure = "tpr", x.measure = "fpr")

roc_df <- data.frame(
    FPR = perf@x.values[[1]], 
    TPR = perf@y.values[[1]], 
    cutoff = perf@alpha.values[[1]]
)

ggplot(roc_df) + 
    geom_line(aes(FPR, TPR), size = 2, col = "darkred") +
    geom_ribbon(aes(FPR, ymin = 0, ymax = TPR), fill = "darkred", alpha = 0.3) +
    geom_segment(
        aes(x = 0, y = 0, xend = 1, yend = 1), 
        linetype = "dotted", col = "black"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) + 
    xlab("False Positive Rate") + ylab("True Positive Rate")





popular_10 <- get_popular_data(news, percent_of_not_popular = 0.9)
popular_train_10 <- train_news(popular_10)
popular_test_10 <- test_news(popular_10)
small_popular_train_10 <- small_data(popular_train_10)
small_popular_test_10 <- small_data(popular_test_10)

ctrl <- trainControl(method="repeatedcv", number = 5, repeats = 3)
knnFit <- train(is_popular ~ ., data = small_popular_train_10, method = "knn",
                trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)

plot(knnFit)

knnFit

knnPredict <- predict(knnFit,newdata = small_popular_test_10)
confusionMatrix(knnPredict, small_popular_test_10$is_popular)



pos_probs <- predict(knnFit, newdata = small_popular_test_10, "prob")$popular
predictions <- prediction(predict(knnFit, small_popular_test_10, "prob")$popular, small_popular_test_10$is_popular)
perf <- performance(predictions, measure = "tpr", x.measure = "fpr")

roc_df <- data.frame(
    FPR = perf@x.values[[1]], 
    TPR = perf@y.values[[1]], 
    cutoff = perf@alpha.values[[1]]
)

ggplot(roc_df) + 
    geom_line(aes(FPR, TPR), size = 2, col = "darkred") +
    geom_ribbon(aes(FPR, ymin = 0, ymax = TPR), fill = "darkred", alpha = 0.3) +
    geom_segment(
        aes(x = 0, y = 0, xend = 1, yend = 1), 
        linetype = "dotted", col = "black"
    ) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) + 
    xlab("False Positive Rate") + ylab("True Positive Rate")

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


# adaboost ----------------------------------------------------------------


# logit -------------------------------------------------------------------


