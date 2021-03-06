source('src/load.R')

library(caret)
library(class)
library(rpart)
library(e1071)
library(ROCR)
library(ada)

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

popular_50 <- get_popular_data(news)
popular_train_50 <- train_news(popular_50)
popular_test_50 <- test_news(popular_50)
small_popular_train_50 <- small_data(popular_train_50)
small_popular_test_50 <- small_data(popular_test_50)

set.seed(3456)
nbFit <- train(is_popular ~ ., 
              data=small_popular_train_50,
              method = "nb",
              trControl = trainControl(method="repeatedcv", number = 5),
              tuneGrid = data.frame(fL=0, usekernel=FALSE))

nbPredict <- predict(nbFit, small_popular_test_50, type="raw")
confusionMatrix(nbPredict, small_popular_test_50$is_popular)



pos_probs <- predict(nbFit, newdata = small_popular_test_50, "prob")$popular
predictions <- prediction(predict(nbFit, small_popular_test_50, "prob")$popular, small_popular_test_50$is_popular)
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

set.seed(3456)
nbFit <- train(is_popular ~ ., 
               data=small_popular_train_10,
               method = "nb",
               trControl = trainControl(method="repeatedcv", number = 5),
               tuneGrid = data.frame(fL=0, usekernel=FALSE))

nbPredict <- predict(nbFit, small_popular_test_10, type="raw")
confusionMatrix(nbPredict, small_popular_test_10$is_popular)



pos_probs <- predict(nbFit, newdata = small_popular_test_10, "prob")$popular
predictions <- prediction(predict(nbFit, small_popular_test_10, "prob")$popular, small_popular_test_10$is_popular)
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


# neural network ----------------------------------------------------------
popular_50 <- get_popular_data(news)
popular_train_50 <- train_news(popular_50)
popular_test_50 <- test_news(popular_50)
small_popular_train_50 <- small_data(popular_train_50)
small_popular_test_50 <- small_data(popular_test_50)

nnFit <- train(is_popular~., data = small_popular_train_50, method = "nnet",
                 algorithm = 'backprop', learningrate = 0.25, maxit = 50)

nnPredict <- predict(nnFit, small_popular_test_50, type="raw")
confusionMatrix(nnPredict, small_popular_test_50$is_popular)

pos_probs <- predict(nnFit, newdata = small_popular_test_50, "prob")$popular
predictions <- prediction(predict(nnFit, small_popular_test_50, "prob")$popular, small_popular_test_50$is_popular)
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

nnFit <- train(is_popular~., data = small_popular_train_10, method = "nnet",
               algorithm = 'backprop', learningrate = 0.25, maxit = 50)

nnPredict <- predict(nnFit, small_popular_test_10, type="raw")
confusionMatrix(nnPredict, small_popular_test_10$is_popular)


pos_probs <- predict(nnFit, newdata = small_popular_test_10, "prob")$popular
predictions <- prediction(predict(nnFit, small_popular_test_10, "prob")$popular, small_popular_test_10$is_popular)
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

# adaboost ----------------------------------------------------------------
popular_50 <- get_popular_data(news)
popular_train_50 <- train_news(popular_50)
popular_test_50 <- test_news(popular_50)
small_popular_train_50 <- small_data(popular_train_50)
small_popular_test_50 <- small_data(popular_test_50)

Grid <- expand.grid(maxdepth=25,nu=2,iter=10)
adaFit = train(is_popular~., data = small_popular_train_50, method="ada",tuneGrid=Grid)

adaPredict <- predict(adaFit, small_popular_test_50, type="raw")
confusionMatrix(adaPredict, small_popular_test_50$is_popular)


pos_probs <- predict(adaFit, newdata = small_popular_test_50, "prob")$popular
predictions <- prediction(predict(adaFit, small_popular_test_50, "prob")$popular, small_popular_test_50$is_popular)
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
