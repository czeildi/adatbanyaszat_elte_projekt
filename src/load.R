library(data.table)
library(rmarkdown)
library(ggplot2)
library(caret)
library(class)
library(rpart)
library(e1071)
library(neuralnet)

getNewsData <- function(filename) {
    data <- fread(filename)
    data$id <- seq(1, nrow(data))
    data$url <- NULL
    data
}

news <- getNewsData(file.path('data', 'OnlineNewsPopularity.csv'))

weekday_columns <- names(news)[31:37]
data_channel_columns <- names(news)[13:18]
kw_columns <- names(news)[19:27]
lda_columns <- names(news)[39:43]

set.seed(123456)
bound <- floor((nrow(news)/5)*3)

news_for_sampling <- news[sample(nrow(news)), ] 
train <- news_for_sampling[1:bound,]
test <- news_for_sampling[(bound+1):nrow(news), ]  

percent_of_not_popular <- 0.9
limit_of_popularity <- quantile(news$shares, percent_of_not_popular)
popular_train <- train
popular_test <- test
popular_train$is_popular <- (popular_train$shares > limit_of_popularity)
popular_train$shares <- NULL
popular_train$is_popular <- as.factor(popular_train$is_popular)
popular_test$is_popular <- (popular_test$shares > limit_of_popularity)
popular_test$shares <- NULL
popular_test$is_popular <- as.factor(popular_test$is_popular)

popular_train_wo_target <- copy(popular_train)
popular_train_wo_target$is_popular <- NULL
popular_test_wo_target <- copy(popular_train)
popular_test_wo_target$is_popular <- NULL


