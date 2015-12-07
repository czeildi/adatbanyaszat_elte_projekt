library(data.table)
library(rmarkdown)
library(ggplot2)

getNewsData <- function(filename) {
    data <- fread(filename)
    data$id <- seq(1, nrow(data))
    data$url <- NULL
    setnames(data, names(data)[13:18],
             sapply(names(data)[13:18], function(x) {substr(x, 17,100)}))
    setnames(data, names(data)[31:37],
             sapply(names(data)[31:37], function(x) {substr(x, 12,100)}))
    data
}

news <- getNewsData(file.path('data', 'OnlineNewsPopularity.csv'))

weekday_columns <- names(news)[31:37]
data_channel_columns <- names(news)[13:18]
kw_columns <- names(news)[19:27]
lda_columns <- names(news)[39:43]


train_news <- function(news, seed = 123456) {
    set.seed(123456)
    bound <- floor((nrow(news)/5)*3)
    news_for_sampling <- news[sample(nrow(news)), ] 
    train <- news_for_sampling[1:bound,]
    train <- train["shares":= log(shares)]
    train
}

small_data <- function(big_data, size = 1000) {
    small <- copy(big_data)
    small[sample(.N, size)]
}

test_news <- function(news, seed = 123456) {
    set.seed(123456)
    bound <- floor((nrow(news)/5)*3)
    news_for_sampling <- news[sample(nrow(news)), ] 
    test <- news_for_sampling[(bound+1):nrow(news), ]  
    test <- test["shares":= log(shares)]
    test
}

# train <- train_news(news)
# small_train <- small_data(train)
# test <- test_news(news)
# small_test <- small_data(test)

get_popular_data <- function(news, percent_of_not_popular = 0.9) {
    limit_of_popularity <- quantile(news$shares, percent_of_not_popular)
    popular <- copy(news)
    popular$is_popular <- (popular_test$shares > limit_of_popularity)
    popular$is_popular <- as.factor(popular$is_popular)
    popular$shares <- NULL
    popular
}

# popular <- get_popular_data(news)
# popular_train <- train_news(popular)
# popular_test <- test_news(popular)
# small_popular_train <- small_data(popular_train)
# small_popular_test <- small_data(popular_test)
# 
