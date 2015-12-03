source('src/load.R')

#linear model
lm <- lm(shares ~ kw_avg_avg, data = news)
news$lin_pred1 <- lm$fitted.values
news$lin_pred1_res <- lm$residuals

ggplot(news, aes(x = kw_avg_avg, y = shares)) + geom_point() +
    geom_smooth(method = "lm", formula = y ~ x)

ggplot(news, aes(x = log(kw_avg_avg + 1), y = log(shares))) + geom_point() +
    geom_smooth(method = "lm", formula = y ~ x)
cor(log(news$shares), log(news$kw_avg_avg + 1))

ggplot(news, aes(x = sqrt(kw_avg_avg), y = sqrt(shares))) + geom_point() +
    geom_smooth(method = "lm", formula = y ~ x)

cor(sqrt(news$shares), sqrt(news$kw_avg_avg))

library('rpart')
percent_of_not_popular <- 0.9
limit_of_popularity <- quantile(news$shares, percent_of_not_popular)
news$is_popular <- (news$shares > limit_of_popularity)
news$shares <- NULL
tree <- rpart(is_popular ~ ., data = news)
printcp(tree)
plotcp(tree)

tree2 <- rpart(is_popular ~ kw_avg_avg + timedelta + num_keywords, data = news)
printcp(tree2)

#clustering

# k nearest neighbors
library('class')
news$knn_pred <- FALSE
news[5001:nrow(news)]$knn_pred <- knn1(news[1:5000,], news[5001:nrow(news)], cl = news[1:5000,]$is_popular)
