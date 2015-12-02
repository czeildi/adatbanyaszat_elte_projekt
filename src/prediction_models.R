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
