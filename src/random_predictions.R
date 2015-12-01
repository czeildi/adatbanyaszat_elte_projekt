source("src/load.R")

news$pred1 <- sample(news$shares)
news$pred2 <- median(news$shares)

#categorical variable
percent_of_not_popular <- 0.9
limit_of_popularity <- quantile(news$shares, percent_of_not_popular)
news$is_popular <- (news$shares > limit_of_popularity)
num_poplular <- sum(news$is_popular)
news$pred3 <- sample(c(rep(TRUE, num_poplular), rep(FALSE,nrow(news) - num_poplular)))
#kw_avg_avg has the highest correlation
news[,"pred4":=
         ifelse(kw_avg_avg > quantile(kw_avg_avg, percent_of_not_popular),
                TRUE, 
                FALSE)]

#calculate errors
sqrt(mean((news$shares-news$pred1)*as.numeric(news$shares-news$pred1)))
sqrt(mean((news$shares-news$pred2)*as.numeric(news$shares-news$pred2)))
sqrt(mean((news$is_popular-news$pred3)*(news$is_popular-news$pred3)))
sqrt(mean((news$is_popular-news$pred4)*(news$is_popular-news$pred4)))
