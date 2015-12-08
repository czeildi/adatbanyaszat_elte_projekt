replace_weekday_columns <- function(news_data) {
    news_data <- news_data[,"weekday":= apply(news_data[,weekday_columns,with = F],1,
                                              function(x) weekday_columns[which(x==1)])]
    news_data <- news_data[,weekday_columns:=NULL, with = F]
    news_data
}

replace_channel_columns <- function(news) {
    news_data <- copy(news)
    news_data <- news_data[,"no_channel":= apply(news_data[,data_channel_columns, with = F],1,
                                                  function(x) {1 - sum(x)})]
    channel_columns <- c(data_channel_columns, "no_channel")
    news_data <- news_data[,"channel":= apply(news_data[,channel_columns,with = F],1,
                                              function(x) channel_columns[which(x==1)])]
    news_data <- news_data[,channel_columns:=NULL, with = F]
    news_data
}
