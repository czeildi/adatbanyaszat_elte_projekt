get_readable_news <- function(news) {
    weekday_columns <- names(news)[32:38]
    data_channel_columns <- names(news)[14:19]
    readable_news <- news
    readable_news$weekday <- apply(readable_news[,weekday_columns,with = F],1,
                                   function(x) weekday_columns[which(x==1)])
    # readable_news$num_channel <- apply(readable_news[,data_channel_columns, with = F],1,
    #                                    function(x) sum(x))
    readable_news$channel <- apply(readable_news[,data_channel_columns, with = F],1,
                                   function(x) data_channel_columns[which(x==1)])
    
    # num_channel is 0 or 1
    readable_news <- readable_news[,weekday_columns:=NULL, with = F]
    readable_news <- readable_news[,data_channel_columns:=NULL, with = F]
    # readable_news$num_channel <- NA
    readable_news
}
