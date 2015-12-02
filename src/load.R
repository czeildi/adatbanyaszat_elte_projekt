library(data.table)
library(rmarkdown)
library(ggplot2)

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

