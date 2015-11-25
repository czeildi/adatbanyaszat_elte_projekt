library(data.table)
library(rmarkdown)
library(ggplot2)

# weekday_columns <- names(news)[32:38]
# data_channel_columns <- names(news)[14:19]
# kw_columns <- names(news)[20:28]
# lda_columns <- names(news)[40:44]

getNewsData <- function(filename) {
    fread(filename)
}
