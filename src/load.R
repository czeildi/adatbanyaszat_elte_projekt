library(data.table)
library(rmarkdown)
library(ggplot2)

getNewsData <- function(filename) {
    fread(filename)
}
