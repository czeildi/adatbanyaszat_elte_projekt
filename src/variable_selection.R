source("src/load.R")
options(scipen = 4)

correlations <- sapply(news, function(x) {cor(x, news$shares)})

columns <- as.data.table(names(news))
columns$corr_with_target <- 0
for (name in columns$V1) {
    columns[V1 == name]$corr_with_target <- correlations[[name]]
}

#dimension reduction
pc <- prcomp(x=news,center=FALSE,scale=TRUE)
