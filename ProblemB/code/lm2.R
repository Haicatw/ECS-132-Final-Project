library(regtools)
data(day1)

data <- data.frame(day1[, c(9:13)])

k = 3 # overall k value search
x = 30 # predict day 30

get_data <- function(k,data) {
    dl <- list()
    size = nrow(data) - k
    d = k - 1
    for (i in 1:size) {
        for (c in 1:ncol(data)) {
            col <- c()
            for (j in 0:d) {
                col <- c(col, data[c][i+j,])
            }
            dl <- c(dl, list(t(col)))
        }
    }
    l <- do.call(rbind.data.frame, dl)
    l <- t(l)
    return (l)
}
data <- data.frame(day1[, c(10,12)])
data <- head(data)
dl <- get_data(3,data)
for (c in 1:10) {
    for(i in 1:x) {
        linearMod <- lm(l[,30] ~ l[,]
    }
}