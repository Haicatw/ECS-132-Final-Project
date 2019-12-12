library(regtools)
data(day1)

data <- data.frame(day1[, c(9:13)])

k = 3 # overall k value search
x = 30 # predict day 30
dl <- list()
for (c in 1:5) {
    for (i in 1:x) {
        col <- c()
        for (j in 1:k) {
            col <- c(col, data[c][i+j,])
        }
        dl <- c(dl, list(t(col)))
    }
}

l <- do.call(rbind.data.frame, dl)
l <- t(l)
for (c in 1:10) {
    for(i in 1:x) {
        linearMod <- lm(l[,30] ~ l[,]
    }
}