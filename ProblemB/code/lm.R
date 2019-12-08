library(regtools)
data(day1)
x <- head(day1)
print(head(x))

Mean_square_error <- function(y_true, y_pred) {
    norm_y_t <- (y_true- min(y_true)) /(max(y_true)-min(y_true))
    norm_y_p <- (y_pred- min(y_pred)) /(max(y_pred)-min(y_pred))
    diff <- (norm_y_t - norm_y_t)^2
    return (mean(diff))
}

train_test_split <- function(dataset, num_train, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    indeces <- sample(1:nrow(dataset), size=num_train, replace=FALSE)

    train_set <- dataset[indeces, ]
    test_set <- dataset[-indeces, ]
    list(train=train_set, test=test_set)
}

normalize <- function(x) {
    norm_x <- x
    for (i in 1:ncol(x)) {
        col <- (x[,i]- min(x[,i])) /(max(x[,i])-min(x[,i]))
        norm_x[,i] <- col
    }
    return(norm_x)
}

data <- data.frame(day1[, c(3,4,5,8:13,16)])
cat("Use normalized season, yr, mnth, workingday, weatherist, temp, atemp, hum and windspeed to predict tot", "\n", sep="")
data <- train_test_split(data, 631)
train <- data$train
test <- data$test
# train_x <- train[c(1,3:7)]
# train_y <- train[2]
# test_x <- test[c(1,3:7)]
# test_y <- test[2]
train_x <- normalize(train[c(1,2,3,4,5,6,7,8,9)])
train_y <- train[10]
test_x <- normalize(test[c(1,2,3,4,5,6,7,8,9)])
test_y <- test[10]


linearMod <- lm(train_y[,1] ~ train_x[,1] + train_x[,2] + train_x[,3] + train_x[,4] + train_x[,5] + train_x[,6] + train_x[,7] + train_x[,8] + train_x[,9])
summary(linearMod)
cfs <- coef(linearMod)
cat("The linearModel has weight", cfs, "\n", sep="")

pred_y <- c()
for (i in 1:nrow(test_x)) {
    x <- c(1)
    x <- c(x, as.numeric(test_x[i,]))
    y <- cfs %*% x
    pred_y <- c(pred_y, y)
    x <- 0
}
pred_y <- round(pred_y)
y_test <- as.numeric(as.character(unlist(test_y[[1]])))
error <- Mean_square_error(pred_y, y_test)
cat("Overall error of this linear model is", error, "\n", sep="")