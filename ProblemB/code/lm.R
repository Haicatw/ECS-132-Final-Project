source(.//untility.R)
library(regtools)
data(day1)
x <- head(day1)
print(head(x))



Mean_square_error <- function(y_true, y_pred) {
    diff <- (y_true - y_pred)^2
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

a <- c(1,2,3)
b <- c(1,2,2)

Mean_square_error(a,b)
data <- data.frame(day1[, c(8:13,16)])

data <- train_test_split(data, 100)
train <- data$test
test <- data$train
train_x <- train[c(1,3:7)]
train_y <- train[2]
test_x <- test[c(1,3:7)]
test_y <- test[2]

linearMod <- lm(train_y ~ train_x, data=data)

linearMod <- lm(train_y[,1] ~ train_x[,1] + train_x[,2] + train_x[,3] + train_x[,4] + train_x[,5] + train_x[,6])
cfs <- coef(linearMod)


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
Mean_square_error(pred_y, y_test)