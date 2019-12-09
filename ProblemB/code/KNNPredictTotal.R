library(regtools)
data(day1)

Mean_square_error <- function(y_true, y_pred) {
    norm_y_t <- (y_true- min(y_true)) /(max(y_true)-min(y_true))
    norm_y_p <- (y_pred- min(y_pred)) /(max(y_pred)-min(y_pred))
    diff <- mean((norm_y_t - norm_y_p)^2)
    return ((diff))
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
train_x <- normalize(train[c(1,2,3,4,5,6,7,8,9)])
train_y <- train[, 10]
test_x <- normalize(test[c(1,2,3,4,5,6,7,8,9)])
test_y <- test[, 10]

k_val_list <- c()
mse_list <- c()
train_mse_list <- c()

for (i in 1:300) {
    pred_y <- basicKNN(train_x, train_y, test_x, i)
    #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
    mse <- Mean_square_error((pred_y$regests), test_y)
    mse_list <- c(mse_list, mse)
    cat("Mean squared error for testing set with k=", i, " is ", mse, "\n", sep="")

    pred_y <- basicKNN(train_x, train_y, train_x, i)
    #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
    mse <- Mean_square_error((pred_y$regests), train_y)
    train_mse_list <- c(train_mse_list, mse)
    cat("Mean squared error for training set with k=", i, " is ", mse, "\n", sep="")
    k_val_list <- c(k_val_list, i)
}

best_k <- which.min(mse_list)
cat("When k=", best_k, " the model has the best performance, where the mean squared error of the model is ", mse_list[best_k], "\n", sep="")
cat("The model has mean squared error equals to ", train_mse_list[best_k], " on training set.", "\n", sep="")
plot(k_val_list, mse_list, main="K Value vs Mean Squared Error on Testing set", xlab="k value", ylab="mean squared error")
lines(k_val_list, mse_list)
plot(k_val_list, train_mse_list, main="K Value vs Mean Squared Error on Training set", xlab="k value", ylab="mean squared error")
lines(k_val_list, train_mse_list)