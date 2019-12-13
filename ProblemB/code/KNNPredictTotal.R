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

data(day1)
processed_data <- day1[, c(3, 5, 9, 10, 11, 12, 13)]
#c("season" ,"mnth" ,"weathersit" ,"temp" ,"atemp" ,"hum" ,"windspeed")
train_test <- train_test_split(processed_data, 631)
train <- train_test$train
test <- train_test$test

eval_grid_search <- function(train_X, test_X, train_y, test_y, para_range, predict_name) {
    k_val_list <- c()
    mse_list <- c()
    train_mse_list <- c()

    for (i in 1:para_range) {
        pred_y <- basicKNN(train_X, train_y, test_X, i)
        #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
        mse <- Mean_square_error((pred_y$regests), test_y)
        mse_list <- c(mse_list, mse)
        cat("Mean squared error for testing set with k=", i, " is ", mse, "\n", sep="")

        pred_y <- basicKNN(train_X, train_y, train_X, i)
        #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
        mse <- Mean_square_error((pred_y$regests), train_y)
        train_mse_list <- c(train_mse_list, mse)
        cat("Mean squared error for training set with k=", i, " is ", mse, "\n", sep="")
        k_val_list <- c(k_val_list, i)
    }

    train_title = paste("K Value vs Mean Squared Error on Training set of ", predict_name, " predictor.", sep="")
    test_title = paste("K Value vs Mean Squared Error on Testing set of ", predict_name, " predictor.", sep="")
    best_k <- which.min(mse_list)
    cat("When k=", best_k, " the model has the best performance, where the mean squared error of the model is ", mse_list[best_k], "\n", sep="")
    cat("The model has mean squared error equals to ", train_mse_list[best_k], " on training set.", "\n", sep="")
    plot(k_val_list, mse_list, main=test_title, xlab="k value", ylab="mean squared error")
    lines(k_val_list, mse_list)
    plot(k_val_list, train_mse_list, main=train_title, xlab="k value", ylab="mean squared error")
    lines(k_val_list, train_mse_list)
}

# Predict temp
train_X <- train[, c(1, 2, 3, 5, 6, 7)]
test_X <- test[, c(1, 2, 3, 5, 6, 7)]
train_y <- train[, 4]
test_y <- test[, 4]
eval_grid_search(train_X, test_X, train_y, test_y, 200, "temp")

# Predict atemp
train_X <- train[, c(1, 2, 3, 4, 6, 7)]
test_X <- test[, c(1, 2, 3, 4, 6, 7)]
train_y <- train[, 5]
test_y <- test[, 5]
eval_grid_search(train_X, test_X, train_y, test_y, 200, "atemp")

# Predict hum
train_X <- train[, c(1, 2, 3, 4, 5, 7)]
test_X <- test[, c(1, 2, 3, 4, 5, 7)]
train_y <- train[, 6]
test_y <- test[, 6]
eval_grid_search(train_X, test_X, train_y, test_y, 200, "hum")

# Predict windspeed
train_X <- train[, c(1, 2, 3, 4, 5, 6)]
test_X <- test[, c(1, 2, 3, 4, 5, 6)]
train_y <- train[, 7]
test_y <- test[, 7]
eval_grid_search(train_X, test_X, train_y, test_y, 200, "windspeed")
