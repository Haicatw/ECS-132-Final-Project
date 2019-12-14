library(regtools)
library(ggplot2)
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

    test_set <- dataset[indeces, ]
    train_set <- dataset[-indeces, ]
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
train_test <- train_test_split(processed_data, 100)
train <- train_test$train
test <- train_test$test

eval_grid_search <- function(train_X, test_X, train_y, test_y, para_range, predict_name) {
    k_val_list <- c()
    mse_list <- c()
    train_mse_list <- c()

    for (i in 2:para_range) {
        pred_y <- basicKNN(train_X, train_y, test_X, i, leave1out=TRUE)
        #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
        mse <- Mean_square_error((pred_y$regests), test_y)
        mse_list <- c(mse_list, mse)
        #cat("Mean squared error for testing set with k=", i, " is ", mse, "\n", sep="")

        pred_y <- basicKNN(train_X, train_y, train_X, i, leave1out=TRUE)
        #sum(ifelse((pred_y$regests) == test_y, 1, 0))/length(test_y)
        mse <- Mean_square_error((pred_y$regests), train_y)
        train_mse_list <- c(train_mse_list, mse)
        #cat("Mean squared error for training set with k=", i, " is ", mse, "\n", sep="")
        k_val_list <- c(k_val_list, i)
    }

    plot_title = paste("K Value vs Mean Squared Error of ", predict_name, " predictor.", sep="")
    best_k <- which.min(mse_list)
    cat("When k=", best_k, " the model has the best performance, where the mean squared error of the model is ", mse_list[best_k], "\n", sep="")
    cat("The model has mean squared error equals to ", train_mse_list[best_k], " on training set.", "\n", sep="")

    plot_df <- data.frame(k_val_list, train_mse_list, mse_list)
    colnames(plot_df) <- list("k", "train_mse", "test_mse")
    p <- ggplot(plot_df, aes(x=k)) + 
        geom_line(aes(y = train_mse, colour="Train MSE")) + 
        geom_line(aes(y = test_mse, colour="Test MSE")) +
        xlab("k Value") +
        ylab("Mean Squared Error") +
        ggtitle(plot_title) +
        scale_colour_manual("", 
            breaks = c("Train MSE", "Test MSE"),
            values = c("Train MSE"="darkred", "Test MSE"="steelblue"))
    print(p)
}

max_k_val = 200

# Predict temp
train_X <- train[, c(1, 2, 3, 5, 6, 7)]
test_X <- test[, c(1, 2, 3, 5, 6, 7)]
train_y <- train[, 4]
test_y <- test[, 4]
eval_grid_search(train_X, test_X, train_y, test_y, max_k_val, "temp")

# Predict atemp
train_X <- train[, c(1, 2, 3, 4, 6, 7)]
test_X <- test[, c(1, 2, 3, 4, 6, 7)]
train_y <- train[, 5]
test_y <- test[, 5]
eval_grid_search(train_X, test_X, train_y, test_y, max_k_val, "atemp")

# Predict hum
train_X <- train[, c(1, 2, 3, 4, 5, 7)]
test_X <- test[, c(1, 2, 3, 4, 5, 7)]
train_y <- train[, 6]
test_y <- test[, 6]
eval_grid_search(train_X, test_X, train_y, test_y, max_k_val, "hum")

# Predict windspeed
train_X <- train[, c(1, 2, 3, 4, 5, 6)]
test_X <- test[, c(1, 2, 3, 4, 5, 6)]
train_y <- train[, 7]
test_y <- test[, 7]
eval_grid_search(train_X, test_X, train_y, test_y, max_k_val, "windspeed")
