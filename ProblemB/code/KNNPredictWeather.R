library(regtools)

train_test_split <- function(dataset, num_train, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    indeces <- sample(1:nrow(dataset), size=num_train, replace=FALSE)

    train_set <- dataset[indeces, ]
    test_set <- dataset[-indeces, ]
    list(train=train_set, test=test_set)
}

Accuracy <- function(y_true, y_pred) {
    return (sum(ifelse(y_pred == y_true, 1, 0))/length(y_pred))
}

data(day1)
processed_data <- day1[, c(3, 5, 9, 10, 11, 12, 13)]
#c("season" ,"mnth" ,"weathersit" ,"temp" ,"atemp" ,"hum" ,"windspeed")
train_test <- train_test_split(processed_data, 631)
train <- train_test$train
test <- train_test$test
train_X <- train[, c(1, 2, 4, 5, 6, 7)]
#c("season" ,"mnth" ,"temp" ,"atemp" ,"hum" ,"windspeed")
test_X <- test[, c(1, 2, 4, 5, 6, 7)]
train_y <- train[, 3]
test_y <- test[, 3]

k_val_list <- c()
accuracy_list <- c()
train_accuracy_list <- c()

for (i in 2:200) {
    pred_y <- basicKNN(train_X, train_y, test_X, i)
    #sum(ifelse(round(pred_y$regests) == test_y, 1, 0))/length(test_y)
    accuracy <- Accuracy(round(pred_y$regests), test_y)
    accuracy_list <- c(accuracy_list, accuracy)
    cat("Accuracy score for testing set with k=", i, " is ", accuracy, "\n", sep="")

    pred_y <- basicKNN(train_X, train_y, train_X, i)
    #sum(ifelse(round(pred_y$regests) == test_y, 1, 0))/length(test_y)
    accuracy <- Accuracy(round(pred_y$regests), train_y)
    train_accuracy_list <- c(train_accuracy_list, accuracy)
    cat("Accuracy score for training set with k=", i, " is ", accuracy, "\n", sep="")

    k_val_list <- c(k_val_list, i)
}

predict_name <- "weathersit"
plot_title <- paste("K Value vs Accuracy of ", predict_name, " predictor.", sep="")
best_k <- which.min(mse_list)
best_k <- which.max(accuracy_list)
cat("When k=", best_k, " the model has the best performance, where the model has accuracy score equals to ", accuracy_list[best_k], "\n", sep="")
cat("The model has accuracy score equals to ", train_accuracy_list[best_k], " on the training set.")

plot_df <- data.frame(k_val_list, train_accuracy_list, accuracy_list)
colnames(plot_df) <- list("k", "train_acc", "test_acc")
p <- ggplot(plot_df, aes(x=k)) + 
    geom_line(aes(y = train_acc, colour="Train Accuracy")) + 
    geom_line(aes(y = test_acc, colour="Test Accuracy")) +
    xlab("k Value") +
    ylab("Mean Squared Error") +
    ggtitle(plot_title) +
    scale_colour_manual("", 
        breaks = c("Train Accuracy", "Test Accuracy"),
        values = c("Train Accuracy"="darkred", "Test Accuracy"="steelblue"))
print(p)