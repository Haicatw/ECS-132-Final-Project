train_test_split <- function(dataset, num_train, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    indeces <- sample(1:nrow(dataset), size=num_train, replace=FALSE)

    train_set <- dataset[indeces, ]
    test_set <- dataset[-indeces, ]
    list(train=train_set, test=test_set)
}

Mean_square_error <- function(y_true, y_pred) {
    diff <- (y_true - y_pred)^2
    return (mean(diff))
}

Accuracy <- function(y_true, y_pred) {
    return (sum(ifelse(y_pred == y_true, 1, 0))/length(y_pred))
}