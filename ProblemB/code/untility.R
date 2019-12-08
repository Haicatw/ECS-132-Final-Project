train_test_split <- function(dataset, num_train) {
    total_rows <- nrow(dataset)
    train_set <- dataset[1:num_train, ]
    test_set <- dataset[(1+num_train):total_rows, ]
    list(train_set, test_set)
}