train_test_split <- function(dataset, num_train, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    dataset <- dataset[sample(nrow(dataset)),]

    train_set <- dataset[1:num_train, ]
    test_set <- dataset[(1+num_train):total_rows, ]
    list(train_set, test_set)
}