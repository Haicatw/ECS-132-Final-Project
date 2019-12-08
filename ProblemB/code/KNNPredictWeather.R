library(regtools)

train_test_split <- function(dataset, num_train, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    indeces <- sample(1:nrow(dataset), size=num_train, replace=FALSE)

    train_set <- dataset[indeces, ]
    test_set <- dataset[-indeces, ]
    list(train=train_set, test=test_set)
}

data(day1)
processed_data <- day1[, c(3, 5, 9, 10, 11, 12, 13)]
#c("season" ,"mnth" ,"weathersit" ,"temp" ,"atemp" ,"hum" ,"windspeed")
train_test <- train_test_split(processed_data, 631)
train <- train_test$train
test <- train_test$test
#print(test)
train_X <- train[, c(1, 2, 4, 5, 6, 7)]
#c("season" ,"mnth" ,"temp" ,"atemp" ,"hum" ,"windspeed")
test_X <- test[, c(1, 2, 4, 5, 6, 7)]
train_y <- train[, 3]
test_y <- test[, 3]

# for (i in range(1:nrow(test_X))) {
#     basicKNN(train_X,train_y,,5)
# }

for (i in 1:300) {
    pred_y <- basicKNN(train_X, train_y, test_X, i)
    #print(sum(ifelse(round(pred_y$regests) == test_y, 1, 0)))
    print(sum(ifelse(round(pred_y$regests) == test_y, 1, 0))/length(test_y))
}
