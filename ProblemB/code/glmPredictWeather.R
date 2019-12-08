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

logistic_model <- glm(formula = train_y ~ train_X[,1] + train_X[,2] + train_X[,3] + train_X[,4] + train_X[,5] + train_X[,6], family = binomial)
summary(logistic_model)
cfs <- coef(logistic_model)
cat("The logistic model has weight", cfs, "\n", sep="")