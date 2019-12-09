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

binarize <- function(Y) {
    nrow <- length(Y)
    classes <- unique(Y)
    ncol <- length(classes)
    binarizedy <- matrix(data = 0, nrow = nrow, ncol = ncol)
    i <- 1
    sort(classes)
    for (class in classes) {
        binarizedy[ ,i] <- ifelse(Y==class, 1, 0)
        i <- i + 1
    }
    return(binarizedy)
}

normalize <- function(x) {
    norm_x <- x
    for (i in 1:ncol(x)) {
        col <- (x[,i]- min(x[,i])) /(max(x[,i])-min(x[,i]))
        norm_x[,i] <- col
    }
    return(norm_x)
}

generatePolynomialInput <- function(power, X) {
    processed_X <- X
    if (power == 1) {
        return (X)
    }
    for (i in 2:power) {
        processed_X <- cbind(processed_X, X^i)
    }
    return (processed_X)
}

data(day1)
processed_data <- day1[, c(3, 5, 9, 10, 11, 12, 13)]
#c("season" ,"mnth" ,"weathersit" ,"temp" ,"atemp" ,"hum" ,"windspeed")
train_test <- train_test_split(processed_data, 631)
train <- train_test$train
test <- train_test$test
train_X <- normalize(train[, c(1, 2, 4, 5, 6, 7)])
#c("season" ,"mnth" ,"temp" ,"atemp" ,"hum" ,"windspeed")
test_X <- normalize(test[, c(1, 2, 4, 5, 6, 7)])
train_y <- binarize(train[, 3])
test_y <- binarize(test[, 3])

doGlm <- function(y, power=1) {
    glm(formula = y ~ train_poly_X[,1] + train_poly_X[,2] + train_poly_X[,3] + train_poly_X[,4] + train_poly_X[,5] + train_poly_X[,6], family = binomial)$coefficients
}

train_poly_X <- generatePolynomialInput(1, train_X)
allCoefVect <- apply(train_y, 2, doGlm)
print(allCoefVect)
input_X <- cbind(rep(1, dim(test_X)[1]), generatePolynomialInput(1, test_X))
predict_prob <- as.matrix(input_X) %*% as.matrix(allCoefVect)
predicted_y <- apply(predict_prob , 1, which.max)
print(Accuracy(predicted_y, test[, 3]))

doGlm <- function(y, power=1) {
    glm(formula = y ~ train_poly_X[,1] + train_poly_X[,2] + train_poly_X[,3] + train_poly_X[,4] + train_poly_X[,5] + train_poly_X[,6] + train_poly_X[,7] + train_poly_X[,8] + train_poly_X[,9] + train_poly_X[,10] + train_poly_X[,11] + train_poly_X[,12], family = binomial)$coefficients
}

train_poly_X <- generatePolynomialInput(2, train_X)
allCoefVect <- apply(train_y, 2, doGlm)
print(allCoefVect)
input_X <- cbind(rep(1, dim(test_X)[1]), generatePolynomialInput(2, test_X))
predict_prob <- as.matrix(input_X) %*% as.matrix(allCoefVect)
predicted_y <- apply(predict_prob , 1, which.max)
print(Accuracy(predicted_y, test[, 3]))

doGlm <- function(y, power=1) {
    glm(formula = y ~ train_poly_X[,1] + train_poly_X[,2] + train_poly_X[,3] + train_poly_X[,4] + train_poly_X[,5] + train_poly_X[,6] + train_poly_X[,7] + train_poly_X[,8] + train_poly_X[,9] + train_poly_X[,10] + train_poly_X[,11] + train_poly_X[,12] + train_poly_X[,13] + train_poly_X[,14] + train_poly_X[,15] + train_poly_X[,16] + train_poly_X[,17] + train_poly_X[,18], family = binomial)$coefficients
}

train_poly_X <- generatePolynomialInput(3, train_X)
allCoefVect <- apply(train_y, 2, doGlm)
print(allCoefVect)
input_X <- cbind(rep(1, dim(test_X)[1]), generatePolynomialInput(3, test_X))
predict_prob <- as.matrix(input_X) %*% as.matrix(allCoefVect)
predicted_y <- apply(predict_prob , 1, which.max)
print(Accuracy(predicted_y, test[, 3]))