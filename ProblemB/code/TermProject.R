# Denpendency
library(regtools)
library(ggplot2)

# Utility Function

# Split test and train set. 
train_test_split <- function(dataset, num_test, shuffle=TRUE, random_state=0) {
    total_rows <- nrow(dataset)

    set.seed(random_state)
    indeces <- sample(1:nrow(dataset), size=num_test, replace=FALSE)

    test_set <- dataset[indeces, ]
    train_set <- dataset[-indeces, ]
    list(train=train_set, test=test_set)
}

# MSE Calculator
Mean_square_error <- function(y_true, y_pred) {
    diff <- (y_true - y_pred)^2
    return (mean(diff))
}

# Accuracy Calculator
Accuracy <- function(y_true, y_pred) {
    return (sum(ifelse(y_pred == y_true, 1, 0))/length(y_pred))
}

# Min-max normalization
normalize <- function(x) {
    norm_x <- x
    for (i in 1:ncol(x)) {
        col <- (x[,i]- min(x[,i])) /(max(x[,i])-min(x[,i]))
        norm_x[,i] <- col
    }
    return(norm_x)
}

# Binarize y into multiple binary columns
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

# Add interactive terms to input variables
generatePolynomialInput <- function(power, X) {
    X <- apply(X, 2, unlist)
    processed_X <- X
    if (power == 1) {
        return (X)
    }
    for (i in 2:power) {
        processed_X <- cbind(processed_X, X^i)
    }
    return (processed_X)
}

# Rearrange dataset for past k days
rearrange <- function(X, y, k) {
    mat <- cbind(X, y)
    nrows <- dim(mat)[1]
    ncols <- dim(mat)[2]
    rearrangedMat <- c()
    for (i in 1:(nrows - k)) {
        rowdata <- X[i,]
        for (j in 1:(k-1)) {
            rowdata <- c(rowdata, X[i+j,])
        }
        rowdata <- c(rowdata, y[i+k])
        rowdata <- t(as.matrix(rowdata))
        rearrangedMat <- rbind(rearrangedMat, rowdata[1, ])
    }
    return (rearrangedMat)
}


# Predict temp, atemp, hum, windspeed using lm
data(day1)

eval_lm_grid_search <- function(feature_colnames, y_name, max_k, max_power, predict_name) {
    k_val_list <- 2:max_k
    train_mse_list <- c()
    test_mse_list <- c()
    for (k_val in 2:max_k) {
        train_mse_list_p <- c()
        test_mse_list_p <- c()
        for (p in 1:max_power) {
            print("----------------------------------------------")
            cat("Linear model with ", p, " order and past ", k_val, " days of data.", sep="")
            features <- day1[, feature_colnames]
            features <- normalize(features)
            act_y <- day1[, y_name]
            data <- rearrange(features, act_y, k_val)
            data <- train_test_split(data, 100)
            train <- data$train
            test <- data$test
            y_col <- dim(train)[2]
            feature_cols <- dim(train)[2] - 1
            train_X <- train[, c(1:feature_cols)]
            test_X <- test[, c(1:feature_cols)]
            train_y <- t(matrix(unlist(train[, y_col]),nr=1))
            test_y <- t(matrix(unlist(test[, y_col]),nr=1))
            
            train_X <- generatePolynomialInput(p, train_X)
            converted_train_X <- apply(as.matrix(train_X), 2, as.numeric)
            test_X <- generatePolynomialInput(p, test_X)
            converted_test_X <- apply(as.matrix(test_X), 2, as.numeric)

            lmout <- lm(train_y ~ ., data=data.frame(converted_train_X))
            print(summary(lmout))

            predict_y <- predict(lmout, newdata=data.frame(converted_test_X))
            mse <- Mean_square_error(predict_y, test_y)
            test_mse_list_p <- c(test_mse_list_p, mse)

            predict_y <- predict(lmout, newdata=data.frame(converted_train_X))
            mse <- Mean_square_error(predict_y, train_y)
            train_mse_list_p <- c(train_mse_list_p, mse)            
        }
        train_mse_list <- c(train_mse_list, train_mse_list_p)
        test_mse_list <- c(test_mse_list, test_mse_list_p)
    }
    train_mse_list <- matrix(data=train_mse_list, nrow=max_power, ncol=max_k-1)
    test_mse_list <- matrix(data=test_mse_list, nrow=max_power, ncol=max_k-1)
    train_mse_list[is.na(train_mse_list)] <- 1
    test_mse_list[is.na(test_mse_list)] <- 1
    plot_title = paste("K Value vs Mean Squared Error of ", predict_name, " predictor.", sep="")
    best_paras <- which(test_mse_list == min(test_mse_list), arr.ind = TRUE)
    best_p <- best_paras[1, "row"]
    best_k <- best_paras[1, "col"]
    cat("When order=", best_p, " k=", best_k, " the model has the best performance, where the mean squared error of the model is ", test_mse_list[best_p, best_k], "\n", sep="")
    cat("The model has mean squared error equals to ", train_mse_list[best_p, best_k], " on training set.", "\n", sep="")
    plot_df <- data.frame(k_val_list, train_mse_list[1,], train_mse_list[2,], train_mse_list[3,], test_mse_list[1,], test_mse_list[2,], test_mse_list[3,])
    colnames(plot_df) <- list("k", "first_train_mse", "second_train_mse", "third_train_mse", "first_test_mse", "second_test_mse", "third_test_mse")
    p <- ggplot(plot_df, aes(x=k)) + 
        geom_line(aes(y = first_train_mse, colour="First Order Train MSE")) + 
        geom_line(aes(y = second_train_mse, colour="Second Order Train MSE")) +
        geom_line(aes(y = third_train_mse, colour="Third Order Train MSE")) + 
        geom_line(aes(y = first_test_mse, colour="First Test MSE")) +
        geom_line(aes(y = second_test_mse, colour="Second Test MSE")) + 
        geom_line(aes(y = third_test_mse, colour="Third Test MSE")) +
        xlab("k Value") +
        ylab("Mean Squared Error") +
        ggtitle(plot_title) +
        scale_colour_manual("", 
            breaks = c("First Order Train MSE", "Second Order Train MSE", "Third Order Train MSE", "First Test MSE", "Second Test MSE", "Third Test MSE"),
            values = c("First Order Train MSE"="blue", "Second Order Train MSE"="yellow", "Third Order Train MSE"="red", "First Test MSE"="green", "Second Test MSE"="navy", "Third Test MSE"="grey"))
    print(p)
}


max_k_val = 7
max_power_val = 3
# Predict temp
eval_lm_grid_search(c("season", "mnth", "weathersit", "atemp", "hum", "windspeed"), c("temp"), max_k_val, max_power_val, "temp")

# Predict atemp
eval_lm_grid_search(c("season", "mnth", "weathersit", "temp", "hum", "windspeed"), c("atemp"), max_k_val, max_power_val, "atemp")

# Predict hum
eval_lm_grid_search(c("season", "mnth", "weathersit", "temp", "atemp", "windspeed"), c("hum"), max_k_val, max_power_val, "hum")

# Predict windspeed
eval_lm_grid_search(c("season", "mnth", "weathersit", "temp", "atemp", "hum"), c("windspeed"), max_k_val, max_power_val, "windspeed")

# Predict weathersit using glm
data(day1)

eval_glm_grid_search <- function(feature_colnames, y_name, max_k, max_power, predict_name) {
    k_val_list <- 2:max_k
    train_acc_list <- c()
    test_acc_list <- c()
    for (k_val in 2:max_k) {
        train_acc_list_p <- c()
        test_acc_list_p <- c()
        for (p in 1:max_power) {
            print("----------------------------------------------")
            cat("Linear model with ", p, " order and past ", k_val, " days of data.\n", sep="")
            features <- day1[, feature_colnames]
            features <- normalize(features)
            act_y <- day1[, y_name]
            data <- rearrange(features, act_y, k_val)
            data <- train_test_split(data, 100)
            train <- data$train
            test <- data$test
            y_col <- dim(train)[2]
            feature_cols <- dim(train)[2] - 1
            train_X <- train[, c(1:feature_cols)]
            test_X <- test[, c(1:feature_cols)]
            train_y <- t(matrix(unlist(train[, y_col]),nr=1))
            test_y <- t(matrix(unlist(test[, y_col]),nr=1))
            
            train_X <- generatePolynomialInput(p, train_X)
            converted_train_X <- apply(as.matrix(train_X), 2, as.numeric)
            test_X <- generatePolynomialInput(p, test_X)
            converted_test_X <- apply(as.matrix(test_X), 2, as.numeric)

            train_y_dumm <- binarize(train_y)
            test_y_dumm <- binarize(test_y)

            lmout1 <- glm(train_y_dumm[,1] ~ ., data=data.frame(converted_train_X), family = binomial)
            print(summary(lmout1))
            lmout2 <- glm(train_y_dumm[,2] ~ ., data=data.frame(converted_train_X), family = binomial)
            print(summary(lmout2))
            lmout3 <- glm(train_y_dumm[,3] ~ ., data=data.frame(converted_train_X), family = binomial)
            print(summary(lmout3))

            predict_y <- predict(lmout1, newdata=data.frame(converted_test_X))
            predict_y <- cbind(predict_y, predict(lmout2, newdata=data.frame(converted_test_X)))
            predict_y <- cbind(predict_y, predict(lmout3, newdata=data.frame(converted_test_X)))
            y_hat <- as.matrix(apply(predict_y, 1, which.max))
            acc <- Accuracy(y_hat, test_y)
            test_acc_list_p <- c(test_acc_list_p, acc)

            predict_y <- predict(lmout1, newdata=data.frame(converted_train_X))
            predict_y <- cbind(predict_y, predict(lmout2, newdata=data.frame(converted_train_X)))
            predict_y <- cbind(predict_y, predict(lmout3, newdata=data.frame(converted_train_X)))
            y_hat <- as.matrix(apply(predict_y, 1, which.max))
            acc <- Accuracy(y_hat, train_y)
            train_acc_list_p <- c(train_acc_list_p, acc)         
        }
        train_acc_list <- c(train_acc_list, train_acc_list_p)
        test_acc_list <- c(test_acc_list, test_acc_list_p)
    }

    train_acc_list <- matrix(data=train_acc_list, nrow=max_power, ncol=max_k-1)
    test_acc_list <- matrix(data=test_acc_list, nrow=max_power, ncol=max_k-1)
    train_acc_list[is.na(train_acc_list)] <- 1
    test_acc_list[is.na(test_acc_list)] <- 1
    plot_title = paste("K Value vs Accuracy of ", predict_name, " predictor.", sep="")

    best_paras <- which(test_acc_list == max(test_acc_list), arr.ind = TRUE)

    best_p <- best_paras[1, "row"]
    best_k <- best_paras[1, "col"]
    cat("When order=", best_p, " k=", best_k, " the model has the best performance, where the accuracy of the model is ", test_acc_list[best_p, best_k], "\n", sep="")
    cat("The model has mean squared error equals to ", train_acc_list[best_p, best_k], " on training set.", "\n", sep="")
    plot_df <- data.frame(k_val_list, train_acc_list[1,], train_acc_list[2,], train_acc_list[3,], test_acc_list[1,], test_acc_list[2,], test_acc_list[3,])
    colnames(plot_df) <- list("k", "first_train_acc", "second_train_acc", "third_train_acc", "first_test_acc", "second_test_acc", "third_test_acc")
    p <- ggplot(plot_df, aes(x=k)) + 
        geom_line(aes(y = first_train_acc, colour="First Order Train Accuracy")) + 
        geom_line(aes(y = second_train_acc, colour="Second Order Train Accuracy")) +
        geom_line(aes(y = third_train_acc, colour="Third Order Train Accuracy")) + 
        geom_line(aes(y = first_test_acc, colour="First Test Accuracy")) +
        geom_line(aes(y = second_test_acc, colour="Second Test Accuracy")) + 
        geom_line(aes(y = third_test_acc, colour="Third Test Accuracy")) +
        xlab("k Value") +
        ylab("Accuracy") +
        ggtitle(plot_title) +
        scale_colour_manual("", 
            breaks = c("First Order Train Accuracy", "Second Order Train Accuracy", "Third Order Train Accuracy", "First Test Accuracy", "Second Test Accuracy", "Third Test Accuracy"),
            values = c("First Order Train Accuracy"="blue", "Second Order Train Accuracy"="yellow", "Third Order Train Accuracy"="red", "First Test Accuracy"="green", "Second Test Accuracy"="navy", "Third Test Accuracy"="grey"))
    print(p)
}

max_k_val = 7
max_power_val = 3

eval_glm_grid_search(c("season", "mnth", "windspeed", "temp", "atemp", "hum"), c("weathersit"), max_k_val, max_power_val, "weathersit")


# Predict temp, atemp, hum, windspeed using kNN
data(day1)
processed_data <- day1[, c(3, 5, 9, 10, 11, 12, 13)]
train_test <- train_test_split(processed_data, 100)
train <- train_test$train
test <- train_test$test

eval_grid_search <- function(train_X, test_X, train_y, test_y, para_range, predict_name) {
    k_val_list <- c()
    mse_list <- c()
    train_mse_list <- c()

    for (i in 2:para_range) {
        pred_y <- basicKNN(train_X, train_y, test_X, i, leave1out=TRUE)
        mse <- Mean_square_error((pred_y$regests), test_y)
        mse_list <- c(mse_list, mse)
        #cat("Mean squared error for testing set with k=", i, " is ", mse, "\n", sep="")

        pred_y <- basicKNN(train_X, train_y, train_X, i, leave1out=TRUE)
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


# Predict weathersit using KNN

data(day1)
processed_data <- day1[, c(3, 5, 10, 11, 12, 13)]
weathersit <- day1[,c("weathersit")]
weathersit_dumms <- factorToDummies(as.factor(weathersit),'weathersit',omitLast=FALSE)
processed_data <- cbind(processed_data, weathersit_dumms)
train_test <- train_test_split(processed_data, 100, random_state=20)
train <- train_test$train
test <- train_test$test
train_X <- train[, c(1, 2, 3, 4, 5, 6)]
test_X <- test[, c(1, 2, 3, 4, 5, 6)]
train_y <- as.matrix(train[, 7:9])
test_y <- as.matrix(test[, 7:9])

k_val_list <- c()
accuracy_list <- c()
train_accuracy_list <- c()

for (i in 2:300) {
    pred_y <- basicKNN(train_X, train_y, test_X, i, leave1out=TRUE)
    y_hat <- as.matrix(apply(pred_y$regests, 2, which.max))
    eval_test_y <- as.matrix(apply(test_y, 1, which.max))
    accuracy <- Accuracy(y_hat, eval_test_y)
    accuracy_list <- c(accuracy_list, accuracy)
    #cat("Accuracy score for testing set with k=", i, " is ", accuracy, "\n", sep="")

    pred_y <- basicKNN(train_X, train_y, train_X, i, leave1out=TRUE)
    y_hat <- as.matrix(apply(pred_y$regests, 2, which.max))
    eval_train_y <- as.matrix(apply(train_y, 1, which.max))
    accuracy <- Accuracy(y_hat, eval_train_y)
    train_accuracy_list <- c(train_accuracy_list, accuracy)
    #cat("Accuracy score for training set with k=", i, " is ", accuracy, "\n", sep="")

    k_val_list <- c(k_val_list, i)
}

predict_name <- "weathersit"
plot_title <- paste("K Value vs Accuracy of ", predict_name, " predictor.", sep="")
best_k <- which.max(accuracy_list)
cat("When k=", best_k, " the model has the best performance, where the model has accuracy score equals to ", accuracy_list[best_k], "\n", sep="")
cat("The model has accuracy score equals to ", train_accuracy_list[best_k], " on the training set.")

plot_df <- data.frame(k_val_list, train_accuracy_list, accuracy_list)
colnames(plot_df) <- list("k", "train_acc", "test_acc")
p <- ggplot(plot_df, aes(x=k)) + 
    geom_line(aes(y = train_acc, colour="Train Accuracy")) + 
    geom_line(aes(y = test_acc, colour="Test Accuracy")) +
    xlab("k Value") +
    ylab("Accuracy") +
    ggtitle(plot_title) +
    scale_colour_manual("", 
        breaks = c("Train Accuracy", "Test Accuracy"),
        values = c("Train Accuracy"="darkred", "Test Accuracy"="steelblue"))
print(p)