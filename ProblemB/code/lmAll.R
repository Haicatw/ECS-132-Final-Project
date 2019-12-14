library(regtools)
library(ggplot2)
data(day1)

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

generatePolynomialInput <- function(power, X) {
    X <- apply(X, 2, unlist)
    processed_X <- X
    if (power == 1) {
        return (X)
    }
    for (i_p in 2:power) {
        poweredX <- X
        for (i_pm in 2:i_p) {
            poweredX <- poweredX * X
        }
        processed_X <- cbind(processed_X, poweredX)
    }
    return (processed_X)
}

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

eval_grid_search <- function(feature_colnames, y_name, max_k, max_power, predict_name) {
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
            #test_X <- generatePolynomialInput(p, test_X)
            converted_train_X <- apply(as.matrix(train_X), 2, as.numeric)
            test_X <- generatePolynomialInput(p, test_X)
            converted_test_X <- apply(as.matrix(test_X), 2, as.numeric)

            lmout <- lm(train_y ~ ., data=data.frame(converted_train_X))
            print(summary(lmout))

            predict_y <- predict(lmout, newdata=data.frame(converted_test_X))
            # allCoefVect <- coef(lmout)
            # input_X <- cbind(rep(1, dim(test_X)[1]), generatePolynomialInput(p, test_X))
            # predict_y <- as.matrix(input_X) %*% as.matrix(allCoefVect)
            mse <- Mean_square_error(predict_y, test_y)
            test_mse_list_p <- c(test_mse_list_p, mse)

            predict_y <- predict(lmout, newdata=data.frame(converted_train_X))
            # allCoefVect <- coef(lmout)
            # input_X <- cbind(rep(1, dim(train_X)[1]), generatePolynomialInput(p, train_X))
            # predict_y <- as.matrix(input_X) %*% as.matrix(allCoefVect)
            mse <- Mean_square_error(predict_y, train_y)
            train_mse_list_p <- c(train_mse_list_p, mse)            
        }
        train_mse_list <- c(train_mse_list, train_mse_list_p)
        test_mse_list <- c(test_mse_list, test_mse_list_p)
    }
    #print(test_mse_list)
    train_mse_list <- matrix(data=train_mse_list, nrow=max_power, ncol=max_k-1)
    test_mse_list <- matrix(data=test_mse_list, nrow=max_power, ncol=max_k-1)
    train_mse_list[is.na(train_mse_list)] <- 1
    test_mse_list[is.na(test_mse_list)] <- 1
    plot_title = paste("K Value vs Mean Squared Error of ", predict_name, " predictor.", sep="")
    #print(test_mse_list)
    best_paras <- which(test_mse_list == min(test_mse_list), arr.ind = TRUE)
    #print(best_paras)
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
            values = c("First Order Train MSE"="dodgeblue4", "Second Order Train MSE"="gray3", "Third Order Train MSE"="greenyellow", "First Test MSE"="chocolate4", "Second Test MSE"="maroon4", "Third Test MSE"="peru"))
    print(p)
}


max_k_val = 7
max_power_val = 3
# Predict temp
eval_grid_search(c("season", "mnth", "weathersit", "atemp", "hum", "windspeed"), c("temp"), max_k_val, max_power_val, "temp")

# Predict atemp
eval_grid_search(c("season", "mnth", "weathersit", "temp", "hum", "windspeed"), c("atemp"), max_k_val, max_power_val, "atemp")

# Predict hum
eval_grid_search(c("season", "mnth", "weathersit", "temp", "atemp", "windspeed"), c("hum"), max_k_val, max_power_val, "hum")

# Predict windspeed
eval_grid_search(c("season", "mnth", "weathersit", "temp", "atemp", "hum"), c("windspeed"), max_k_val, max_power_val, "windspeed")
