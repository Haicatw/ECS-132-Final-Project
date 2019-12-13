library(regtools)
library(ggplot2)
data(day1)

rearrange <- function(X, y, k) {
    mat <- cbind(X, y)
    #print(head(X))
    #print(head(y))
    #print(head(mat))
    nrows <- dim(mat)[1]
    ncols <- dim(mat)[2]
    rearrangedMat <- c()#matrix(0, nrow=(nrows-k), ncol=(ncols-1)*k+1)
    #print(dim(rearrangedMat))
    for (i in 1:(nrows - k)) {
        rowdata <- X[i,]
        for (j in 1:(k-1)) {
            rowdata <- c(rowdata, X[i+j,])
        }
        rowdata <- c(rowdata, y[i+k])
        rowdata <- t(as.matrix(rowdata))
        #print(rowdata)
        #print(dim(rowdata))
        #print(rearrangedMat[i,])
        rearrangedMat <- rbind(rearrangedMat, rowdata[1, ])
    }
    return (rearrangedMat)
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

    train_set <- dataset[indeces, ]
    test_set <- dataset[-indeces, ]
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

data <- data.frame(day1[, c(9:13)])
cat("Use normalized season, yr, mnth, workingday, weatherist, temp, atemp, hum and windspeed to predict", "\n", sep="")
data <- train_test_split(data, 631)
train <- data$train
test <- data$test



eval_grid_search <- function(train_x, test_x, train_y, test_y, para_range, predict_name) {
    k_val_list <- c()
    mse_list <- c()
    train_mse_list <- c()

    for (i in 2:5) {
        linearMod <- lm(train_y[,1][1:i] ~ train_x[,1][1:i] + train_x[,2][1:i] + train_x[,3][1:i] + train_x[,4][1:i] + train_x[,5][1:i])
        cfs <- coef(linearMod)
        pred_y <- c()
        for (j in 1:nrow(test_x)) {
            x <- c(1)
            x <- c(x, as.numeric(test_x[j,]))
            y <- cfs %*% x
            pred_y <- c(pred_y, y)
            x <- 0
        }
    }
        y_test <- as.numeric(as.character(unlist(test_y[[1]])))
        mse <- Mean_square_error(y_test, pred_y)
        mse_list <- c(mse_list, mse)

    
        pred_y <- c()
        for (j in 1:i) {
            x <- c(1)
            x <- c(x, as.numeric(train_x[,1][1:i]))
            y <- cfs %*% x
            pred_y <- c(pred_y, y)
            x <- 0
        }
        mse <- Mean_square_error(train_y[,1][1:i], pred_y)
        train_mse_list <- c(train_mse_list, mse)
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



# weather
train_x <- normalize(train[c(1,2,3,4,6,7,8,9)])
train_y <- train[5]
test_x <- normalize(test[c(1,2,3,4,6,7,8,9)])
test_y <- test[5]

eval_grid_search(train_x, test_x, train_y, test_y, 300, "weather")

# windspeed
train_x <- normalize(train[c(1,2,3,4,5,6,7,8)])
train_y <- train[9]
test_x <- normalize(test[c(1,2,3,4,5,6,7,8)])
test_y <- test[9]
eval_grid_search(train_x, test_x, train_y, test_y, 300, "windspeed")

# hum
train_x <- normalize(train[c(1,2,3,4,5,6,7,9)])
train_y <- train[8]
test_x <- normalize(test[c(1,2,3,4,5,6,7,9)])
test_y <- test[8]
eval_grid_search(train_x, test_x, train_y, test_y, 300, "hum")

#temp
train_x <- normalize(train[c(1,2,3,4,5,7,8,9)])
train_y <- train[6]
test_x <- normalize(test[c(1,2,3,4,5,7,8,9)])
test_y <- test[6]
eval_grid_search(train_x, test_x, train_y, test_y, 300, "temp")

#atemp
train_x <- normalize(train[c(1,2,3,4,5,6,8,9)])
train_y <- train[7]
test_x <- normalize(test[c(1,2,3,4,5,6,8,9)])
test_y <- test[7]
eval_grid_search(train_x, test_x, train_y, test_y, 300, "atemp")

