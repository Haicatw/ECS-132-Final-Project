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
            #test_X <- generatePolynomialInput(p, test_X)
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
            #print(predict_y)
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
    #print(test_acc_list)
    train_acc_list <- matrix(data=train_acc_list, nrow=max_power, ncol=max_k-1)
    test_acc_list <- matrix(data=test_acc_list, nrow=max_power, ncol=max_k-1)
    train_acc_list[is.na(train_acc_list)] <- 1
    test_acc_list[is.na(test_acc_list)] <- 1
    plot_title = paste("K Value vs Accuracy of ", predict_name, " predictor.", sep="")
    #print(test_acc_list)
    best_paras <- which(test_acc_list == max(test_acc_list), arr.ind = TRUE)
    #print(best_paras)
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

