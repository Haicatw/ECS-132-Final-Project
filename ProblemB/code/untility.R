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

normalize <- function(x) {
    norm_x <- x
    for (i in 1:ncol(x)) {
        col <- (x[,i]- min(x[,i])) /(max(x[,i])-min(x[,i]))
        norm_x[,i] <- col
    }
    return(norm_x)
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
    processed_X <- X
    if (power == 1) {
        return (X)
    }
    for (i in 2:power) {
        processed_X <- cbind(processed_X, X^i)
    }
    return (processed_X)
}

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