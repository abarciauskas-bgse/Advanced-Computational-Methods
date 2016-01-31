if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

kNN <- function(X, y, train.set = NULL, 
                k = 1, p = 2, type="train") {
    
    # test the inputs
    library(assertthat)
    not_empty(X); not_empty(y); 
    if (type == "train") {
        assert_that(nrow(X) == length(y))
    }
    is.string(type); assert_that(type %in% c("train", "predict"))
    is.count(k); 
    assert_that(p %in% c(1, 2, Inf))
    if (type == "predict") {
        assert_that(not_empty(train.set) & 
                    ncol(train.set) == ncol(X) & 
                    nrow(train.set) == length(y))
    }

    # Compute the distance between each point and all others 
    noObs <- nrow(X)
    
    # if we are making predictions on the test set based on the train.set, 
    # we compute distances between each test observation and observations
    # in our train.set
    if (type == "train") {
        distMatrix <- matrix(NA, noObs, noObs)
        for (obs in 1:noObs) {
            
            # values of the current observation
            x.current <- as.numeric(X[obs,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of noObs x ncol(x.current)
            x.current.expanded <- matrix(x.current, nrow = noObs, ncol = 2, byrow = TRUE)

            # computing distances between the current x and everyone else in the training set
            #
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(X - 
                                      x.current.expanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(X - x.current.expanded), 1, max)
            }  
        }
    } else if (type == "predict") {
        notrain.set <- nrow(train.set)
        distMatrix <- matrix(NA, noObs, notrain.set)
        for (obs in 1:noObs) {
           
            # feature values for the current observation
            x.current <- as.numeric(X[obs,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of notrain.set (which now behaves as the training set) x ncol(x.current)
            x.current.expanded <- matrix(x.current, nrow = notrain.set, ncol = 2, 
                                    byrow = TRUE)

            # computing distances between the probe and exemplars in the train.set
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(train.set - 
                                      x.current.expanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(train.set - x.current.expanded), 1, max)
            }  
        }
    }
    
    # Sort the distances in increasing numerical order and pick the first 
    # k elements
    neighbors <- apply(distMatrix, 1, order) 

    # Compute and return the most frequent class in the k nearest neighbors
    predLabels <- rep(NA, noObs)
    prob <- rep(NA, noObs)
    for (obs in 1:noObs) {
        label <- my.mode(as.vector(y[neighbors[1:k, obs]]))
        predLabels[obs] <- as.integer(label)

        # sum the total neighbors predicting the predicted label
        prob[obs] <- sum(y[neighbors[1:k,obs]] == predLabels[obs])/k
    }

    # examine the performance, available only if training
    if (type == "train") {
        errorCount <- table(predLabels, y)
        accuracy <- mean(predLabels == y)
    } else if (type == "predict") {
        errorCount <- NA
        accuracy <- NA
    }

    # return the results
    return(list(predLabels = predLabels, 
                prob = prob,
                accuracy = accuracy,
                errorCount = errorCount))
}
