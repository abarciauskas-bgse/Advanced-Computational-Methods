if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if (!require('assertthat')) install.packages('assertthat')
library(assertthat)
if (!require('Rcpp')) install.packages('Rcpp')
library(Rcpp)

Rcpp::sourceCpp('distance.cpp')

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# `kNN` returns the predicted labels of the features argument.
# It requires the following arguments:
#   `features`: dataframe or matrix of feature values
#   `labels`: vector of labels
# The following arguments are optional:
#   `train.set`: a dataframe or matrix of feature values to use as the training set.
#                The labels argument is used in training the classifier.
#   `k`: The number of neighbors to use in assigning a probabilistic label
#        Defaults to 1.
#   `p`: The dimension of the distance metric
#        Defaults to 2, the Euclidian distance
#   `type`: The type of classification to run.
#           Defaults to train, which runs training and predictions on the argment passed as features.
#           If 'predict', `train.set` is used to train the classifier and labels of features are predicted using `train.set`
#
kNN <- function(features, labels, train.set = NULL, 
                k = 1, p = 2, type = "train") {
    
    # test the inputs
    library(assertthat)
    not_empty(features); not_empty(labels); 
    if (type == "train") {
        assert_that(nrow(features) == length(labels))
    }
    is.string(type); assert_that(type %in% c("train", "predict"))
    is.count(k); 
    assert_that(p %in% c(1, 2, Inf))
    if (type == "predict") {
        assert_that(not_empty(train.set) & 
                    ncol(train.set) == ncol(features) & 
                    nrow(train.set) == length(labels))
    }

    # Compute the distance between each point and all others 
    noObs <- nrow(features)
    
    # if we are making predictions on the test set based on the train.set, 
    # we compute distances between each test observation and observations
    # in our train.set
    if (type == "train") {
        # distMatrix is noObs x noObs and will hold the distance between each observation and every other in every column and row
        distMatrix <- matrix(NA, noObs, noObs)
        for (obs.idx in 1:noObs) {
            
            # values of the current observation
            x.current <- as.numeric(features[obs.idx,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of noObs x ncol(x.current) (i.e. no.observations x no.features)
            x.current.expanded <- matrix(x.current, nrow = noObs, ncol = 2, byrow = TRUE)

            # computing distances between the current x and everyone else in the training set
            #
            if (p %in% c(1,2)) {
                # computes the by-row (i.e. by-observation) distance from x.current (which is has index obs.idx)
                distMatrix[obs.idx, ] <- calcDist(as.matrix(features), x.current.expanded, p)
            } else if (p==Inf) {
                distMatrix[obs.idx, ] <- apply(abs(features - x.current.expanded), 1, max)
            }  
        }
    } else if (type == "predict") {
        notrain.set <- nrow(train.set)
        distMatrix <- matrix(NA, noObs, notrain.set)
        for (obs.idx in 1:noObs) {
           
            # feature values for the current observation
            x.current <- as.numeric(features[obs.idx,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of notrain.set (which now behaves as the training set) x ncol(x.current)
            x.current.expanded <- matrix(x.current, nrow = notrain.set, ncol = 2, 
                                    byrow = TRUE)

            # computing distances between the probe and exemplars in the train.set
            if (p %in% c(1,2)) {
                distMatrix[obs.idx, ] <- (rowSums((abs(train.set - 
                                      x.current.expanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs.idx, ] <- apply(abs(train.set - x.current.expanded), 1, max)
            }  
        }
    }
    
    # Sort the distances in increasing numerical order and pick the first 
    # k elements
    neighbors <- apply(distMatrix, 1, order) 

    # Compute and return the most frequent class in the k nearest neighbors
    predLabels <- rep(NA, noObs)
    prob <- rep(NA, noObs)
    for (obs.idx in 1:noObs) {
        label <- my.mode(as.vector(labels[neighbors[1:k, obs.idx]]))
        predLabels[obs.idx] <- as.integer(label)

        # sum the total neighbors predicting the predicted label
        prob[obs.idx] <- sum(labels[neighbors[1:k,obs.idx]] == predLabels[obs.idx])/k
    }

    # examine the performance, available only if training
    if (type == "train") {
        errorCount <- table(predLabels, labels)
        accuracy <- mean(predLabels == labels)
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
