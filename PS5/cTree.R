# lets first develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut
# X is a an n x m matrix of a multi-dimensional input
# Y is a vector of k classes
# X.distances: matrix of distances
library(mvtnorm)

euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

findThreshold <- function(X, Y) {
    
    noPoints <- nrow(X)
    errors <- matrix(NA, nrow=noPoints-1, ncol=ncol(X))
    thresholds <- matrix(NA, nrow=noPoints-1, ncol=ncol(X))
    # splitLabels should hold the labels for each split and feature option
    # Is it okay this has only two options? e.g. each split picks majority on either half?
    splitLabels <- matrix(NA, ncol=2, nrow=noPoints-1)

    for (m.idx in 1:ncol(X)) {
      # we go sequentially over each point and cut between that point and threshold
      # closest neighbor
      for (x.idx in 1:(noPoints-1)) {

          # locate a potential threshold, a split between two points
          # find nearest neighbor along the current feature dimension
          # ok so this is actually unnecessary, we can just go point by point
          x.feature.value <- X[x.idx,m.idx]
          potThres <- x.feature.value

          # check the classification error, when both sides, 
          # are classified with mean label
          predictedClasses <- rep(NA, noPoints)
          (meanLeft <- my.mode(Y[X[,m.idx] < potThres]))
          (meanRight <- my.mode(Y[X[,m.idx] >= potThres]))
          predictedClasses[X[,m.idx] <= potThres] <- meanLeft
          predictedClasses[X[,m.idx] > potThres] <- meanRight
          # error of this split
          misError <- mean(predictedClasses != Y)
          
          # recording the accuracy, thresholds and labels of 
          # the splitted interval
          errors[x.idx,m.idx] <- misError
          thresholds[x.idx,m.idx] <- potThres
          splitLabels[x.idx,] <- c(predictedClasses[X[,m.idx] < potThres][1],
                                   predictedClasses[X[,m.idx] > potThres][1])
      }           
    }
    # print(cbind(errors, thresholds, splitLabels))

    # next we find the minimum and the best threshold
    minError <- min(na.omit(errors))
    bestThresholds <- which(errors==minError, arr.ind = TRUE)
    bestThreshold <- bestThresholds[sample(nrow(bestThresholds),1),]
    # if more than 1 threshold has the same accuracy we choose one randomly

    # what are the final labels of the best split?
    labels <- splitLabels[as.numeric(bestThreshold['row']),]

    return(list(thres = thresholds[bestThreshold['row'],bestThreshold['col']],
                # split on which feature
                feature = bestThreshold['col'],
                err = minError, 
                labels = labels))
}


X <- rmvnorm(300, mean = c(-3,0,0.5), sigma = diag(3))
Y <- sample(rep(c(1,3,3), 100))

(res <- findThreshold(X, Y))
