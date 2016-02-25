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

miss.error <- function(prob) {
  miss.error <- 1 - prob
  return(miss.error)
}

gini <- function(prob) {
  gini <- prob*(1-prob)
  return(gini)
}

cross.entropy <- function(prob) {
  cross.entropy <- -prob*log(prob)
  return(cross.entropy)
}

prob.k <- function(meank, actual) {
  length(meank == actual)/length(actual)
}

findThreshold <- function(X, Y, costFnc = 'Entropy') {
    
    X <- as.matrix(X)
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
          # we can just go point by point
          x.feature.value <- X[x.idx,m.idx]
          potThres <- x.feature.value

          # check the classification error, when both sides, 
          # are classified with mean label
          predictedClasses <- rep(NA, noPoints)
          Y.left <- Y[X[,m.idx] < potThres]
          Y.right <- Y[X[,m.idx] >= potThres]
          modeLeft <- my.mode(Y.left)
          modeRight <- my.mode(Y.right)
          predictedClasses[X[,m.idx] <= potThres] <- modeLeft
          predictedClasses[X[,m.idx] > potThres] <- modeRight
          # error of this split
          # for each side need to calculate the probability of
          # the mode class being used for classification
          prob.k.left <- prob.k(modeLeft, Y.left)
          prob.k.right <- prob.k(modeRight, Y.right)
          misError <- if (costFnc == 'ME') {
            miss.error(prob.k.left) + miss.error(prob.k.right)
          } else if (costFnc == 'Gini') {
            gini(prob.k.left) + gini(prob.k.right)
          } else if (costFnc == 'Entropy') {
            cross.entropy(prob.k.left) + cross.entropy(prob.k.right)
          }
          
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

# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
#
cTree <- function(K, X, Y, minPoints = 1, costFnc = 'Entropy') {

    # setting up the initial boundaries - whole interval, with each
    # iteration of k this will expand
    boundaries <- c(0, 1)

    # iterating for K times, i.e. we iteratively split input space in an 
    # empirically greedy way, keeping only the best split and adding it 
    # to the boundaries, we stop after doing this K times
    for (k in 1:K) {

        # first we subset our input space according to the boundaries 
        # found so far
        intervals <- cut(X, boundaries, include.lowest = TRUE)
        noIntervals <- length(levels(intervals))

        # then we go over each subset and see what is the best splitting
        # point locally in this subset, using the findThreshold function
        thresholds <- rep(NA, noIntervals)
        errors <- rep(NA, noIntervals)
        splitLabels <- matrix(NA, ncol=2, nrow=noIntervals)
        for (iter in 1:noIntervals) {
            x <- X[intervals==levels(intervals)[iter]]
            y <- Y[intervals==levels(intervals)[iter]]
            # we skip if there is a single element in the interval
            # nothing to split there
            if (length(y)>minPoints) {
                # find the local splitting point
                results <- findThreshold(x, y, costFnc)
                thresholds[iter] <- results$thres
                splitLabels[iter,] <- results$labels

                # add the potential threshold to our list of boundaries
                boundariesHH <- c(boundaries, abs(results$thres))
                boundariesHH <- sort(boundariesHH)

                # add the signs of the new threshold which indicates what 
                # is the label of the of newly splitted interval
                if (k==1) {
                    signsHH <- results$labels
                } else {
                    signsHH <- append(signs, results$labels[1], 
                        after=which(boundariesHH==results$thres)-2)
                    signsHH[which(boundariesHH==results$thres)] <- 
                        results$labels[2]
                }

                # now we compute predictions with new boundaries based on the 
                # potential split
                predictedClasses <- cut(X, boundariesHH)
                levels(predictedClasses) <- signsHH 

                # we compute a global, overall error rate for this local
                # modification, we do not use the local error to evaluate 
                # the splitting point
                errors[iter] <- mean(predictedClasses != Y)
            }
        }

        # find the best threshold in this iteration, greedy strategy
        minError <- min(errors, na.rm=TRUE)
        bestThreshold <- thresholds[which(errors==minError)]
        bestThreshold <- sample(bestThreshold, 1)
        labels <- splitLabels[which(thresholds==bestThreshold),]

        # add the new threshold to our list of boundaries
        boundaries <- c(boundaries, abs(bestThreshold))
        boundaries <- sort(boundaries)

        # add the signs of the new threshold which indicates what is the label of the newly splitted interval
        if (k==1) {
            signs <- labels
        } else {
            signs <- append(signs, labels[1], 
                after=which(boundaries==bestThreshold)-2)
            signs[which(boundaries==bestThreshold)] <- labels[2]
        }
    }

    # get the final predicted classes
    predictedClasses <- cut(X, boundaries)
    levels(predictedClasses) <- signs 

    # now we evaluate the final accuracy, after K iterations
    misError <- mean(predictedClasses != Y)


    return(list(predictedClasses = predictedClasses, 
                misError = misError,
                boundaries = boundaries,
                signs = signs))
}
