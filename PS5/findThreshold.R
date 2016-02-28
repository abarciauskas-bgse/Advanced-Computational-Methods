# lets first develop a function that will partition the input space, 
# cut the input space exhaustively, count errors for each cut and
# choose the best cut
# X is a an n x m matrix of a multi-dimensional input
# Y is a vector of k classes
findThreshold <- function(X, Y, costFnc = 'Entropy') {
  
  if (typeof(X) == 'list') {
    X <- as.matrix(X[[1]])
  } else {
    X <- as.matrix(X)
  }
  noPoints <- nrow(X)
  # FIXME: this should be noPoints - 1
  errors <- matrix(NA, nrow=(noPoints-1), ncol=ncol(X))
  thresholds <- matrix(NA, nrow=(noPoints-1), ncol=ncol(X))
  # splitLabels should hold the labels for each split and feature option
  # Is it okay this has only two options? e.g. each split picks majority on either half?
  splitLabels <- matrix(NA, ncol=2, nrow=noPoints*ncol(X))
  # classes in Y
  kclasses <- unique(Y)
  # number of classes in Y
  nclasses <- length(kclasses)

  if (nclasses > 1) {
    # determine all the options of splits and their corresponding errors
    for (m.idx in 1:ncol(X)) {
      # we go sequentially over each point and cut between that point and threshold
      # closest neighbor
      for (x.idx in 1:(noPoints-1)) {
        # locate a potential threshold, a split between two points
        # we can just go point by point
        (x.feature.value <- X[x.idx,m.idx])
        (potThres <- x.feature.value + 1e-6)
        # check the classification error, when both sides, 
        # are classified with mean label
        predictedClasses <- rep(NA, noPoints)
        
        # depending on the loss function,
        # take the minimum of the missclassification error for each possible class
        (Y.left <- Y[X[,m.idx] < potThres])
        (Y.right <- Y[X[,m.idx] >= potThres])

        # calculate the probability for each class
        class.probs.left <- c()
        class.probs.right <- c()
        for (class.idx in 1:nclasses) {
          k <- kclasses[class.idx]
          class.probs.left <- append(class.probs.left, sum(Y.left == k)/length(Y.left))
          class.probs.right <- append(class.probs.right, sum(Y.right == k)/length(Y.right))
        }

        # declare winners for this split
        (max.prob.left <- class.probs.left[which.max(class.probs.left)])
        (left.class <- kclasses[which.max(class.probs.left)])
        (max.prob.right <- class.probs.right[which.max(class.probs.right)])
        (right.class <- kclasses[which.max(class.probs.right)])
        (predictedClasses[X[,m.idx] < potThres] <- left.class)
        (predictedClasses[X[,m.idx] >= potThres] <- right.class)
        
        # calculate misError according to the loss function argument
        # for each of k classes on each side, pick the class which maximizes the information gain
        misError.left <- NA
        misError.right <- NA
        misError <- NA
  
        if (costFnc == 'ME') {
          misError.left <- 1 - max.prob.left
          misError.right <- 1 - max.prob.right
        } else if (costFnc == 'Gini') {
          misError.left <- sum(class.probs.left*(1-class.probs.left))
          misError.right <- sum(class.probs.right*(1-class.probs.right))
        } else if (costFnc == 'Entropy') {
          class.probs.left <- sapply(class.probs.left, function(x) {
            if (!is.nan(x) && x == 1) {
              x-1e-6
            } else if (!is.nan(x) && x == 0) {
              x+1e-6
            } else {
              x
            }
          })
          class.probs.right <- sapply(class.probs.right, function(x) {
            if (!is.nan(x) && x == 1) {
              x-1e-6
            } else if (!is.nan(x) && x==0) {
              x+1e-6
            } else {
              x
            }
          })
          (misError.left <- -sum(class.probs.left*log(class.probs.left)))
          (misError.right <- -sum(class.probs.right*log(class.probs.right)))
        }
        # should it be this or something else?
        (misError <- misError.left + misError.right)
        # If there is no error on the left or we are done digging,
        # assign the label
        label.left <- predictedClasses[X[,m.idx] < potThres][1]
        label.right <- predictedClasses[X[,m.idx] >= potThres][1]
        # recording the accuracy, thresholds and labels of 
        # the splitted interval
        errors[x.idx,m.idx] <- as.numeric(misError)
        thresholds[x.idx,m.idx] <- potThres
        splitLabels[(x.idx+m.idx),] <- c(label.left, label.right)
      }
    }
  }

  # next we find the minimum and the best threshold
  (minError <-  min(na.omit(errors)))
  # row and cols of thresholds which minimized the error
  (bestThresholds <- which(errors==minError, arr.ind = TRUE))
  #(bestThresholds <- as.matrix(bestThresholds))
  sample.thres <- sample(nrow(bestThresholds),1)
  # if more than 1 threshold has the same accuracy we choose one randomly
  bestThresholds.row <- bestThresholds[sample.thres,]['row']
  bestThresholds.col <- bestThresholds[sample.thres,]['col']
  (best.threshold.splitpoint <- thresholds[bestThresholds.row,bestThresholds.col])
  (best.threshold.feature <- as.numeric(bestThresholds.col['col']))
  
  # what are the final labels of the best split?
  labels <- splitLabels[(bestThresholds.row+best.threshold.feature),]
  
  return(list(min.feature = min(X[,best.threshold.feature]),
              max.feature = max(X[,best.threshold.feature]),
              thres = best.threshold.splitpoint,
              # split on which feature
              feature = best.threshold.feature,
              minerror = minError, 
              labels = labels))
}
