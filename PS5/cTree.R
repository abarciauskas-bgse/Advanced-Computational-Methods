setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS5/')
source('findThreshold.R')

# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
#
cTree <- function(K, X, Y, minPoints = 1, costFnc = 'Entropy') {

    # setting up the initial boundaries - whole interval, with each
    # iteration of k this will expand
    # need to set boundaries according to the m dimensions
    boundaries <- NA

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
        (minError <- min(errors))
        bestThreshold <- thresholds[which(errors==minError)]
        bestThreshold <- bestThreshold[1]
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
