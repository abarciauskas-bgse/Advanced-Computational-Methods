#setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS5/')
source('findThreshold.R')
if (!require('formula.tools')) install.packages('formula.tools')

# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
#
# FIXME: add depth
# FIXME: add minPoints
cTree <- function(formula, data, depth = 3, minPoints = 1, costFnc = 'Entropy') {
    Y.var <- get.vars(lhs(formula))
    X.vars <- get.vars(rhs(formula))
    Y <- data[,Y.var]
    X <- data[,X.vars]

    thresholds <- matrix(NA, ncol = 5)
    # TODO: add minPoints
    thresholds <- findThresholds(X, Y, thresholds = thresholds)

    # FIXME: remove NA
    new.thresholds <- thresholds[2:nrow(thresholds),]
    
    preds <- rep(NA, length(Y))
    for (i in 1:nrow(new.thresholds)) {
      (threshold <- new.thresholds[i,])
      (threshold.value <- new.thresholds[i,1])
      (feature <- new.thresholds[i,2])
      
      rhs <- which(X[,feature] > threshold.value)
      lhs <- which(X[,feature] <= threshold.value)
      lapply(rhs, function(x) {
        if (is.na(preds[x])) {
          preds[x] <<- new.thresholds[i,4]
        }
      })
      lapply(lhs, function(x) {
        if (is.na(preds[x])) {
          preds[x] <<- new.thresholds[i,3]
        }
      })
    }
    # Now to cut up the data
    return(list(predLabels = preds, prob = new.thresholds[,5]))
}


res <- cTree(formula = as.formula('Species ~ Petal.Width + Sepal.Width'),data =iris)
