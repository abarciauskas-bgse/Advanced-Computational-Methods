require(mvtnorm)

intervals_2D <- function(noObs, intervals, epsilon = 0.5, 
                         seed = round(runif(1)*100000)) {
  set.seed(seed)
  
  # create 1D input points, where each observation is drawn independently
  # from the uniform distribution, interval from 0 to 1
  X <- rmvnorm(noObs, mean = c(0,0))#, sigma = matrix(c(1,0.7,0.7,1), nrow=2, ncol=2))
  
  # create intervals on each feature
  # intervals should be randomly dispersed in the feature range
  # so if the range of one feature is from (-3,3)
  # each item in thresholds is feature index, threshold value, label or threshold left,
  # label or threshold right
  Y <- rep(NA, nrow(X))
  thresholds <- matrix(NA, nrow = 3, ncol = 4)
  colnames(thresholds) <- c('feature.idx','threshold.val','label.left','label.right')
  thresholds[1,] <- c(2, 0.2, 0, 1)
  thresholds[2,] <- c(2, -0.5, 1, 0)
  thresholds[3,] <- c(2, 0.0, 0, 1)
  for (x.idx in 1:noObs) {
    for (thres.idx in 1:nrow(thresholds)) {
      thresh <- thresholds[thres.idx,]
      if (is.na(Y[x.idx]) && (X[x.idx,thresh[1]] > thresh[2])) {
        Y[x.idx] <- thresh[3]
      } else {
        Y[x.idx] <- thresh[4]
      }
    }
  }
  Y
  
  return(list(X=X, Y=Y))
}
