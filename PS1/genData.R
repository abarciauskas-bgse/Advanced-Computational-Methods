# genData generates a 3-d binary-outcome dataset
# that linear models would have troubles with fitting well.
if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('rgl')) install.packages('rgl')
library(rgl)

# Inspired by https://archive.ics.uci.edu/ml/datasets/Madelon
# genData generates data from vertices of 3-d cube
# with alternating classifications of 0 and 1
#
# Arguments:
# `n`: (approximate) number of observations per vertex
# `min.vertex` and `max.vertex`:
#   - The lower corner of the cube is given by (min.vertex, min.vertex, min.vertex)
#   - The upper corner of the cube is given by (max.vertex, max.vertex, max.vertex)
# `var`: variance for each vertex
# `write.csv`: boolean whether to save data points to `dataset.csv`, defaults to TRUE
# `save.pdf`: boolean whether to save 3Dplot to pdf, defaults to TRUE
#
genData <- function(n = 100,
                    min.vertex = 1,
                    max.vertex = 2,
                    var = 0.05,
                    write.csv = TRUE,
                    save.pdf = TRUE) {
  # shorthand
  minv <- min.vertex
  maxv <- max.vertex

  numCenters <- 4
  m <- 4 # 3 features + 1 output
  
  zeroes <- matrix(nrow = 0, ncol = m)
  zeroes.means <- matrix(c(minv,minv,minv,maxv,minv,maxv,minv,maxv,maxv,maxv,maxv,minv), ncol = numCenters)
  rdraws <- runif(numCenters*n)
  rdraws.bincount <- as.vector(table(cut(rdraws, b = numCenters)))
  for (idx in 1:length(rdraws.bincount)) {
    # Random draw of distribution
    bin.means <- zeroes.means[,idx]
    bin.count <- rdraws.bincount[idx]
    bin.zeroes <- rmvnorm(bin.count, mean = bin.means, sigma = var*diag(3))
    bin.zeroes <- cbind(bin.zeroes, rep(0, bin.count))
    zeroes <- rbind(zeroes, bin.zeroes)
  }
  
  ones <- matrix(nrow = 0, ncol = m)
  ones.means <- matrix(c(maxv,minv,minv,minv,minv,maxv,minv,maxv,minv,maxv,maxv,maxv), ncol = numCenters)
  rdraws <- runif(numCenters*n)
  rdraws.bincount <- as.vector(table(cut(rdraws, b = numCenters)))
  for (idx in 1:length(rdraws.bincount)) {
    # Random draw of distribution
    bin.means <- ones.means[,idx]
    bin.count <- rdraws.bincount[idx]
    bin.ones <- rmvnorm(bin.count, mean = bin.means, sigma = var*diag(3))
    bin.ones <- cbind(bin.ones, rep(1, bin.count))
    ones <- rbind(ones, bin.ones)
  }
  
  df <- data.frame(rbind(zeroes, ones))
  if (write.csv == TRUE) write.table(df, file = "dataset.csv", row.names = FALSE)
  if (save.pdf == TRUE) {
    plot3d(df[,1:3], radius = 0.05, type='s', col = df[,4]+2)
    rgl.postscript("dataPlot.pdf","pdf")
  }
  df
}

# Example:
# genData(500, min.vertex = 0, max.vertex = 4, var = 0.5)
