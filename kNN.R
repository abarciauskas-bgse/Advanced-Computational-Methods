# Comments
# genGaussMix

k = 1
p = 2

noObs <- nrow(dataset)
distMatrix <- matrix(NA, nObs, nObs)
features <- as.matrix(dataset[,1:2])
for (obs in 1:nObs) {
  point <- dataset[obs,1:2]
  pointMatrix <- matrix(point, nrow=nObs, ncol=2, byrow=TRUE)
  rowSums((abs(features-pointMatrix))**p)
}

# did something else ^^ to make it work computin gthe distances between the probe and exemplars in memory

distances <- apply(distMatrix,1,order)
# produces columns
distances[,1]