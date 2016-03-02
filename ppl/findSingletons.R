source('relativePosition.R')
find.singletons <- function(points) {
  distances <- distances(points)
  singletons <- c()
  for (i in 1:nrow(points)) {
    # direct neighbors
    x.distances <- distances[,i]
    (further.neighbors <- setdiff(which(x.distances <= sqrt(5)), i))
    if (length(further.neighbors) <= 4) {
      neighbors <- setdiff(which(x.distances <= sqrt(2)), i)
      neighbors.positions <- na.omit(sapply(neighbors, function(n) {
        relative.position(points[i,], points[n,])
      }))
      if ((max(neighbors.positions) - min(neighbors.positions)) < 2) {
        # also check it doesn't have neighbors in both directions
        singletons <- append(singletons, points[i,])
      }
    }
  }
  if (length(singletons) == 0) {
    dist.from.ori <- dist.from.origin(points)
    nearest <- points[which.min(dist.from.ori),]
    singletons <- nearest
  }
  return(matrix(singletons, ncol = 2, nrow = length(singletons)/2, byrow = TRUE))
}
