curr.int <- all.thinned.ints[[1]]
plot(curr.int$points, pch = 19)
dist.from.origin <- function(points) {
  (points[1] - points[2])**2
}
nearest.origin <- function(points) {
  dists.from.origin <- apply(points,1, dist.from.origin)
}
plot.point(, color = 'blue')
