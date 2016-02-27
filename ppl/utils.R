# color the point on the plot with given color
# defaults to cadetblue
# http://kktg.net/sgr/wp-content/uploads/colors1.png
# http://www.stat.columbia.edu/~tzheng/tianblog/uploaded_images/screenshots-755976.bmp
plot.point <- function(point, color = 'cadetblue') {
  points(point[1], point[2], col = color, pch = 19)
}

angle <- function(x, y) {
  x1 <- x[1]
  x2 <- x[2]
  y1 <- y[1]
  y2 <- y[2]  
  dot = x1*x2 + y1*y2      # dot product
  det = x1*y2 - y1*x2      # determinant
  angle = atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)
  return(angle)
}

rad2deg <- function(rad) {(rad * 180) / (pi)}

dist.from.origin <- function(white.pixels) {
  (white.pixels[,1] - white.pixels[,2])**2
}

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

distance <- function(point1, point2) {
  sqrt((point2[1] - point1[1])**2 + (point2[2] - point1[2])**2)
}

if (!require('Rcpp')) install.packages('Rcpp')
Rcpp::sourceCpp('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS4/distance.cpp')

distances <- function(matrix) {
  n <- nrow(matrix)
  dist.matrix <- matrix(NA, n, n)
  for (idx in 1:n) {
    x.current <- matrix[idx,]
    x.current.expanded <- matrix(x.current, nrow = n, ncol = 2, byrow = TRUE)
    dist.matrix[idx, ] <- calcDist(matrix, x.current.expanded, 2)
  }
  return(dist.matrix)
}

rotate <- function(x) t(apply(x, 2, rev))
