# generates a parse of a skeleton for strokes

# start at corner closest to origin
(current.point <- white.pixels.thinned[order(dist.from.origin(white.pixels.thinned))[1],])

# initialize pts.to.be.visited and strokes
pts.to.be.visited <- white.pixels.thinned
current.stroke.idx <- 1
strokes <- list(current.point)

lambda <- 0.1

# begin while still points to be be visited loop
# every point should be visited at least once
while (!is.na(pts.to.be.visited)[1]) {
  plot(white.pixels.thinned, pch = 19)
  # remove from pts.to.be.visited
  r <- row.match(current.point, pts.to.be.visited)
  if (!is.na(r)) pts.to.be.visited <- pts.to.be.visited[-r,]

  # limit to nearby neighbors
  distances <- apply(white.pixels.thinned, 1, function(x) { distance(current.point, x)})
  ordered.distances <- distances[order(distances, decreasing = FALSE)]
  eight.neighbors <- white.pixels.thinned[order(distances, decreasing = FALSE),][2:9,]

  # order angles in white.pixels.thinned
  angles <- apply(eight.neighbors, 1, function(x) {
    if (!all(current.point == x)) rad2deg(angle(current.point, x))
  })
  ordered.angles <- angles[order(angles, decreasing = FALSE)]
  # order points by angle
  points.ordered.by.angle <- eight.neighbors[order(angles, decreasing = FALSE),]
  points.ordered.by.angle <- points.ordered.by.angle[2:nrow(points.ordered.by.angle),]

  # generate random angle: rexp(1, rate = 0.05)
  random.angle <- rexp(1, rate = lambda)

  # find nearest angle which(abs(angles-random.angle)==min(abs(angles-random.angle)))
  (nearest.angle <- angles[which(abs(angles-random.angle)==min(abs(angles-random.angle)))][1])

  # find corresponding point (new point)
  (point.match <- which(ordered.angles == nearest.angle))
  if (length(point.match) > 0) {
    point.match <- sample(point.match, 1)
  }
  new.point <- points.ordered.by.angle[point.match,]

  # if angle is greater than 45, start new stroke
  if (length(new.point) > 0) {
    if (!all(new.point == current.point)) {
      if (nearest.angle > 45) {
        current.stroke.idx <- current.stroke.idx + 1
        strokes[[current.stroke.idx]] <- new.point
      # else append to current stroke
      } else {
        strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], new.point)
      }

      current.point <- new.point
    }
  }
}

stroke.1 <- matrix(strokes[[1]], ncol = 2, byrow=TRUE)
colnames(stroke.1) <- c('x','y')
#plot(stroke.1,pch=19)
apply(stroke.1, 1, plot.point)
