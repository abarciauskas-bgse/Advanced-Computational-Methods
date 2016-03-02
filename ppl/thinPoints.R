thin.points <- function(current.point, white.pixels, visited = NA, animation = FALSE) {
  if (all(is.na(visited))) {
    visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
  }
  white.pixels.thinned <- white.pixels

  if (!is.na(current.point['x'])) {
    if (animation) plot.point(current.point, color = 'blue')
    if (animation) Sys.sleep(0.2)
    visited <- rbind(visited, current.point)
    # find 8 nearest neighbors
    distances <- apply(white.pixels, 1, function(x) { distance(current.point, x)})
    ordered.distances <- distances[order(distances, decreasing = FALSE)]
    eight.neighbors.position <- white.pixels[order(distances, decreasing = FALSE),][2:9,]
    four.nwse <- eight.neighbors.position[1:4,]
    four.corners <- eight.neighbors.position[5:8,]
    if (animation) apply(four.corners, 1, plot.point, color = 'yellow')
    if (animation) apply(four.nwse, 1, plot.point, color = 'orange')
    if (animation) Sys.sleep(0.2)
    (four.neighbors.nwse <- ordered.distances[2:5] == 1)
    (four.neighbors.corners <- ordered.distances[6:9] == sqrt(2))
    # Delete if two foreground neighbors or surrounded
    if (sum(four.neighbors.nwse) == 3 || sum(four.neighbors.nwse) == 2) {
      # delete it from thinned
      position <- row.match(current.point,white.pixels.thinned)
      if (!is.na(position)) {
        if (animation) plot.point(white.pixels.thinned[position,], color = 'red')
        if (animation) Sys.sleep(0.2)
        # only do this if it's not creating a SCHISM
        # e.g. if it's breaking a line btw two corners
        # KEEP IF 
        #  - If 1 and 3 of nwse are both missing OR
        #  - If 2 and 4 of nwse are both missing
        left <- row.match(current.point-c(1,0), white.pixels.thinned)
        right <- row.match(current.point+c(1,0), white.pixels.thinned)
        bottom <- row.match(current.point-c(0,1), white.pixels.thinned)
        top <- row.match(current.point+c(0,1), white.pixels.thinned)
        top.left <- row.match(current.point-c(1,-1), white.pixels.thinned)
        top.right <- row.match(current.point+c(1,1), white.pixels.thinned)
        bottom.left <- row.match(current.point-c(1,1), white.pixels.thinned)
        bottom.right <- row.match(current.point+c(1,-1), white.pixels.thinned)
        
        # being on the edge means the current point has a min or max y or x value
        min.x <- min(white.pixels[,1])
        min.y <- min(white.pixels[,2])
        max.x <- max(white.pixels[,1])
        max.y <- max(white.pixels[,2])
        (on.the.edge <- (current.point[1] == min.x || current.point[1] == max.x || current.point[2] == min.y || current.point[2] == max.y))
        
        if (!(is.na(left) && is.na(right)) &&
            !(is.na(top) && is.na(bottom)) &&
            !(is.na(top.left) && is.na(bottom.right)) &&
            !(is.na(top.right) && is.na(bottom.left))) {
          white.pixels.thinned <- white.pixels.thinned[-position,]
        }
        
        if (animation) plot(white.pixels.thinned,pch=19)
        if (animation) Sys.sleep(0.2)
        nrow(white.pixels.thinned)
        # move to a neighbor
      }
    }
    next.pos <- four.nwse
  }
  return(white.pixels.thinned)
}
