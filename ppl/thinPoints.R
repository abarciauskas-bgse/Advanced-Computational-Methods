source('../ppl/onPeriphery.R')

thin.points <- function(current.point, white.pixels, animation = FALSE) {
  white.pixels.thinned <- white.pixels
  unvisited <- white.pixels
  max.iter <- length(white.pixels)
  iter <- 0
  while (!is.null(nrow(unvisited)) > 0 && iter < max.iter) {
    iter <- iter + 1
    if (animation) plot.point(current.point, color = 'blue')
    if (animation) Sys.sleep(0.2)

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
      (on.the.edge <- (current.point[1] == min.x ||
                       current.point[1] == max.x ||
                       current.point[2] == min.y ||
                       current.point[2] == max.y))
      
      if (!(on.the.edge) &&
          !(is.na(left) && is.na(right)) &&
          !(is.na(top) && is.na(bottom)) &&
          !(is.na(top.left) && is.na(bottom.right)) &&
          !(is.na(top.right) && is.na(bottom.left))) {
        if (on.periphery(current.point, white.pixels.thinned)) {
          white.pixels.thinned <- white.pixels.thinned[-position,]
        }
      }
      
      if (animation) plot(white.pixels.thinned,pch=19)
      if (animation) Sys.sleep(0.2)
      nrow(white.pixels.thinned)
      # move to another point
      r <- row.match(current.point, unvisited)
      if (!is.na(r)) unvisited <- unvisited[-r,]
      if (!is.null(nrow(unvisited))) current.point <- unvisited[sample(1:nrow(unvisited), 1),]
    }
  }
  return(white.pixels.thinned)
}
