relative.position <- function(point1, point2) {
  position <- NA
  if (all(point1-c(1,-1) == point2)) {
    position <- 1
  } else if (all(point1+c(0,1) == point2)) {
    position <- 2
  } else if (all(point1+c(1,1) == point2)) {
    position <- 3
  } else if (all(point1+c(1,0) == point2)) {
    position <- 4
  } else if (all(point1+c(1,-1) == point2)) {
    position <- 5
  } else if (all(point1-c(0,1) == point2)) {
    position <- 6
  } else if (all(point1-c(1,1) == point2)) {
    position <- 7
  } else if (all(point1-c(1,0) == point2)) {
    position <- 8
  }
  
  return(position)
}

if (!require('binhf')) install.packages('binhf')
# Return the point at relative position [1,8] to the current point
relative.pt <- function(pixels, current.point, position) {
  pt.idx <- NA
  if (position == 1) {
    pt.idx <- row.match(current.point-c(1,-1), pixels)
  } else if (position == 2) {
    pt.idx <- row.match(current.point+c(0,1), pixels)
  } else if (position == 3) {
    pt.idx <- row.match(current.point+c(1,1), pixels)
  } else if (position == 4) {
    pt.idx <- row.match(current.point+c(1,0), pixels)
  } else if (position == 5) {
    pt.idx <- row.match(current.point+c(1,-1), pixels)
  } else if (position == 6) {
    pt.idx <- row.match(current.point-c(0,1), pixels)
  } else if (position == 7) {
    pt.idx <- row.match(current.point-c(1,1), pixels)
  } else if (position == 8) {
    pt.idx <- row.match(current.point-c(1,0), pixels)
  }
  
  return(pixels[pt.idx,])
}

