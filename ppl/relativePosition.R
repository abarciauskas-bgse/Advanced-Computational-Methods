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
  } else if (all(point1-c(0,1) == point2)) {
    position <- 7
  } else if (all(point1-c(1,0) == point2)) {
    position <- 8
  }
  
  return(position)
}
