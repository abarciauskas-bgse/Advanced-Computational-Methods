# if has a solid side and missing opposite side, delete it
on.periphery <- function(current.point, pixels) {
  left <- !is.na(row.match(current.point-c(1,0), pixels))
  right <- !is.na(row.match(current.point+c(1,0), pixels))
  bottom <- !is.na(row.match(current.point-c(0,1), pixels))
  top <- !is.na(row.match(current.point+c(0,1), pixels))
  top.left <- !is.na(row.match(current.point-c(1,-1), pixels))
  top.right <- !is.na(row.match(current.point+c(1,1), pixels))
  bottom.left <- !is.na(row.match(current.point-c(1,1), pixels))
  bottom.right <- !is.na(row.match(current.point+c(1,-1), pixels))
  
  on.periphery <- FALSE
  
  # surrounded on top?
  if ((top.left && top && top.right) && sum((bottom.left && bottom && bottom.right) <= 1)) {
    on.periphery <- TRUE
  }
  
  # surrounded on right?
  if ((top.right && right && bottom.right) && sum((top.left && left && bottom.left) <= 1)) {
    on.periphery <- TRUE
  }
  
  # surrounded on bottom?
  if ((bottom.left && bottom && bottom.right) && sum((top.left && top && top.right) <= 1)) {
    on.periphery <- TRUE
  }
  
  # surrounded on left?
  if ((top.left && left && bottom.left) && sum((top.right && right && bottom.right) <= 1)) {
    on.periphery <- TRUE
  }
  
  return(on.periphery)
}
