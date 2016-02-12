if (!require('grid')) install.packages('grid')
if (!require('prodlim')) install.packages('prodlim')

setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/data/')
source('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/displayDigit.R')

digits <- read.csv('MNIST_training.csv')
digit <- digits[12,]
(label <- as.numeric(digit[1]))

# 2. the rest are pixel intensities for 16x16 image of digits
features <- as.numeric(digit[2:ncol(digit)])

# displaying the digit
displayDigit(features, label, newDevice = FALSE)

if (!require('splines')) install.packages('splines')
library(splines)

pixels <- matrix(features, 16, 16, byrow = TRUE)
rotate <- function(x) t(apply(x, 2, rev))
pixels <- rotate(pixels)

# plots just the pixels with some value
white.pixels <- which(pixels > 0, arr.ind = TRUE)
plot(white.pixels, pch = 19)

# subtract greatest y value from all y values
# so top is now x = 0 and we are in the 4th quadrant of xy
white.pixels[,2] <- white.pixels[,2] - max(white.pixels[,2])
colnames(white.pixels) <- c('x','y')

plot(white.pixels, pch = 19)

dist.from.origin <- (white.pixels[,1] - white.pixels[,2])**2

# Find the start position
start.px.idx <- which.min(dist.from.origin)
start.point <- white.pixels[start.px.idx,]
# remove min
white.pixels.start.removed <- white.pixels[setdiff(1:nrow(white.pixels), start.px.idx),]

# add points to matrix for each stroke
points <- matrix(data = start.point, ncol = 2, nrow = 1)

add.points <- function(stoke.points, points.matrix, current.direction) {
  start <- points[-1,]
  distances <- apply(points.matrix, 1, function(x) { distance(start, x)})
  others.orderedby.proximity <- points.matrix[order(distances, decreasing = FALSE),]
  
  directions <- apply(others.orderedby.proximity[1:4,], 1, angle, start)
  direction <- rad2deg(my.mode(directions))
  
  # figure out which direction we really want
  if (direction > 0 & direction < 45) {
    direction.new <- 'right'
    # if direction is different, break
  } else if (direction >= 45 & direction <= 135) {
    direction.new <- 'up'
  } else if (direction > 135 & direction < 225) {
    direction.new <- 'left'
  } else if (direction >= 225) {
    direction.new <- 'down'
  }
  
  if (direction.new != direction.current) {
    
  } else {
    # add new point
    # add neigbor in that direction
    
    # recurse
    add.points(stroke.points)
  }
}
# Find the direction of neighbors, adding points to the current stroke
# Build the stroke until most neighbors switch direction
# then start a new spline in that direction
create.skeleton <- function(points, points.matrix) {
  # neighbors will be all the nearest points
  # point should by an x,y coordinate
  # points.matrix should be a matrix of x,y coordinates
  # find 4? nearest neighbors
  distances <- apply(points.matrix, 1, function(x) { distance(start, x)})
  others.orderedby.proximity <- points.matrix[order(distances, decreasing = FALSE),]
  # neighbors touching this guy should have an average direction
  # if nearest neighbors are all in the same direction, we are in the start position for this storke
  # if neighbors are indifferent directions, build spline in one direction randomly from the to
  directions <- apply(others.orderedby.proximity[1:4,], 1, angle, start)
  (direction <- rad2deg(my.mode(directions)))

  # figure out which direction we really want
  if (direction > 0 & direction < 45) {
    start.direction <- 'right'
    # if direction is different, break
  } else if (direction >= 45 & direction <= 135) {
    start.direction <- 'up'
  } else if (direction > 135 & direction < 225) {
    start.direction <- 'left'
  } else if (direction >= 225) {
    start.direction <- 'down'
  }
}


my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
distance <- function(point1, point2) {
  sqrt((point2['x'] - point1['x'])**2 + (point2['y'] - point1['y'])**2)
}

angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

rad2deg <- function(rad) {(rad * 180) / (pi)}

# color the point on the plot with given color
# defaults to cadetblue
# http://kktg.net/sgr/wp-content/uploads/colors1.png
# http://www.stat.columbia.edu/~tzheng/tianblog/uploaded_images/screenshots-755976.bmp
plot.point <- function(point, color = 'cadetblue') {
  points(point['x'], point['y'], col = color, pch = 19)
}
plot.point(point1)
plot.point(point2, color = 'cornflowerblue')
direction(point1,point2)


# Consider all pixels on the boundaries of foreground regions 
# (i.e. foreground points that have at least one background neighbor).
# Iterate through points
# if a point doesn't have 4 n-w-s-e neighbors
# and has more than one neighbor, delete it
# unless

current.index <- 1
current.point <- white.pixels[current.index,]
white.pixels.thinned <- white.pixels
plot(white.pixels, pch=19)
plot.point(current.point, color = 'blue')
visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
thin.points <- function(current.point) {
  if (!is.na(current.point['x'])) {
    plot.point(current.point, color = 'blue')
    Sys.sleep(0.2)
    visited <<- rbind(visited, current.point)
    print(nrow(visited))
    print(nrow(white.pixels.thinned))
    # find 4 nearest neighbors
    #plot.point(current.point, color = 'blue')
    distances <- apply(white.pixels, 1, function(x) { distance(current.point, x)})
    ordered.distances <- distances[order(distances, decreasing = FALSE)]
    four.neighbors.position <- white.pixels[order(distances, decreasing = FALSE),][2:5,]
    apply(four.neighbors.position, 1, plot.point, color = 'green')
    Sys.sleep(0.2)
    four.neighbors.nwse <- ordered.distances[2:5] == 1
    if (sum(four.neighbors.nwse) == 3 || sum(four.neighbors.nwse) == 2) {
      # delete it from thinned
      position <- row.match(current.point,white.pixels.thinned)
      if (!is.na(position)) {
        plot.point(white.pixels.thinned[position,], color = 'red')
        Sys.sleep(0.2)
        # only do this if it's not creating a SCHISM
        white.pixels.thinned <<- white.pixels.thinned[-position,] 
        plot(white.pixels.thinned,pch=19)
        Sys.sleep(0.2)
        nrow(white.pixels.thinned)
        # move to a neighbor
      }
    }
    next.pos <- four.neighbors.position
    #current.point <- next.pos[sample(nrow(next.pos), 1),]
    #thin.points(current.point, white.pixels.thinned)
    if (nrow(next.pos) > 0) {
      for (j in 1:nrow(next.pos)) {
        if (is.na(row.match(next.pos[j,],visited))) {
          thin.points(next.pos[j,])
        }
      }
  
    }
  }
  white.pixels.thinned
}
wp <- thin.points(current.point)
plot(wp, pch = 19)


