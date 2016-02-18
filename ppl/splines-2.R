if (!require('grid')) install.packages('grid')
if (!require('prodlim')) install.packages('prodlim')

setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/data/')
source('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/displayDigit.R')

digits <- read.csv('MNIST_training.csv')
digit <- digits[27,]
(label <- as.numeric(digit[1]))

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

dist.from.origin <- function(white.pixels) {
  (white.pixels[,1] - white.pixels[,2])**2
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

current.point <- white.pixels[1,]
white.pixels.thinned <- white.pixels
plot(white.pixels, pch=19)
plot.point(current.point, color = 'blue')
visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
thin.points <- function(current.point, animation = FALSE) {
  if (!is.na(current.point['x'])) {
    plot.point(current.point, color = 'blue')
    if (animation) Sys.sleep(0.2)
    visited <<- rbind(visited, current.point)
    print(nrow(visited))
    print(nrow(white.pixels.thinned))
    # find 8 nearest neighbors
    distances <- apply(white.pixels, 1, function(x) { distance(current.point, x)})
    ordered.distances <- distances[order(distances, decreasing = FALSE)]
    eight.neighbors.position <- white.pixels[order(distances, decreasing = FALSE),][2:9,]
    four.nwse <- eight.neighbors.position[1:4,]
    four.corners <- eight.neighbors.position[5:8,]
    apply(four.corners, 1, plot.point, color = 'yellow')
    apply(four.nwse, 1, plot.point, color = 'orange')
    if (animation) Sys.sleep(0.2)
    (four.neighbors.nwse <- ordered.distances[2:5] == 1)
    (four.neighbors.corners <- ordered.distances[6:9] == sqrt(2))
    # Delete if two foreground neighbors or surrounded
    if (sum(four.neighbors.nwse) == 3 || sum(four.neighbors.nwse) == 2) {
      # delete it from thinned
      position <- row.match(current.point,white.pixels.thinned)
      if (!is.na(position)) {
        plot.point(white.pixels.thinned[position,], color = 'red')
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
          white.pixels.thinned <<- white.pixels.thinned[-position,]
        }

        plot(white.pixels.thinned,pch=19)
        if (animation) Sys.sleep(0.2)
        nrow(white.pixels.thinned)
        # move to a neighbor
      }
    }
    next.pos <- four.nwse
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

# go throuh pixels row and column
# keep if in thinned points
white.pixels.thinned.2 <- white.pixels.thinned
white.pixels.thinned.2[,2] <- white.pixels.thinned[,2] - min(white.pixels.thinned.2[,2])
pixels.thinned <- pixels
for (ridx in 1:nrow(pixels)) {
  for (cidx in 1:ncol(pixels)) {
    if (is.na(row.match(c(ridx,cidx), white.pixels.thinned.2))) {
      pixels.thinned[ridx,cidx] <- -1.0
    }
  }
}

features.new <- as.numeric(rotate(rotate(pixels.thinned)))
displayDigit(features.new, label, newDevice = FALSE)


# create substrokes
(current.point <- white.pixels.thinned[order(dist.from.origin(white.pixels.thinned))[1],])
changes.direction <- 0
current.stroke.idx <- 1
strokes <- list(current.point)
pts.to.be.visited <- white.pixels.thinned
plot(pts.to.be.visited, pch = 19)x.threshold <- current.point[1]
y.threshold <- current.point[2]
current.direction <- NA

while (length(pts.to.be.visited) > 0) {
  # remove from points to be visited
  r <- row.match(current.point, pts.to.be.visited)
  pts.to.be.visited <- pts.to.be.visited[-r,]
  plot.point(current.point, color = 'red')
  Sys.sleep(0.5)
  # find nearest neighbors
  print(pts.to.be.visited)
  distances <- apply(pts.to.be.visited, 1, function(x) { distance(current.point, x)})
  ordered.distances <- distances[order(distances, decreasing = FALSE)]
  two.neighbors <- pts.to.be.visited[order(distances, decreasing = FALSE),][2:3,]
  # if we're just starting the stroke, set current.direction to nearest neighbor
  if (is.na(current.direction)) {
    nn <- two.neighbors[1,]
    plot.point(nn, color = 'yellow')
    Sys.sleep(0.5)
    current.direction <- angle(current.point, nn)
    strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], nn)
    current.point <- nn
  } else {
    # check if next is in the same direction
    nn <- two.neighbors[1,]
    new.direction <- angle(current.point, nn)
    # if nearest point is in the same direction, no change in direction
    if (new.direction == current.direction) {
      strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], nn)
      current.point <- nn
    } else if (ordered.distances[2] <= 2) {
      # or at a 45 degree angle
      changes.direction <- changes.direction + 1
      strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], nn)
      current.point <- nn
    } else {
      current.stroke.idx <- current.stroke.idx + 1
      (current.point <- pts.to.be.visited[order(dist.from.origin(pts.to.be.visited))[1],])
      strokes[[current.stroke.idx]] <- current.point
    }
  }
}

strokes

stroke.1 <- matrix(strokes[[1]], ncol = 2, byrow=TRUE)
plot(stroke.1,pch=19)
