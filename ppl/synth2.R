if (!require('grid')) install.packages('grid')
if (!require('prodlim')) install.packages('prodlim')
if (!require('igraph')) install.packages('igraph')

setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/data/')
source('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/displayDigit.R')
source('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/utils.R')
source('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/thinPoints.R')

digits <- read.csv('MNIST_training.csv')
all.thinned.ints <- list()

for (row.idx in 1:nrow(digits)) {
  digit <- digits[row.idx,]
  label <- as.numeric(digit[1])
  
  features <- as.numeric(digit[2:ncol(digit)])
  pixels <- matrix(features, 16, 16, byrow = TRUE)
  pixels <- rotate(pixels)
  
  # plot the pixels in the foreground
  white.pixels <- which(pixels > quantile(pixels, probs = c(0.667)), arr.ind = TRUE)

  if (!(length(white.pixels[,2]) == 0)) {
    white.pixels[,2] <- white.pixels[,2] - max(white.pixels[,2],0)
    # subtract greatest y value from all y values
    # so top is now x = 0 and we are in the 4th quadrant of xy
    colnames(white.pixels) <- c('x','y')
    
    current.pt <- white.pixels[1,]
    white.pixels.thinned <- white.pixels
    visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
    points <- thin.points(current.pt)
    
    all.thinned.ints[[row.idx]] <- list(label=label, num.pixels=length(points), points = points)
  }
}

# started at 9:37
#
int.lengths <- lapply(all.thinned.ints, function(num) {
  c(num$label, num$num.pixels)
})

int.lengths.mat <- matrix(unlist(int.lengths), nrow = length(int.lengths), ncol = 2, byrow = TRUE)
plot(int.lengths.mat[,1], int.lengths.mat[,2], pch = 19)

for (i in 1:10) {
  plot(thinned.ints[[i]]$points, pch = 19, ylim = c(-16,0), xlim = c(0,16))
  Sys.sleep(1)
}
#setwd('../ppl/')
source('relativePosition.R')
nearest.origin <- function(points) {
  (dists <- apply(points, 1, dist.from.origin))
  return(which.min(dists))
}

nearest.point <- function(point, points, order=1) {
  current.row <- row.match(point, points)
  x.distances <- distances(points)[current.row,]
  ordered.distances <- x.distances[order(x.distances)]
  nearest.idx <- setdiff(order(x.distances), current.row)[order]
  return(points[nearest.idx,])
}

neighbors.directions <- function(point, points) {
  current.row <- row.match(point, points)
  x.distances <- distances(points)[current.row,]
  neighbors <- setdiff(which(x.distances <= sqrt(2)), current.row)
  neighbors.points <- points[neighbors,]
  relative.positions <- NA
  if (length(neighbors) > 1) {
    relative.positions <- apply(neighbors.points, 1, function(n) {
      relative.position(point,n)
    })
  } else {
    relative.positions <- relative.position(point, neighbors.points)
  }
  directions <- relative.positions
  return(list(directions = directions, neighbors = neighbors.points))
}

for (idx in 1:length(all.thinned.ints)) {
  new.int <- all.thinned.ints[[idx]]
  points <- new.int$points
  label <- new.int$label
  start <- points[nearest.origin(points),]
  unvisited <- points
  current.pt <- start
  current.directions <- neighbors.directions(current.pt, unvisited)
  current.direction <- median(current.directions$directions)
  loops <- 0
  path <- matrix(current.pt, nrow = 1, ncol = 2, byrow = TRUE)
  if (animation) plot(points, pch = 19)
  while (!is.null(nrow(unvisited))) {
    (pt.in.graph <- relative.pt(path, current.pt, current.direction))
    new.pt <- NA
    if (all(is.na(pt.in.graph))) {
      new.pt <- relative.pt(unvisited, current.pt, current.direction)
    } else {
      loops <- loops + 1
      (new.pt <- nearest.point(current.pt, unvisited))
      current.directions <- neighbors.directions(new.pt, unvisited)
      (current.direction <- median(current.directions$directions))
      #break
    }
    
    # if still nothing, find new direction
    if (all(is.na(new.pt))) {
      current.directions <- neighbors.directions(current.pt, unvisited)
      current.direction <- current.directions$directions[which.min(abs(current.directions$directions - current.direction))]
      new.pt <- relative.pt(unvisited, current.pt, current.direction)
    } else {
      if (!is.null(nrow(current.directions$neighbors))) {
        apply(current.directions$neighbors, 1, function(r) {
          r <- row.match(r, unvisited)
          if (!is.na(r)) unvisited <<- unvisited[-r,]
        })
      } else {
        r <- row.match(current.directions$neighbors, unvisited)
        if (!is.na(r)) unvisited <<- unvisited[-r,]
      }
    }
    if (all(is.na(new.pt))) {
      order <- 2
      while (all(is.na(new.pt))) {
        (new.pt <- nearest.point(current.pt, unvisited, order))
        order <- order + 1
        if (order > 3) break
      }
    } 
  
    r <- row.match(current.pt, unvisited)
    if (!is.na(r)) unvisited <<- unvisited[-r,]
  
    # update
    if (all(is.na(new.pt))) {
      break
    } else {
      path <- rbind(path, new.pt)
      if (animation) {
        plot.point(new.pt, color = 'orange')
        Sys.sleep(0.25)
      }
      current.pt <- new.pt
    }
  }

  all.thinned.ints[[idx]]['loops'] <- loops
  all.thinned.ints[[idx]]['path'] <- list(path)
}

int.loops <- lapply(all.thinned.ints, function(num) {
  c(num$label, num$loops)
})

int.loops.mat <- matrix(unlist(int.loops), nrow = length(int.loops), ncol = 2, byrow = TRUE)
plot(int.loops.mat[,1], int.loops.mat[,2], pch = 19)

for (idx in 1:length(all.thinned.ints)) {
  d <- all.thinned.ints[[idx]][['path']]
  # in a 16x16 image, a point is positioned at the absolute value of y - 1 + x
  d.points <- apply(d, 1, function(pt) { (16*abs(pt[2])-1)+pt[1] })
  mat <- matrix(0, nrow = 16, ncol = 16)
  for (i in 1:256) {
    if (i %in% d.points) {
      row.idx <- i %/% 16 + 1
      col.idx <- i %% 16
      mat[row.idx,col.idx] <- 1
    }
  }
  d <- graph.adjacency(mat)
  g <- minimum.spanning.tree(d)

  features <- as.numeric(t(mat))
  # displaying the original digit
  displayDigit(features, all.thinned.ints[[idx]]$label, newDevice = FALSE)
  Sys.sleep(1)
}