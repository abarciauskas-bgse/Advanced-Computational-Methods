if (!require('splines')) install.packages('splines')
setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/')
source('penUpDown.R')
# Direction taken from:
# https://github.com/brendenlake/BPL/blob/7413a9724949a9f1af0e53df4a90045436cd60ab/bottomup/initialize/RandomWalker.m
# % Steps for generating a random walk
# %   Step 1) Add singleton nodes in the graph
# % Repeat until complete
# %   Step 2) For each point on a new edge, place pen
# %      down on that point with weight inversely related to the 
# %      degree of new edges coming from it.
# %   Step 3) Choose a new node based on the curvature of the local spline fit.

# search points for singleton nodes (has only one edge)
# having only one edge means having only one neighbor
find.singletons <- function(pixels) {
  distances <- distances(white.pixels.thinned)
  singletons <- c()
  for (i in 1:nrow(pixels)) {
    # direct neighbors
    x.distances <- distances[setdiff(1:nrow(distances), i),i]
    if (sum(x.distances == 1)  <= 1 && sum(x.distances == sqrt(2)) <= 1) {
      singletons <- append(singletons, pixels[i,])
    }
  }
  return(matrix(singletons, ncol = 2, nrow = length(singletons)/2, byrow = TRUE))
}

(singletons <- find.singletons(white.pixels.thinned))
plot(white.pixels.thinned, pch = 19)
apply(singletons, 1, plot.point)

source('penUpDown.R')
strokes <- list()
pen_up_down(unvisited, singletons)
lapply(strokes, length)

# for creating splines
iter <- 1
max.iter <- 10
while (iter <= max.iter) {
  iter = iter + 1

  plot(white.pixels.thinned, pch = 19)
  unvisited <- white.pixels.thinned
  stroke.idx <- 1
  lapply(strokes, length)

  plot(white.pixels.thinned, pch = 19)
  for (i in 1:length(strokes)) {
    if (length(strokes[[i]]) >= 8) {
      stroke <- matrix(unlist(unique(strokes[[i]])), nrow = length(unique(strokes[[i]])), ncol = 2, byrow = TRUE)
  
      apply(stroke, 1, plot.point)
      x <- stroke[,1]
      y <- stroke[,2]
      summary(fm1 <- lm(y ~ bs(x, degree = 3, knots = 5)))
      
      # plot(x, y, xlab = "xs", ylab = "ys",
      #      xlim = range(white.pixels.thinned[,1]),
      #      ylim = range(white.pixels.thinned[,2]),
      #      pch = 19)
      ht <- seq(min(x), max(x), length.out = 100)
      lines(ht, predict(fm1, data.frame(x = ht)), col = 'red')
    }
  }
}
