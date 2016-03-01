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
unvisited <- white.pixels.thinned
stroke.idx <- 1
pen_up_down(unvisited, singletons)
lapply(strokes, length)

strokes <- lapply(strokes, unique)
list.condition <- sapply(strokes, function(x) length(x) >= 4)
strokes  <- strokes[list.condition]

merged.strokes <- list()
unvisited.strokes <- c(1:length(strokes))
# check every stroke to see if it should be merged into another stroke
while (length(unvisited.strokes) > 0) {
  curr.stroke.idx <- unvisited.strokes[1]
  merged <- FALSE
  curr.stroke <- strokes[[curr.stroke.idx]]
  curr.length <- length(curr.stroke)
  if (length(unvisited.strokes) > 1) {
    unvisited.strokes <- unvisited.strokes[2:length(unvisited.strokes)]
  } else {
    unvisited.strokes <- c()
  }

  # if there anything has already been merged strokes check there first
  # otherwise check if we should merge anything from unvisited.strokes
  if (length(merged.strokes) > 0) {
    # check merged
    for (merged.idx in 1:length(merged.strokes)) {
      if (!merged) {
        length.metric <- (curr.length-length(setdiff(curr.stroke, merged.strokes[[merged.idx]])))/curr.length
        if (length.metric > 0.75) {
          merged.strokes[[merged.idx]] <- intersect(curr.stroke, merged.strokes[[merged.idx]])
          merged <- TRUE
          break
        }
      }
    }
  }
  
  if ((length(unvisited.strokes) > 0) && !merged) {
    for (unchecked.idx in 1:length(unvisited.strokes)) {
      if ((curr.length-length(setdiff(curr.stroke, strokes[[unchecked.idx]])))/curr.length > 0.75) {
        merged.strokes <- append(merged.strokes, list(intersect(curr.stroke, strokes[[unchecked.idx]])))
        unvisited.strokes <- unvisited.strokes[-unchecked.idx]
      }
    }
  }
}

plot(white.pixels.thinned, pch = 19)
colors <- c('red','blue','orange','green','pink')
for (i in 1:length(merged.strokes)) {
  color <- colors[i]
  stroke <- matrix(unlist(merged.strokes[[i]]), nrow = length(merged.strokes[[i]]), ncol = 2, byrow = TRUE) 
  apply(stroke, 1, plot.point, color = color)
}

## Now we will create relations
# For this we just say if the strokes share a bunch of points it's an end-relation (e.g. append)
# if they don't share a bunch of points, it's an along relation
relations <- matrix(NA, nrow = length(merged.strokes), ncol = length(merged.strokes))

for (stroke.idx in 1:length(merged.strokes)) {
  curr.stroke <- merged.strokes[[stroke.idx]]
  curr.length <- length(curr.stroke)
  for (stroke.idx.2 in setdiff(c(1:length(merged.strokes)), stroke.idx)) {
    if (!(stroke.idx == stroke.idx.2)) {
      if ((curr.length-length(setdiff(curr.stroke, merged.strokes[[stroke.idx.2]])))/curr.length > 0.5) {
        relations[stroke.idx.2, stroke.idx] <- 'end'
      }
    }
  }
}

# we should do this more than once, it seems we don't get back really distinct parses from penUpDown
