if (!require('splines')) install.packages('splines')
setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/')
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
    x.distances <- distances[,i]
    (further.neighbors <- setdiff(which(x.distances <= sqrt(5)), i))
    if (length(further.neighbors) <= 4) {
      neighbors <- setdiff(which(x.distances <= sqrt(2)), i)
      neighbors.positions <- na.omit(sapply(neighbors, function(n) {
        relative.position(pixels[i,], pixels[n,])
      }))
      if ((max(neighbors.positions) - min(neighbors.positions)) < 2) {
        # also check it doesn't have neighbors in both directions
        singletons <- append(singletons, pixels[i,])
      }
    }
  }
  if (length(singletons) == 0) {
    dist.from.ori <- dist.from.origin(white.pixels.thinned)
    nearest <- white.pixels.thinned[which.min(dist.from.ori),]
    singletons <- nearest
  }
  return(matrix(singletons, ncol = 2, nrow = length(singletons)/2, byrow = TRUE))
}

(singletons <- find.singletons(white.pixels.thinned))
plot(white.pixels.thinned, pch = 19)
apply(singletons, 1, plot.point)

source('penUpDown.R')
strokes <- list()
stroke.idx <- 1
unvisited <- white.pixels.thinned

for (i in 1:3) {
  print(paste('iter', i))
  strokes.just.parsed <- pen_up_down(unvisited, singleton.extremums, list())
  strokes <- append(strokes, strokes.just.parsed)
}

length(strokes)
strokes <- lapply(strokes, unique)
lapply(strokes, length)

for (i in 1:length(strokes)) {
  plot(white.pixels.thinned, pch = 19)
  Sys.sleep(0.3)
  stroke <- matrix(unlist(strokes[[i]]), nrow = length(strokes[[i]]), ncol = 2, byrow = TRUE) 
  apply(stroke, 1, plot.point, color = 'orange')
  Sys.sleep(0.3)
}

# Remove strokes with less than 25% or greater than 67% of points
(total.points <- nrow(white.pixels.thinned))
(quarter.percent.thresh <- round(total.points/4, 0))
(half.percent.thresh <- round(total.points/2, 0))

list.condition <- sapply(strokes, function(x) (length(x) > quarter.percent.thresh) && (length(x) < half.percent.thresh))
substrokes <- strokes[list.condition]
length(substrokes)
lapply(substrokes, length)

for (i in 1:length(substrokes)) {
  plot(white.pixels.thinned, pch = 19)
  Sys.sleep(0.3)
  stroke <- matrix(unlist(substrokes[[i]]), nrow = length(substrokes[[i]]), ncol = 2, byrow = TRUE) 
  apply(stroke, 1, plot.point, color = 'orange')
  Sys.sleep(0.3)
}

substrokes <- strokes
merged.strokes <- list()
unvisited.strokes <- c(1:length(substrokes))
# check every stroke to see if it should be merged into another stroke
while (length(unvisited.strokes) > 0) {
  curr.stroke.idx <- unvisited.strokes[1]
  merged <- FALSE
  curr.stroke <- substrokes[[curr.stroke.idx]]
  curr.length <- length(curr.stroke)
  if (length(unvisited.strokes) > 1) {
    unvisited.strokes <- unvisited.strokes[2:length(unvisited.strokes)]
  } else {
    unvisited.strokes <- c()
  }
  
  # if there anything has already been merged substrokes check there first
  # otherwise check if we should merge anything from unvisited.strokes
  if (length(merged.strokes) > 0) {
    # check merged
    for (merged.idx in 1:length(merged.strokes)) {
      if (!merged) {
        length.metric <- (curr.length-length(setdiff(curr.stroke, merged.strokes[[merged.idx]])))/curr.length
        if (length.metric > 0.75) {
          merged.strokes[[merged.idx]] <- union(curr.stroke, merged.strokes[[merged.idx]])
          merged <- TRUE
          break
        }
      }
    }
  }
  
  if ((length(unvisited.strokes) > 0) && !merged) {
    for (unchecked.idx in 1:length(unvisited.strokes)) {
      if ((curr.length-length(setdiff(curr.stroke, substrokes[[unchecked.idx]])))/curr.length > 0.75) {
        merged.strokes <- append(merged.strokes, list(union(curr.stroke, substrokes[[unchecked.idx]])))
        unvisited.strokes <- unvisited.strokes[-unchecked.idx]
      }
    }
  }
}

length(merged.strokes)


for (i in 1:length(merged.strokes)) {
  plot(white.pixels.thinned, pch = 19)
  Sys.sleep(0.33)
  stroke <- matrix(unlist(merged.strokes[[i]]), nrow = length(merged.strokes[[i]]), ncol = 2, byrow = TRUE) 
  apply(stroke, 1, plot.point, color = 'orange')
  Sys.sleep(0.33)
}





stroke.lengths <- lapply(merged.strokes, length)
merged.and.ordered <- merged.strokes[order(unlist(stroke.lengths), decreasing = TRUE)]

first.two <- merged.and.ordered[1:2]
plot(white.pixels.thinned, pch = 19)

colors <- c('red','blue','orange','green','pink')
for (i in 1:length(first.two)) {
  color <- colors[i]
  stroke <- matrix(unlist(first.two[[i]]), nrow = length(first.two[[i]]), ncol = 2, byrow = TRUE) 
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
