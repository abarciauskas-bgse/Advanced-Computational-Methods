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

# COOL 

# % Place you pen down at an unvisited edge,
# % inversely proportional to the number of unvisited
# % paths going from it.
# start at one of the singletons
# current node <- singleton
# find unvisited edges going from it
# add edge/node inversely proportional to 
# the number of unvisted paths going from that edge node

pen_up_down <- function() {
  current.node <- singletons[sample(nrow(singletons),1),]
  
  while (!is.na(unvisited[1]) && nrow(unvisited) > 0) {
    plot.point(current.node, color = 'red')
    Sys.sleep(0.2)
    curr.distances <- distances(unvisited)
    current.node.idx <- row.match(current.node, unvisited)
    # find unvisited nodes with an edge to the current node  
    x.distances <- curr.distances[,current.node.idx]
    # TODO: this neighboring should be weighted by edges and current 'trajectory'
    neighbors.idx <- intersect(which(x.distances > 0), which(x.distances <= sqrt(2)))
    neighbors <- unvisited[as.vector(neighbors.idx),]

    next.to.visit <- NA
    if (is.null(nrow(neighbors))) {
      plot.point(neighbors, color = 'orange')
      Sys.sleep(0.2)
      next.to.visit <- neighbors
    } else if (nrow(neighbors) >= 1) {
      apply(neighbors, 1, plot.point, color = 'orange')
      Sys.sleep(0.2)
      s <- sample(1:nrow(neighbors),1)
      s <- ifelse(s == 0, 1, s)
      next.to.visit <- neighbors[s,]
    } else {
      # pick up pen, start new stroke
      stroke.idx <- stroke.idx + 1
      next.to.visit <- singletons[sample(nrow(singletons),1),]
    }

    # add current node to current stroke
    if (length(strokes[stroke.idx][[1]]) < 1) {
      strokes[[stroke.idx]] <<- list(current.node)
    } else {
      strokes[[stroke.idx]] <<- append(strokes[[stroke.idx]], list(current.node))
    }

    # unless already exists in another stroke
    # in which case merge strokes
    # remove current node from unvisted
    unvisited <- unvisited[-row.match(current.node, unvisited),]
    current.node <- next.to.visit
  }
}

plot(white.pixels.thinned, pch = 19)
unvisited <- white.pixels.thinned
strokes <- list()
stroke.idx <- 1
pen_up_down()

stroke <- matrix(unlist(strokes[[1]]), nrow = length(strokes[[1]]), ncol = 2, byrow = TRUE)
plot(white.pixels.thinned, pch = 19)
apply(stroke, 1, plot.point)

if (!require('splines')) install.packages('splines')
x <- stroke[,1]
y <- stroke[,2]
summary(fm1 <- lm(y ~ bs(x, degree =3, knots = 5)))

plot(x, y, xlab = "xs", ylab = "ys",
     xlim = range(white.pixels.thinned[,1]),
     ylim = range(white.pixels.thinned[,2]),
     pch = 19)
ht <- seq(min(x), max(x), length.out = 100)
lines(ht, predict(fm1, data.frame(x = ht)))

