# % Place you pen down at an unvisited edge,
# % inversely proportional to the number of unvisited
# % paths going from it.
# start at one of the singletons
# current node <- singleton
# find unvisited edges going from it
# add edge/node inversely proportional to 
# the number of unvisted paths going from that edge node
# also if we cross an x or y threshold, we should start a new stroke
pen_up_down <- function(unvisited, animation = FALSE) {
  unvisited.singletons <- singletons
  current.node <- singletons[sample(nrow(singletons),1),]
  y.max.reached <- FALSE
  x.max.reached <- FALSE
  y.max <- max(unvisited[,2])
  x.max <- max(unvisited[,1])

  # if we have reached the min or max x or y,
  # we don't want to cross the y or x threshold
  while (!(is.null(nrow(unvisited))) && (nrow(unvisited) > 0)) {
    curr.x <- current.node[1]
    curr.y <- current.node[2]

    if (curr.y == y.max) y.max.reached = TRUE
    if (curr.x == x.max) x.max.reached = TRUE

    plot.point(current.node, color = 'red')
    if (animation) Sys.sleep(0.1)
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
      if (animation) Sys.sleep(0.1)
      next.to.visit <- neighbors
    } else if (nrow(neighbors) >= 1) {
      apply(neighbors, 1, plot.point, color = 'orange')
      if (animation) Sys.sleep(0.1)
      s <- sample(1:nrow(neighbors),1)
      s <- ifelse(s == 0, 1, s)
      next.to.visit <- neighbors[s,]
    } else {
      # pick up pen, start new stroke
      stroke.idx <- stroke.idx + 1
      if (!(nrow(unvisited.singletons) == 0)) {
        next.singleton.row <- sample(1:nrow(unvisited.singletons),1)
        next.to.visit <- unvisited.singletons[next.singleton.row,]

        unvisited.singletons <- unvisited.singletons[-next.singleton.row,]
      } else {
        next.random.row <- sample(1:nrow(unvisited),1)
        next.to.visit <- unvisited[next.random.row,]
      }
    }
   
    if (x.max.reached && y.max.reached) {
      print('top reached')
      stroke.idx <- stroke.idx + 1
      x.max.reached <- FALSE
      y.max.reached <- FALSE
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
    if (!(is.na(row.match(current.node, unvisited)))) {
      unvisited <- unvisited[-row.match(current.node, unvisited),]
    }
    current.node <- next.to.visit
    print(nrow(unvisited))
  }
}
