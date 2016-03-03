generatePaths <- function(thinned.ints, animation = FALSE, sleep.time = 0.5) {
  for (idx in 1:length(thinned.ints)) {
    new.int <- thinned.ints[[idx]]
    points <- new.int$points
    label <- new.int$label
    relativity = 'nearest'
    start <- points[nearorfar.from.origin(points, relativity),]
    unvisited <<- points
    current.pt <- start
    current.directions <- neighbors.directions(current.pt, unvisited)
    current.direction <- sample(current.directions$directions,1)
    loops <- 0
    strokes <- 1
    path <- matrix(current.pt, nrow = 1, ncol = 2, byrow = TRUE)
    if (animation) {
      plot(points, pch = 19, ylim = c(-16,0), xlim = c(0,16))
      Sys.sleep(sleep.time)
    }
    
    # if we have reached 3 of the exteme locations, stop
    max.x <- max(unvisited[,1])
    min.x <- min(unvisited[,1])
    max.y <- max(unvisited[,2])
    min.y <- min(unvisited[,2])
    max.x.visited <- FALSE
    min.x.visited <- FALSE
    max.y.visited <- FALSE
    min.y.visited <- FALSE
    while (!is.null(nrow(unvisited) > 2)) {
      if (animation) {
        plot.point(current.pt, color = 'orange')
        Sys.sleep(sleep.time)
      }
      (pt.in.graph <- relative.pt(path, current.pt, current.direction))
      new.pt <- NA

      if (all(is.na(pt.in.graph))) {
        new.pt <- relative.pt(unvisited, current.pt, current.direction)
      } else {
        if (loops > 2) break
        loops <- loops + 1
        # check if we should be doing this at all
        new.pt <- nearest.point(current.pt, unvisited)
        current.directions <- neighbors.directions(new.pt, unvisited)
        current.direction <- sample(current.directions$directions, 1)
      }
      
      # if still nothing, find new direction
      if (all(is.na(new.pt))) {
        current.directions <- neighbors.directions(current.pt, unvisited)
        current.direction <- current.directions$directions[which.min(abs(current.directions$directions - current.direction))]
        new.pt <- relative.pt(unvisited, current.pt, current.direction)
      } else {
        if (!is.null(nrow(current.directions$neighbors))) {
          apply(current.directions$neighbors, 1, function(r) {
            if (r[1] == max.x) max.x.visited <- TRUE
            if (r[1] == min.x) min.x.visited <- TRUE
            if (r[2] == max.y) max.y.visited <- TRUE
            if (r[2] == min.y) min.y.visited <- TRUE
            r <- row.match(r, unvisited)
            if (!is.na(r)) unvisited <<- unvisited[-r,]
          })
        } else {
          r <- row.match(current.directions$neighbors, unvisited)
          if (!is.na(r)) unvisited <<- unvisited[-r,]
        }
      }
      if (all(is.na(new.pt))) {
        while (all(is.na(new.pt))) {
          if (strokes > 3) break
          strokes <- strokes + 1
          # toggle
          #relativity <- ifelse(relativity == 'nearest', 'furthest', 'nearest')
          # FIXME: we do this in 2-3 places to start / restart
          new.pt <- unvisited[nearorfar.from.origin(unvisited, relativity),]
          current.directions <- neighbors.directions(new.pt, unvisited)
          current.direction <- sample(current.directions$directions, 1)
        }
      } 
      
      if (!all(is.na(unvisited))) {
        r <- row.match(current.pt, unvisited)
        if (!is.na(r)) unvisited <<- unvisited[-r,]
      }
      
      # update
      if (all(is.na(new.pt))) {
        break
      } else {
        path <- rbind(path, new.pt)
        if (new.pt[1] == max.x) max.x.visited <- TRUE
        if (new.pt[1] == min.x) min.x.visited <- TRUE
        if (new.pt[2] == max.y) max.y.visited <- TRUE
        if (new.pt[2] == min.y) min.y.visited <- TRUE
        current.pt <- new.pt
      }
    }
    
    thinned.ints[[idx]]['loops'] <- loops
    thinned.ints[[idx]]['strokes'] <- strokes
    thinned.ints[[idx]]['path'] <- list(path)
  }
  return(thinned.ints)
}
