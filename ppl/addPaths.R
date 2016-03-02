generatePaths <- function(thinned.ints, animation = FALSE) {
  for (idx in 1:length(thinned.ints)) {
    new.int <- thinned.ints[[idx]]
    points <- new.int$points
    label <- new.int$label
    start <- points[nearest.origin(points),]
    unvisited <<- points
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
        if (loops > 3) break
        # check if we should be doing this at all
        (new.pt <- nearest.point(current.pt, unvisited))
        current.directions <- neighbors.directions(new.pt, unvisited)
        (current.direction <- median(current.directions$directions))
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
      
      if (!all(is.na(unvisited))) {
        r <- row.match(current.pt, unvisited)
        if (!is.na(r)) unvisited <<- unvisited[-r,]
      }
      
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
    
    thinned.ints[[idx]]['loops'] <- loops
    thinned.ints[[idx]]['path'] <- list(path)
  }
  return(thinned.ints)
}
