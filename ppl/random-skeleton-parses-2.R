
(current.point <- white.pixels.thinned[sample(nrow(white.pixels.thinned),1),])
strokes <- list()
current.stroke.idx <- 1
strokes[[current.stroke.idx]] <- list(as.numeric(current.point))
# prior probability of each direction
corner.probability <- 0.05
direct.probability <- 0.2
(direction.probabilities <- rep(c(corner.probability,direct.probability), 4))
pts.to.be.visited <- white.pixels.thinned
# weight current direction with higher probability
current.dir.probability <- 0.75
new.point <- current.point
while(length(pts.to.be.visited) > 0) {
  r <- row.match(current.point, pts.to.be.visited)
  if (!is.na(r)) pts.to.be.visited <- pts.to.be.visited[-r,]
  i = 0
  while (!all(new.point, current.point) || !(i > 25)) {
    (rdraw <- which(rmultinom(1, prob = direction.probabilities, size = 1) == 1))
    # determine if there is a point
    (new.point.try <- relative.pt(white.pixels.thinned, current.point, rdraw))
    if (!is.na(all(new.point.try))) {
      new.point <- new.point.try
    } else {
      i <- i + 1
    }
  }

  if (!is.na(all(new.point)) & !(all(new.point == current.point))) {
    # This re-weighting is arbitrary, there is probably a better way to do it.
    direction.probabilities <- rep((1-current.dir.probability)/7, 8)
    direction.probabilities[rdraw] <- current.dir.probability
    # should do the stroke existence check here to merge!!!
    strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], list(as.numeric(new.point)))
    current.point <- new.point
  } else {
    print("herer")
    # pick a new point at random
    (current.point <- pts.to.be.visited[sample(nrow(pts.to.be.visited),1),])
    res <- lapply(strokes, function(stroke) { lapply(stroke, function(x) { all(x==as.numeric(current.point)) }) })
    stroke.idx <- which(unlist(lapply(res, function(x) { any(unlist(x)) })))
    # if not in strokes start new stroke
    if (length(stroke.idx) == 0) {
      current.stroke.idx <- current.stroke.idx + 1
      strokes[[current.stroke.idx]] <- list(as.numeric(current.point))
    } else { # update stroke index to stroke index of current point
      current.stroke.idx <- stroke.idx
      strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], list(as.numeric(current.point)))
    }
  }
}

plot(white.pixels.thinned, pch =19)
lapply(strokes, function(stroke) {
  Sys.sleep(1)
  stroke <- matrix(unlist(unique(stroke)), ncol = 2, byrow=TRUE)
  colnames(stroke) <- c('x','y')
  apply(stroke, 1, plot.point, color = sample(colours(), 1))
})

# Return the point at relative position [1,8] to the current point
relative.pt <- function(pixels, current.point, position) {
  pt.idx <- NA
  if (position == 1) {
    pt.idx <- row.match(current.point-c(1,-1), pixels)
  } else if (position == 2) {
    pt.idx <- row.match(current.point+c(0,1), pixels)
  } else if (position == 3) {
    pt.idx <- row.match(current.point+c(1,1), pixels)
  } else if (position == 4) {
    pt.idx <- row.match(current.point+c(1,0), pixels)
  } else if (position == 5) {
    pt.idx <- row.match(current.point+c(1,-1), pixels)
  } else if (position == 6) {
    pt.idx <- row.match(current.point-c(0,1), pixels)
  } else if (position == 7) {
    pt.idx <- row.match(current.point-c(0,1), pixels)
  } else if (position == 8) {
    pt.idx <- row.match(current.point-c(1,0), pixels)
  }

  return(pixels[pt.idx,])
}

