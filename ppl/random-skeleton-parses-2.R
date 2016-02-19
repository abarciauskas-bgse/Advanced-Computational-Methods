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

find.one.neighbor <- function(pixels, current.point) {
  shuffled.positions <- sample(1:8)
  for (position.idx in 1:8) {
    position <- shuffled.positions[position.idx]
    neighbor <- row.match(current.point-c(1,0), pixels)
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

    if (!is.na(pt.idx)) break
  }

  return(pixels[pt.idx,])
}


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
current.dir.probability <- 0.5
corner.dir.probability <- 0.25
new.point <- current.point
max.iter <- 10*length(white.pixels.thinned)
current.iter <- 0

while((length(pts.to.be.visited) > 1) && current.iter < max.iter) {
  current.iter <- current.iter + 1
  # remove current point from points to be visited
  if (!is.null(nrow(pts.to.be.visited))) {
    r <- row.match(current.point, pts.to.be.visited)
    if (!is.na(r)) {
      pts.to.be.visited <- pts.to.be.visited[-r,]
    }
  }

  # select a randomish neighbor in the current direction
  i <- 0
  # try to find a new neighbor to visit or stop after 25 tries
  while (is.na(new.point) || all(new.point == current.point)) {
    if (i > 25) {
      new.point <- find.one.neighbor(white.pixels.thinned, current.point)
      break
    }
    (rdraw <- which(rmultinom(1, prob = direction.probabilities, size = 1) == 1))
    # determine if there is a point in the chosend direction
    (new.point.try <- relative.pt(white.pixels.thinned, current.point, rdraw))
    # if there is a new point in the current-ish direction, go there
    if (!is.na(all(new.point.try))) {
      new.point <- new.point.try
      break
    } else {
      i <- i + 1
    }
  }

  # if new point is NOT still equal to the current
  if (!is.na(all(new.point)) & !(all(new.point == current.point)) & all(abs(new.point - current.point) <= 1)) {
    # This re-weighting is arbitrary, there is probably a better way to do it.
    direction.probabilities <- rep(0, 8)
    direction.probabilities[rdraw] <- current.dir.probability
    if (rdraw == 1) {
      direction.probabilities[2] <- corner.dir.probability
      direction.probabilities[8] <- corner.dir.probability
    } else if (rdraw == 8) {
      direction.probabilities[1] <- corner.dir.probability
      direction.probabilities[7] <- corner.dir.probability
    } else {
      direction.probabilities[rdraw+1] <- corner.dir.probability
      direction.probabilities[rdraw-1] <- corner.dir.probability
    }

    # if new point exists at beginning or end of existing strokes
    # set current point to head of current stroke
    # merge current stroke with that stroke
    # reverse direction - update probabilities
    reverse.direction <- FALSE
    lapply(seq_along(strokes), function(stroke.idx) {
      stroke <- strokes[[stroke.idx]]
      if (all(stroke[[1]] == new.point)) {
        print('appending new point to existing')
        reverse.direction <- TRUE
        # stroke with head at new point exists

        # reverse direction
        # reverse the stroke's items
        strokes[[stroke.idx]] <- rev(strokes[[stroke.idx]])
        # append current stroke to stroke
        strokes[[stroke.idx]] <- append(strokes[[stroke.idx]], strokes[[current.stroke.idx]])
        current.stroke.idx <- stroke.idx
        # append current point to stroke
        strokes[[stroke.idx]] <- append(strokes[[stroke.idx]], list(as.numeric(current.point)))

        # update probabilities for the reverse
        direction.probabilities <- rep(0, 8)
        opposite.positions <- shift(c(1:8), 4)
        opp.pos <- opposite.positions[rdraw]
        direction.probabilities[opp.pos] <- current.dir.probability
        if (opp.pos == 1) {
          direction.probabilities[2] <- corner.dir.probability
          direction.probabilities[8] <- corner.dir.probability
        } else if (opp.pos == 8) {
          direction.probabilities[1] <- corner.dir.probability
          direction.probabilities[7] <- corner.dir.probability
        } else {
          direction.probabilities[opp.pos+1] <- corner.dir.probability
          direction.probabilities[opp.pos-1] <- corner.dir.probability
        }
      }
    })

    # update current point
    if (!reverse.direction) {
      strokes[[current.stroke.idx]] <- append(strokes[[current.stroke.idx]], list(as.numeric(new.point)))
      current.point <- new.point
    }
  } else {
    (direction.probabilities <- rep(c(corner.probability,direct.probability), 4))
    # pick a new point at random
    current.point <- white.pixels.thinned[sample(nrow(white.pixels.thinned),1),]

    current.stroke.idx <- current.stroke.idx + 1
    strokes[[current.stroke.idx]] <- list(as.numeric(current.point))
  }
}

print(length(strokes))
lapply(strokes, function(s) { print(length(s)) })

s <- unique(strokes[[1]])
(current.direction <- rad2deg(angle(s[[1]], s[[2]])))
new.stroke.idx <- 1
new.strokes <- list()
new.strokes[[new.stroke.idx]] <- list(as.numeric(s[[1]]))
for (i in 2:(length(s)-1)) {
  new.strokes[[new.stroke.idx]] <- append(new.strokes[[new.stroke.idx]], list(as.numeric(s[[i]])))
  if (abs(rad2deg(angle(s[[i]], s[[i+1]])) - current.direction) > 90) {
    print('direction change')
    current.direction <- rad2deg(angle(s[[i]], s[[i+1]]))
    new.stroke.idx <- new.stroke.idx + 1
    new.strokes[[new.stroke.idx]] <- list()
  }
}
lapply(new.strokes, function(s) { print(length(s)) })

new.strokes <- new.strokes[which(lapply(new.strokes, function(s) { length(s) > 6 }) == TRUE)]

plot(white.pixels.thinned, pch =19)
lapply(new.strokes, function(stroke) {
  Sys.sleep(1)
  stroke <- matrix(unlist(unique(stroke)), ncol = 2, byrow=TRUE)
  colnames(stroke) <- c('x','y')
  apply(stroke, 1, plot.point, color = sample(rainbow(10),1))
})

sp <- smooth.spline(stroke.ex, nknots=5)
lines(sp)

bs(stroke.ex[,'x'], df = 5)
summary(fm1 <- lm(stroke.ex[,'y'] ~ bs(stroke.ex[,'x'], df = 5)))

## example of safe prediction
plot(stroke.ex, xlab = "", ylab = "", pch = 19)
ht <- seq(min(stroke.ex[,'x']), max(stroke.ex[,'x']), length.out = 200)
summary(fm2 <- lm(stroke.ex[,'y'] ~ stroke.ex[,'x']))
lines(stroke.ex[,'x'], fm1$fitted.values, col = 'blue')
lines(stroke.ex[,'x'], fm2$fitted.values, col = 'red')
