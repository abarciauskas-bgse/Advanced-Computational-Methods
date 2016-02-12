if (!require('grid')) install.packages('grid')
library(grid)

setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/data/')
source('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/displayDigit.R')

digits <- read.csv('MNIST_training.csv')
digit <- digits[4,]
label <- as.numeric(digit[1])

# 2. the rest are pixel intensities for 16x16 image of digits
features <- as.numeric(digit[2:ncol(digit)])

# displaying the digit
displayDigit(features, label, newDevice = FALSE)

if (!require('splines')) install.packages('splines')
library(splines)

require(stats); require(graphics)
bs(women$height, df = 5)
summary(fm1 <- lm(weight ~ bs(height, df = 5), data = women))

## example of safe prediction
plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
ht <- seq(57, 73, length.out = 200)
lines(ht, predict(fm1, data.frame(height = ht)))


set.seed(1)
n <- 400
x <- 0:(n-1)/(n-1)
# Crazy polynomial
f <- 0.2*x^11*(10*(1-x))^6 + 10*(10*x)^3*(1-x)^10
# With noise
y <- f + rnorm(n, 0, sd = 2)

## load the splines package - comes with R
require(splines)
mod <- lm(y ~ bs(x, knots = seq(0.1, 0.9, by = 0.1)))
pdat <- data.frame(x = seq(min(x), max(x), length = 100))
## predict for new `x`
pdat <- transform(pdat, yhat = predict(mod, newdata = pdat))

## now plot
ylim <- range(pdat$y, y) ## not needed, but may be if plotting CIs too
plot(y ~ x)
lines(yhat ~ x, data = pdat, lwd = 2, col = "red")

x <- seq(0,1,0.005)
(y <- append(rev(seq(0,1,0.01)), seq(0,1,0.01)))
sp <- smooth.spline(x, y = y[2:length(y)])
plot(sp)
lines(sp)

# There is probably a bettwer way to do this
# But we want to extract all the positions of pixels which are whiteish
# so cycle through every row, and every column, and if the value is greather than 0.5
# add the x,y tuple
pixels <- matrix(features, 16, 16, byrow = TRUE)
rotate <- function(x) t(apply(x, 2, rev))
pixels <- rotate(pixels)

for (x in 1:nrow(pixels)) {
  for (y in 1:ncol(pixels)) {
    if (pixels[x,y] > 0.5) {
      xs <- append(xs, x)
      ys <- append(ys, y)
    }
  }
}
plot(xs,ys)
sp <- smooth.spline(xs[1:20], ys[1:20])
sp$fit
plot(xs,ys)

# Random search to get only the skeleton
# starting from the random location,
# recursively look at neighbors until everything has been visited

check.neighbors <- function(current.node.idx) {
  current.node <- as.numeric(nodes.yet.visited[current.node.idx,])
  current.node.value <- pixels[current.node[1], current.node[2]]
  
  if (!is.na(current.node.value)) {
    # If value of pixel is greater than -1 and greater than all it's neighbors
    # Add it's x and y to the skeleton x and y
    neighbor.locations <- vector(mode = "list", length = 8)
    # upper left corner
    neighbor.locations[[1]] <- c(curr.x-1,curr.y+1)
    neighbor.locations[[2]] <- c(curr.x-1,curr.y)
    neighbor.locations[[3]] <- c(curr.x-1,curr.y-1)
    neighbor.locations[[4]] <- c(curr.x,curr.y-1)
    neighbor.locations[[5]] <- c(curr.x+1,curr.y-1)
    neighbor.locations[[6]] <- c(curr.x+1,curr.y)
    neighbor.locations[[7]] <- c(curr.x+1,curr.y+1)
    neighbor.locations[[8]] <- c(curr.x,curr.y+1)
    
    neighbor.pixels.values <- c()
    for (i in 1:length(neighbor.locations)) {
      neighbor.pixels.values <- append(
        neighbor.pixels.values,
        pixels[neighbor.locations[[i]][1], neighbor.locations[[i]][2]]
      )
    }
    
    if (!(is.na(current.node.value)) &
        current.node.value > -1 &
        (current.node.value > max(neighbor.pixels.values) || current.node.value > 0.5)) {
      print('TRUE')
      print(current.node[1])
      no.items <- no.items + 1
      xs <<- c(xs, c(current.node[1]))
      ys <<- c(ys, c(current.node[2]))
    }
  }
}

nodes.yet.visited <- expand.grid(1:16, 1:16)
xs <- c()
ys <- c()
no.items <- 0

# Stopping condition is when there are no new neighbors to check
while (nrow(nodes.yet.visited) > 3) {
  # start somewhere
  current.node.idx <- as.numeric(sample(rownames(nodes.yet.visited),1))
  # Remove current node from nodes.yet.visited
  nodes.yet.visited <- nodes.yet.visited[-current.node.idx,]
  print(paste('rows left:',nrow(nodes.yet.visited)))
  check.neighbors(current.node.idx)
}

plot(xs,ys)

