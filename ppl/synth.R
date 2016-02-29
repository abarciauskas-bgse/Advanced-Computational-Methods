if (!require('grid')) install.packages('grid')
if (!require('prodlim')) install.packages('prodlim')

setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/data/')
source('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/displayDigit.R')
source('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/utils.R')

digits <- read.csv('MNIST_training.csv')
# 1573 is an 8 which looks ok
digit <- digits[sample(1:nrow(digits),1),]
(label <- as.numeric(digit[1]))

features <- as.numeric(digit[2:ncol(digit)])

# displaying the original digit
displayDigit(features, label, newDevice = FALSE)

pixels <- matrix(features, 16, 16, byrow = TRUE)
pixels <- rotate(pixels)

# plot the pixels in the foreground
white.pixels <- which(pixels > 0, arr.ind = TRUE)
plot(white.pixels, pch = 19)

# subtract greatest y value from all y values
# so top is now x = 0 and we are in the 4th quadrant of xy
white.pixels[,2] <- white.pixels[,2] - max(white.pixels[,2])
colnames(white.pixels) <- c('x','y')

source('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/ppl/thinPoints.R')

current.point <- white.pixels[1,]
white.pixels.thinned <- white.pixels
plot(white.pixels, pch=19)
plot.point(current.point, color = 'blue')
visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
wp <- thin.points(current.point)

# Recreate original image using thinned points
# go throuh pixels row and column
# keep if in thinned points
white.pixels.thinned.2 <- white.pixels.thinned
white.pixels.thinned.2[,2] <- white.pixels.thinned[,2] - min(white.pixels.thinned.2[,2])
pixels.thinned <- pixels
for (ridx in 1:nrow(pixels)) {
  for (cidx in 1:ncol(pixels)) {
    if (is.na(row.match(c(ridx,cidx), white.pixels.thinned.2))) {
      pixels.thinned[ridx,cidx] <- -1.0
    }
  }
}

# Not sure why at the moment, but it's flipped
pixels.thinned <- pixels.thinned[,16:1]
features.new <- as.numeric(pixels.thinned)
displayDigit(features.new, label, newDevice = FALSE)
