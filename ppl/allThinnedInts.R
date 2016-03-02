all.thinned.ints <- function(digits, limit = NA) {
  if (is.na(limit)) limit <- nrow(digits)
  all.thinned.ints <- list()
  
  for (row.idx in 1:limit) {
    digit <- digits[row.idx,]
    label <- as.numeric(digit[1])

    features <- as.numeric(digit[2:ncol(digit)])
    pixels <- matrix(features, 16, 16, byrow = TRUE)
    pixels <- rotate(pixels)
    
    # plot the pixels in the foreground
    white.pixels <- which(pixels > quantile(pixels, probs = c(0.667)), arr.ind = TRUE)
    
    if (!(length(white.pixels[,2]) == 0)) {
      white.pixels[,2] <- white.pixels[,2] - max(white.pixels[,2],0)
      # subtract greatest y value from all y values
      # so top is now x = 0 and we are in the 4th quadrant of xy
      colnames(white.pixels) <- c('x','y')
      
      current.pt <- white.pixels[1,]
      white.pixels.thinned <- white.pixels
      visited <- matrix(data = white.pixels[1,], nrow=1,ncol=2)
      points <- thin.points(current.pt, white.pixels)

      all.thinned.ints[[row.idx]] <- list(label=label, num.pixels=length(points), points = points)
    }
  }
  return(all.thinned.ints)
}
