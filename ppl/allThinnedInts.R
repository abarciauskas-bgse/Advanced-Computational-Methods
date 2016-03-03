all.thinned.ints <- function(digits, limit = NA) {
  if (is.na(limit)) limit <- nrow(digits)
  all.thinned.ints <- list()
  
  for (row.idx in 1:limit) {
    # row.idx <- sample(1:nrow(digits),1)
    digit <- digits[row.idx,]
    label <- as.numeric(digit[1])

    features <- as.numeric(digit[2:ncol(digit)])
    pixels <- matrix(features, 16, 16, byrow = TRUE)
    pixels <- rotate(pixels)
    
    # plot the pixels in the foreground
    #all.pixels <- which(pixels > 0, arr.ind = TRUE)
    #plot(all.pixels, pch = 19)
    white.pixels <- which(pixels > (quantile(pixels, probs = c(0.73)) && 0), arr.ind = TRUE)
    
    if (!(length(white.pixels[,2]) == 0)) {
      white.pixels[,2] <- white.pixels[,2] - max(white.pixels[,2],0)
      # subtract greatest y value from all y values
      # so top is now x = 0 and we are in the 4th quadrant of xy
      colnames(white.pixels) <- c('x','y')
      
      iter <- 0
      max.iter <- 2
      while (iter < max.iter) {
        iter <- iter  + 1
        current.pt <- white.pixels[1,]
        white.pixels <- thin.points(current.pt, white.pixels)
      }
      all.thinned.ints[[row.idx]] <- list(label=label, num.pixels=length(white.pixels), points = white.pixels)
    } else {
      all.thinned.ints[[row.idx]] <- list(label=label, num.pixels=NA)
    }
  }
  return(all.thinned.ints)
}
