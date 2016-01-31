setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS4/')

genSun <- function(n = 200, 
                   features = 2, 
                   seed = NA, mus = NULL, sigma = NULL,
                   saveData = TRUE, 
                   savePlot = TRUE) {
  
  ####################################################################
  # Inspired by function "mlbench.circle" from package "mlbench"
  # URL: https://cran.r-project.org/web/packages/mlbench/mlbench.pdf
  # (c) Miquel Torrens, 2016.01.11
  # n : number of observation for the dataset
  # features : number of features generated
  # seed : preferred seed
  # mus : means of the features
  # sigma : variance-covariance matrix of the features specified
  # saveData : TRUE if we want to save the dataset (in current working directory)
  # savePlot : TRUE if we want to save the plot (in current working directory)
  ####################################################################
  
  # Libraries
  if (! require(mvtnorm)) { stop('required package not installed: mvtnorm') }
  if (! require(ggplot2)) { stop('required package not installed: ggplot2') }
  
  # For simplicity we restrict to 2 or 3 dimensions (this can be relaxed)
  if (! as.numeric(features) %in% 2:3) {
    stop('argument "features" must be 2 or 3.')
  }
  
  # Default values
  if (! is.na(seed)) { set.seed(seed) }
  if (is.null(sigma)) { sigma <- diag(features) }
  if (is.null(mus)) { mus <- rep(0, features) }
  
  # Simulate points from a bivariate normal
  phi <- rmvnorm(n, mean = mus, sigma = sigma)
  
  # Decide which belong to each cluster
  rad <- (2 ** (features - 1) * gamma(1 + features / 2) /
            (pi ** (features / 2))) ** (1 / features)
  ones <- apply(phi, 1, function(x) { jitter(sum((x - mus) ** 2)) }) > rad ** 2
  #ones <- apply(phi, 1, function(x) { sum((x - mus) ** 2) }) > rad ** 2    
  category <- rep(0, length = n)
  category[ones] <- 1
  
  # Build the final data frame
  new.phi <- cbind.data.frame(phi, as.character(category))
  new.phi[, 3] <- as.factor(new.phi[, 3])
  colnames(new.phi) <- c("x1", "x2", 'y')
  
  # Save the data in a .csv file
  if (saveData) {
    write.csv(new.phi, file = 'dataset.csv', row.names = FALSE)
    cat('Saved file:', paste0(getwd(), '/dataset.csv'), '\n')
  }
  
  # Plot
  if (savePlot) {
    unlink('dataPlot.pdf')
    cairo_pdf('dataPlot.pdf')
    plot1 <- 
      ggplot(data = new.phi, aes(x = x1, y = x2,
                                 colour = y, fill = y)) +
      geom_point() +
      xlab('x1') +
      ylab('x2') +
      theme_bw()
    print(plot1)
    dev.off()
    cat('Saved plot:', paste0(getwd(), '/dataPlot.pdf'), '\n')
  }
  
  # End
  return(new.phi)
}

data <- genSun()
source('kNN.R')
trainResults <- kNN(X=data[,1:2], y=data[,3], k=2, p=2)
head(trainResults$predLabels)
head(data[,3])
head(trainResults$prob)

# accuracy
trainResults$errorCount
trainResults$accuracy

