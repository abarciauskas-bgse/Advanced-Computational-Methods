if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)
if (!require('directlabels')) install.packages("directlabels", repos = "http://r-forge.r-project.org")
require('directlabels')

sigmaXY <- function(rho, sdX, sdY) {
  covTerm <- rho * sdX * sdY
  VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2),
                     2, 2, byrow = TRUE)
  return(VCmatrix)
}

genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
  if(!is.na(seed)) set.seed(seed)
  rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
  return(rdraws)
}

loanData <- function(noApproved, noUndecided, noDenied,
                     muApproved, muUndecided, muDenied,
                     sdApproved, sdUndecided, sdDenied,
                     rhoApproved, rhoUndecided, rhoDenied,
                     seed=1111) {

  sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
  sigmaUndecided <- sigmaXY(rho=rhoUndecided, sdX=sdUndecided[1], sdY=sdUndecided[2])
  sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])

  approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
  undecided <- genBVN(noUndecided, muUndecided, sigmaUndecided, seed = seed+1)
  denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+2)

  loanDf <- as.data.frame(rbind(approved, undecided, denied))
  target.label <- c(rep("Approved", noApproved), rep("Undecided", noUndecided), rep("Denied", noDenied))
  target.class <- c(rep(0, noApproved), rep(1, noUndecided), rep(2, noDenied))
  loanDf <- data.frame(loanDf, target.class, target.label)
  colnames(loanDf) <- c("PIratio", "solvency", "target.class", "target.label")
  
  return(loanDf)
}

# `predictLoanApprovals`
# - generates a loan data frame as training data,
# - runs 1 of K discriminant model,
# - generates predictions
# - writes dataset with predictions to csv file
# - generates plot with decision boundaries
#
predictLoanApprovals <- function(noApproved, noUndecided, noDenied,
                    muApproved, muUndecided, muDenied,
                    sdApproved, sdUndecided, sdDenied,
                    rhoApproved, rhoUndecided, rhoDenied,
                    seed=1111, write.csv = TRUE, save.pdf = TRUE) {
 
  loanDf <- loanData(noApproved, noUndecided, noDenied,
                     muApproved, muUndecided, muDenied,
                     sdApproved, sdUndecided, sdDenied,
                     rhoApproved, rhoUndecided, rhoDenied)
  
  Y <- cbind(approved.target = c(rep(1, noApproved), rep(0, noDenied+noUndecided)),
             undecided.target = c(rep(0, noApproved), rep(1, noUndecided), rep(0, noDenied)),
             denied.target = c(rep(0, noApproved+noUndecided), rep(1, noDenied)))
  X <- as.matrix(cbind(ind=rep(1, nrow(loanDf)), loanDf[,c("PIratio", "solvency")]))
  weightsOptim <- solve(t(X)%*%X) %*% t(X) %*% Y
  predictions <- X %*% weightsOptim
  
  # Main dataframe output
  loanDf.withPredictions <- cbind(loanDf, decision = apply(predictions, 1, which.max))
  labels <- list('1' = 'Approved', '2' = 'Undecided', '3' = 'Denied')
  decisions <- as.character(labels[as.character(loanDf.withPredictions[,'decision'])])
  loanDf.withPredictions <- cbind(loanDf.withPredictions,
                                  decision.label = decisions)
  
  # write data frame to file
  if (write.csv == TRUE) write.table(loanDf.withPredictions, file = "predictions.csv", row.names = FALSE)
  
  # plots
  p <- ggplot(data = loanDf, aes(x = solvency, y = PIratio, col = target.label)) + geom_point()
  # approved line
  approved.weights <- weightsOptim[,'approved.target']
  undecided.weights <- weightsOptim[,'undecided.target']
  denied.weights <- weightsOptim[,'denied.target']
  
  denominator <- -(approved.weights['PIratio'] - undecided.weights['PIratio'])
  solv.diff <- (approved.weights['solvency'] - undecided.weights['solvency'])
  w01 <- approved.weights['ind']
  w02 <- undecided.weights['ind']
  intercept <- (w01 - w02)/denominator
  slope <- solv.diff/denominator
  
  b1 <- geom_abline(size = 1.25,intercept = intercept, slope = slope, aes(color = 'Approved'))
  b4 <- geom_abline(size = 1.25,intercept = intercept+0.12, slope = slope, aes(color = 'Undecided'))
  
  denominator <- -(approved.weights['PIratio'] - denied.weights['PIratio'])
  solv.diff <- (approved.weights['solvency'] - denied.weights['solvency'])
  w01 <- approved.weights['ind']
  w02 <- denied.weights['ind']
  intercept <- (w01 - w02)/denominator
  slope <- solv.diff/denominator
  
  b2 <- geom_abline(size = 1.25,intercept = intercept, slope = slope, aes(color = 'Approved'))
  b5 <- geom_abline(size = 1.25,intercept = intercept+0.18, slope = slope, aes(color = 'Denied'))
  
  denominator <- -(undecided.weights['PIratio'] - denied.weights['PIratio'])
  solv.diff <- (undecided.weights['solvency'] - denied.weights['solvency'])
  w01 <- undecided.weights['ind']
  w02 <- denied.weights['ind']
  intercept <- (w01 - w02)/denominator
  slope <- solv.diff/denominator
  
  b3 <- geom_abline(size = 1.25,intercept = intercept, slope = slope, aes(color = 'Denied'))
  b6 <- geom_abline(size = 1.25,intercept = intercept+0.29, slope = slope, aes(color = 'Undecided'))
  
  p <- p + b1 + b2 + b3 + b4 + b5 + b6
  direct.label(p, list(last.points, hjust = 0.7, vjust = 1))
  if (save.pdf == TRUE) ggsave("discFunction3C.pdf") 
}

noApproved <- 50
noUndecided <- 50
noDenied <- 50
muApproved <- c(4, 200)
muUndecided <- c(12, 220)
muDenied <- c(10, 100)
sdApproved <- c(2,30)
sdUndecided <- c(2,30)
sdDenied <- c(2,30)
rhoApproved <- -0.5
rhoUndecided <- -0.3
rhoDenied <- 0.3

predictLoanApprovals(noApproved, noUndecided, noDenied,
        muApproved, muUndecided, muDenied,
        sdApproved, sdUndecided, sdDenied,
        rhoApproved, rhoUndecided, rhoDenied)
