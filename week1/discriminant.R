library(mvtnorm)

# Generate fake loan data
# Payment-Income Ratio (PIRatio)
# Denied v Approved

# Random blob
x <- rmvnorm(100, mean = c(1, 5), sigma = 1*diag(2))
plot(x)

# Adding correlation
rho <- 0.8
sdv1 <- 2
sdv2 <- 2
covTerm <- rho*sdv1*sdv2
cvc <- matrix(c(sdv1^2, covTerm, covTerm, sdv2^2), ncol = 2)

x <- rmvnorm(100, mean = c(1, 5), sigma = cvc)
plot(x)
