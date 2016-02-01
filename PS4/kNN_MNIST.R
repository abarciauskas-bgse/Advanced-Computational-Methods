# Making predictions on MNIST dataset

# REMOVE
# setwd('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/MNIST/')
if (!require('class')) install.packages('class')
library(class)
if (!require('dplyr')) install.packages('dplyr')
library(dplyr)

success.rate <- function(predictions, actual) {
  errors <- 0
  # count number of mistakes
  for (i in 1:length(actual)) {
    if (predictions[i] != actual[i]) {
      errors <- errors + 1
    }
  }
  return (1 - errors/length(actual))
}

data.train <- read.csv('MNIST_training.csv')

# test with some data
# FIXME: Call it `validation`
test.size <- 1000
test.sample.rows <- sample(1:5999, test.size)
train.sample.rows <- setdiff(1:5999, test.sample.rows)
data.train.sample <- data.train[train.sample.rows,]
data.test.sample <- data.train[test.sample.rows,]
X.train <- data.train.sample[,2:ncol(data.train.sample)]
X.test <- data.test.sample[,2:ncol(data.test.sample)]
numbers.train <- data.train.sample[,1]
numbers.test <- data.test.sample[,1]
res <- knn(train = X.train, test = X.test, cl = numbers.train)

# optimize over k
success.rate(res, numbers.test)
ks <- c(1,3,10,50,100)
ks.results <- list()

for (i in 1:length(ks)) {
  k <- ks[i]
  res <- knn(train = X.train, test = X.test, cl = numbers.train, k = k)
  ks.results[toString(k)] <- success.rate(res, numbers.test)
}
# k = 1 is best predictor!

# TODO
# Optimize over p
# http://www.inside-r.org/packages/cran/knnflex/docs/knn.dist?

# save a csv file named MNIST_predictions.csv with a single column
# predicted labels for each observation in the MNIST_test.csv file
data.test <- read.csv('MNIST_test.csv')
train.X <- data.train[,2:ncol(data.train)]
train.class <- data.train[,1]
res <- knn(train = train.X, test = data.test, cl = train.class, k = 1)
filename <- 'MNIST_predictions.csv'
# setwd('../../../Advanced Computational Methods/PS4/')
write.csv(as.vector(res), file=filename, row.names = FALSE)
print(paste('Saved file:', paste0(getwd(), '/', filename)))
