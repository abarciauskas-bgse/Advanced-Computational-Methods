# setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS6')

spam <- read.csv('spambase.data')
n.total.rows <- nrow(spam)

#FACTORFACTORFACTOR
spam$X1 <- as.factor(spam$X1)

# Generate training and test data
test.portion <- 1/10
train.portion <- 1-test.portion
train.idcs <- sample(1:n.total.rows, round(train.portion*n.total.rows))
test.idcs <- setdiff(1:n.total.rows, train.idcs)
train.spam <- spam[train.idcs,]
test.spam <- spam[test.idcs,]

source('adaBoost.R')
ada.trained <- adaBoost(formula = (X1 ~ .), data = train.spam, depth = 10)

# test on test data
source('makePredictions.R')
test.result <- make.predictions(ada.trained$stumps, ada.trained$alphas, test.spam)

