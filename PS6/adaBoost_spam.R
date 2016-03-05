# setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS6/')
if (!require('gbm')) install.packages('gbm')
source('adaBoost.R')
source('boostingTests.R')
spam <- read.csv('spambase.data')

#FACTORFACTORFACTOR
spam$X1 <- as.factor(spam$X1)

res <- boosting.tests(spam)
