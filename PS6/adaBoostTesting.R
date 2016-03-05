if (!require('rpart')) install.packages('rpart')
# setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS6')

spam <- read.csv('spambase.data')
n.total.rows <- nrow(spam)

# Generate training and test data
test.portion <- 1/10
train.portion <- 1-test.portion
train.idcs <- sample(1:n.total.rows, round(train.portion*n.total.rows))
test.idcs <- setdiff(1:n.total.rows, train.idcs)
train.spam <- spam[train.idcs,]
test.spam <- spam[test.idcs,]

# Initialize weights
weights <- rep(1/(nrow(train.spam)), nrow(train.spam))

# train an test a shallow tree, as a base model to compare with
base.stump <- rpart(
  as.factor(X1) ~ .,
  data = train.spam,
  control=rpart.control(maxdepth = 1))
# basic accuracy of simplest decision stump
(stump.acc <- 1-sum(predict(base.stump, test.spam, type="class") != as.factor(test.spam$X1))/nrow(test.spam))
# 0.8891304

predictions <- predict(base.stump, test.spam, type="class")
errors.made <- predictions != as.factor(test.spam$X1)

# compute the error of the base classifer
error <- sum(weights*errors.made)/sum(weights)

# compute the error of the classifier
alpha <- log((1-error)/error)

# update weights
weights <- weights*exp(alpha*errors.made)

