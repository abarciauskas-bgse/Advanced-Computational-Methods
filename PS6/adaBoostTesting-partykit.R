if (!require('partykit')) install.packages('partykit')
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
base.stump <- ctree(
  formula = as.factor(X1) ~ .,
  data = train.spam,
  # weights = weights,
  control=ctree_control(maxdepth = 1, stump = TRUE))
# basic accuracy of simplest decision stump
(stump.acc <- 1-sum(predict(base.stump, test.spam, type="response") != as.factor(test.spam$X1))/nrow(test.spam))
# 0.6347826

# Can we make decisions without the base.stump object?
# Fitted party:
#   [1] root
# |   [2] X0.28 <= 0.05: 0 (n = 3401, err = 45.8%)
# |   [3] X0.28 > 0.05: 0 (n = 739, err = 11.9%)
base.stump$terms

predictions <- predict(base.stump, test.spam, type="response")
errors.made <- predictions != as.factor(test.spam$X1)

# compute the error of the base classifer
error <- sum(weights*errors.made)/sum(weights)

# compute the error of the classifier
alpha <- log((1-error)/error)

# update weights
weights <- weights*exp(alpha*errors.made)

test.adaboost <- function(formula, data) {
  stumps <- list()
  base.stump <- ctree(
    formula = formula,
    data = data,
    control=ctree_control(maxdepth = 10))#, stump = TRUE))
  print(base.stump)
  stumps <- append(stumps, list(base.stump))
  stumps <- append(stumps, list(base.stump))
  alphas <- rep(alpha, 2)
  return(list(stumps = stumps, alphas = alphas))
}

res <- test.adaboost(as.formula('X1 ~ .'), train.spam)

# generate predictions on test data
# for every classifier, store the weighted prediction in a matrix of dimension
# nrow = nrow(test.spam), ncol = length(stumps)
#
weighted.preds <- matrix(NA, nrow = nrow(train.spam), ncol = length(res$stumps))
for (i in 1:length(res$alphas)) {
  stump.preds <- res$alphas[i]*predict(res$stumps[[i]], train.spam, type="response")
  weighted.preds[,i] <- stump.preds
}

summary(sign(rowSums(weighted.preds)))

