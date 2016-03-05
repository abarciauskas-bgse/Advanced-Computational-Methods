if (!require('rpart')) install.packages('rpart')
if (!require('formula.tools')) install.packages('formula.tools')
source('makePredictions.R')

# example:
# res <- adaBoost(formula = (X1 ~ .), data = train.spam)
adaBoost <- function(formula = formula(), data = matrix(), depth = 1, noTrees = 3) {
  # initialize weights, list of stumps, list of alphas
  # noTrees is the number of stumps and alphas to have
  weights <- rep(1/nrow(data), nrow(data))
  alphas <- rep(0, noTrees)
  stumps <- list()

  y.column <- get.vars(lhs(formula))
  feature.columns <- get.vars(rhs(formula))
  if (feature.columns == '.') {
    feature.columns <- setdiff(colnames(data), y.column)
  }
  data.y <- data[,y.column]
  data.x <- data[,feature.columns]

  for (tree.idx in 1:noTrees) {
    # create a stump
    stump <- rpart(
      formula = formula,
      data = data,
      weights = weights,
      control=rpart.control(maxdepth = depth))

    # fitted values of data
    preds <- predict(stump, data)
    preds <- as.factor(apply(preds, 1, which.max)-1)

    # compute error
    errors.made <- preds != data.y

    # compute the error of the stump
    error <- sum(weights*errors.made)/sum(weights)
    # compute alpha
    alpha <- 0.5*log((1-error)/error)
    
    # update weights
    weights <- weights*exp(alpha*errors.made)

    # store the stump and alpha
    stumps <- append(stumps, list(stump))
    alphas[tree.idx] <- alpha
  }

  # calculate final predictions
  final.preds <- make.predictions(stumps, alphas, data)
  return(list(stumps = stumps,
              alphas = alphas,
              predLabels = final.preds$preds,
              train.error = final.preds$error.rate))
}
