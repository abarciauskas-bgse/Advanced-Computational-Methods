if (!require('rpart')) install.packages('rpart')
if (!require('formula.tools')) install.packages('formula.tools')
source('adaboostPredict.R')

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
  (formula <- as.formula(paste(y.column, '~', paste(feature.columns, collapse = '+'))))
  data.y <- as.factor(data[,y.column])
  data.x <- data[,feature.columns]
  data <- cbind(data.y, data.x)
  colnames(data)[1] <- y.column
  # Step 1: for m in 1 to M
  for (tree.idx in 1:noTrees) {
    # Fit a classifier G_m(x)
    stump <- rpart(
      formula = formula,
      data = data,
      weights = weights,
      control = rpart.control(maxdepth = depth))

    # fitted values of data
    preds <- predict(stump, data, type = 'class')

    # compute error: 1 if mistake was made, 0 otherwise
    errors.made <- ifelse(preds != data.y, 1, 0)

    # compute the error of the classifier
    error <- sum(weights*errors.made)/sum(weights)
    # compute alpha
    alpha <- 0.5*log((1-error)/error)
    # store the m-th alpha
    alphas[tree.idx] <- alpha

    # update weights
    weights <- weights*exp(alpha*errors.made)

    # store the m-th classifer
    stumps <- append(stumps, list(stump))
  }

  # calculate final predictions
  final.preds <- adaboost.predict(stumps, alphas, data)
  return(list(stumps = stumps,
              alphas = alphas,
              predLabels = final.preds$preds,
              error = final.preds$error.rate))
}
