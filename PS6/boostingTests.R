source('adaBoost.R')
if (!require('ada')) install.packages('ada')
# function to run ada and my adaboost function on data,
# split into training and test sets
# returns list of error vectors corresponding to sequence of iterations:
# my.ada.train.errors, my.ada.test.errors, ada.train.errors, ada.test.errors
#
boosting.tests <- function(data,
                           test.fraction = 1/10,
                           boosting.iterations = c(1,seq(5,30,5))) {
  # Setup training and test data
  n.total.rows <- nrow(data)
  
  # Generate training and test data
  train.fraction <- 1-test.fraction
  train.idcs <- sample(1:n.total.rows, round(train.fraction*n.total.rows))
  test.idcs <- setdiff(1:n.total.rows, train.idcs)
  data.train <- data[train.idcs,]
  data.test <- data[test.idcs,]
  
  # initialize matrices to store training and test errors
  num.tests <- length(boosting.iterations)
  # store errors in ntests x 5 matrix
  # first column is the number of boosting iterations
  # 2nd and 3rd columns are for my adaboost errors
  # 4th and 5th columns are for ada errors
  test.results.matrix <- matrix(
    c(5*rep(0, num.tests)),
    nrow = num.tests,
    ncol = 5)
  colnames(test.results.matrix) <- c(
    'boosting.iterations',
    'my.adaboost.train.error',
    'my.adaboost.test.error',
    'ada.train.error',
    'ada.test.error')
  test.results.matrix[,'boosting.iterations'] <- boosting.iterations

  for (i in 1:num.tests) {
    # train using my adaboost function with noTrees = iter
    boost.iters <- boosting.iterations[i]
    my.ada.trained <- adaBoost(formula = (X1 ~ .), data = data.train, depth = 1, noTrees = boost.iters)
    test.results.matrix[i,'my.adaboost.train.error'] <- my.ada.trained$error
    
    # make predictions using the trained model
    test.result <- adaboost.predict(my.ada.trained$stumps, my.ada.trained$alphas, data.test)
    test.results.matrix[i,'my.adaboost.test.error'] <- test.result$error

    # For power-users with many variables use ada.fit
    ada.trained <- ada(
      X1 ~ .,
      data = data.train,
      iter = boost.iters,
      loss = 'exponential')

    # FIXME: CHECK THIS IS RIGHT WAY TO DO THINGS
    train.preds <- predict(ada.trained, data.train)
    test.results.matrix[i,'ada.train.error'] <- sum(train.preds != data.train[,'X1'])/nrow(data.train)
  
    test.preds <- predict(ada.trained, data.test)
    test.results.matrix[i,'ada.test.error'] <- sum(test.preds != data.test[,'X1'])/nrow(data.test)
  }
  
  return(test.results.matrix)
}
