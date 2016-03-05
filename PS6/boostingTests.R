source('makePredictions.R')
source('adaBoost.R')
# function to run gbm and my adaboost function on data,
# split into training and test sets
# returns list of error vectors corresponding to sequence of iterations:
# my.ada.train.errors, my.ada.test.errors, gbm.train.errors, gbm.test.errors
#
boosting.tests <- function(data,
                           test.fraction = 1/10,
                           boosting.iterations = c(1,seq(5,30,5))) {
  # Setup training and test data
  n.total.rows <- nrow(data)
  
  #FACTORFACTORFACTOR
  data$X1 <- as.factor(data$X1)
  
  # Generate training and test data
  train.fraction <- 1-test.fraction
  train.idcs <- sample(1:n.total.rows, round(train.fraction*n.total.rows))
  test.idcs <- setdiff(1:n.total.rows, train.idcs)
  data.train <- data[train.idcs,]
  data.test <- data[test.idcs,]
  
  # initialize matrices to store training and test errors
  num.tests <- length(boosting.iterations)
  test.results.matrix <- matrix(c(boosting.iterations, rep(0, num.tests)), nrow = num.tests, ncol = 2)
  colnames(test.results.matrix) <- c('boosting.iterations', 'error')
  my.ada.train.errors <- test.results.matrix
  my.ada.test.errors <- test.results.matrix
  gbm.train.errors <- test.results.matrix
  gbm.test.errors <- test.results.matrix

  for (i in 1:num.tests) {
    # train using my adaboost function with noTrees = iter
    boost.iters <- boosting.iterations[i]
    # FIXME - add depths
    source('makePredictions.R')
    my.ada.trained <- adaBoost(formula = (X1 ~ .), data = data.train, depth = 1, noTrees = boost.iters)
    my.ada.train.errors[i,2] <- my.ada.trained$train.error
    
    # make predictions using the trained model
    test.result <- make.predictions(my.ada.trained$stumps, my.ada.trained$alphas, test.data)
    my.ada.test.errors[i,2] <- test.result$error.rate
    
    # help a data out
    y.column <- 'X1'
    feature.columns <- setdiff(colnames(data), y.column)
    data.train.y <- as.vector(data.train[,y.column])
    data.train.x <- data.train[,feature.columns]
    data.test.y <- as.vector(data.test[,y.column])
    data.test.x <- data.test[,feature.columns]
    # For power-users with many variables use gbm.fit
    gbm.trained <- gbm.fit(data.train.x,
                           data.train.y,
                           distribution = 'bernoulli',
                           n.trees = boost.iters)
    # FIXME: CHECK THIS IS RIGHT WAY TO DO THINGS
    train.preds <- ifelse(inv.logit(gbm.trained$fit) > 0.5, 1, 0)
    gbm.train.errors[i,2] <- sum(train.preds != data.train.y)/length(data.train.y)
    
    test.preds <- ifelse(predict(gbm.trained, newdata = data.test.x, n.trees = boost.iters) > 0.5, 1, 0)
    gbm.test.errors[i,2] <- sum(test.preds != data.test.y)/length(data.test.y)
  }
  
  return(list(my.ada.train.errors = my.ada.train.errors,
              my.ada.test.errors = my.ada.test.errors,
              gbm.train.errors = gbm.train.errors,
              gbm.test.errors = gbm.test.errors))
}
