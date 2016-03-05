make.predictions <- function(stumps = list(), alphas = c(), data = matrix()) {
  weighted.preds <- matrix(NA, nrow = nrow(data), ncol = length(stumps))
  for (i in 1:length(alphas)) {
    # (weighted for stump) probability of being a 1
    prediction <- predict(stumps[[i]], newdata=data, type="class")
    weighted.preds[,i] <- ifelse(as.numeric(prediction) == 2, 1, -1)
  }

  preds <- ifelse(sign(weighted.preds%*%alphas) == 1, 1, 0)
  actual <- data[,'X1']
  error.rate <- sum(preds != actual)/length(preds)
  print(error.rate)
  return(list(preds = preds, error.rate = error.rate))
}
