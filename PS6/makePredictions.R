make.predictions <- function(stumps = list(), alphas = c(), data = matrix()) {
  weighted.probs <- matrix(NA, nrow = nrow(data), ncol = length(stumps))
  for (i in 1:length(alphas)) {
    # (weighted for stump) probability of being a 1
    stump.probs.1 <- alphas[i]*predict(stumps[[i]], data, type="prob")[,2]
    weighted.probs[,i] <- stump.probs.1
  }
  preds <- ifelse(rowSums(weighted.probs) > 0.5, 1, 0)
  error.rate <- sum(preds != data[,'X1'])/nrow(data)
  return(list(probs = weighted.probs, preds = preds, error.rate = error.rate))
}
