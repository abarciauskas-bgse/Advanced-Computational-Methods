# train.digit takes a thinned digit, generates a directed path for that digit
#   and returns a vector of probabilities for each step type, the number of steps in the 
#   and the label of the digit
train.digit <- function(digit) {
  label <- digit$label
  # create the path
  digit.train <- generatePaths(list(digit))
  # also return number of long strokes
  digit.num.real.strokes <- sum(sapply(digit.train[[1]]$direction.steps, length) >= 5)

  # find the longest path
  digit.steps <- digit.train[[1]]$direction.steps[[which.max(sapply(digit.train[[1]]$direction.steps, length))]]
  digit.total.steps <- length(digit.steps)
  digit.steps.probabilities <- rep(0, 8)

  for (step.type in 1:8) {
    # find number of times step happened in the first stroke
    digit.steps.probabilities[step.type] <- sum(digit.steps == step.type)/digit.total.steps
  }
  
  # Add progression of steps
  # Find the first step type that occupies > 10% of the direction
  first.major.step <- NA
  second.major.step <- NA
  major.step.threshold <- round(0.1*digit.total.steps)
  for (i in 1:length(digit.steps)) {
    if (i + major.step.threshold < length(digit.steps)) {
      if (length(unique(digit.steps[i:(i+major.step.threshold)])) == 1) {
        if (is.na(first.major.step)) {
          first.major.step <- digit.steps[i]
        } else if (is.na(second.major.step) && (first.major.step != digit.steps[i])) {
          second.major.step <- digit.steps[i]
          break
        }
      }
    }
  }

  # also the number of strokes
  return(list(
    step.probs = digit.steps.probabilities,
    label = label,
    total.steps = digit.total.steps,
    num.real.strokes = digit.num.real.strokes,
    changes.in.direction = digit.train[[1]]$changes.in.direction,
    first.major.step = first.major.step,
    second.major.step = second.major.step))
}

