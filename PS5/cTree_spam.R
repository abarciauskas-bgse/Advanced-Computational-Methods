# setwd('~/Box Sync/abarciausksas/myfiles/15D012 Advanced Computational Methods/datasets/Spam')
source('cTree.R')

data <- read.csv('spambase.data')
head(data)

formula <- as.formula('X1 ~ .')

res <- cTree(formula, data = data, depth = 50)
table(res$predLabels)

if (!require('partykit')) install.packages('partykit')
ctree(formula, data)

# split data into training and test
# for k in 1:max.depth
nobs <- nrow(data)
test.data.nobs <- nobs/10
training.data <- data[1:(nobs-test.data.nobs),]
test.data <- data[((nobs-test.data.nobs)+1):nrow(data),]
# generate the test error from training using cTree 
# and add to errors
max.depth <- 20
errors <- rep(NA, max.depth)
for (depth in 1:max.depth) {
  model <- ctree(formula, training.data, control = ctree_control(maxdepth = depth))
  preds <- ifelse(predict(model, test.data[,1:(ncol(data)-1)]) > 0.5, 1, 0)
  error.rate <- sum(preds != test.data[,ncol(test.data)])/nrow(test.data)
  print(error.rate)
  errors[depth] <- error.rate
}

plot(errors, type = 'l', col = 'red', xlab = 'depth', ylab = 'error rate')
