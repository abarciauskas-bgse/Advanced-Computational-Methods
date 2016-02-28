#setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS5/')
if (!require('partykit')) install.packages('partykit')
source('cTree.R')

data <- read.csv('spambase.data')
formula <- as.formula('X1 ~ .')
max.depth <- 20

# split data into training and test
# for k in 1:max.depth
nobs <- nrow(data)
test.data.nobs <- nobs/10
training.data <- data[1:(nobs-test.data.nobs),]
test.data <- data[((nobs-test.data.nobs)+1):nrow(data),]
# generate the test error from training using cTree 
# and add to errors

errors.party <- rep(NA, max.depth)
errors.me <- rep(NA, max.depth)
for (depth in 2:max.depth) {
  print(paste('depth:', depth))
  model <- ctree(formula, training.data, control = ctree_control(maxdepth = depth))
  preds <- ifelse(predict(model, test.data[,1:(ncol(data)-1)]) > 0.5, 1, 0)
  error.rate <- sum(preds != test.data[,ncol(test.data)])/nrow(test.data)
  print(error.rate)
  errors.party[depth] <- error.rate
  
  res <- cTree(formula, data = data, depth = k)
  table(res$predLabels)
  error.rate <- sum(data[,'X1'] != res$predLabels)/length(res$predLabels)
  print(error.rate)
  errors.me[depth] <- error.rate
}

pdf("cTree.pdf")
plot(errors.party, type = 'l', col = 'red', xlab = 'depth', ylab = 'test error rate')
lines(errors.me, col = 'blue')
dev.off()
