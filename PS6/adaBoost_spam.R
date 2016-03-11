# setwd('~/Box Sync/abarciausksas/second_term/Advanced Computational Methods/PS6/')
if (!require("RColorBrewer")) install.packages("RColorBrewer")

source('adaBoost.R')
source('boostingTests.R')
spam <- read.csv('spambase.data')

#FACTORFACTORFACTOR
spam$X1 <- as.factor(spam$X1)

res <- boosting.tests(spam)

cols=brewer.pal(4,"Spectral")
pdf('adaBoost.pdf') 
plot(res[,1:2], ylab = 'Error Rate', ylim = c(0, 0.25), xlab = 'Boosting Iterations', type = 'l', col = cols[1], lwd = 2.5)
lines(res[,c(1,3)], col = cols[2], lwd = 2.5)
lines(res[,c(1,4)], col = cols[3], lwd = 2.5)
lines(res[,c(1,5)], col = cols[4], lwd = 2.5)
legend('topright', legend = colnames(res[,2:5]),
       col = cols, lty=c(1,1), lwd=c(2.5,2.5))
dev.off()
