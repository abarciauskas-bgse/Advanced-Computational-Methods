source('intervals.R')
intervals <- list(c(0.09, 0.17), c(0.24, 0.37))
data <- intervals_2D(50, intervals, epsilon = 0.1, seed = 12345)
plot(data$X[,1], data$X[,2], col = ifelse(data$Y, 'darkred', 'blue'), pch = 19)

source('findThreshold.R')
library(ggplot2)

plot <- ggplot(data = iris, aes(x = Petal.Width, y=Sepal.Length, color = Species)) +
  geom_point()
plot
X <- iris[,c('Petal.Width','Sepal.Length')]
Y <- iris[,'Species']
res <- findThreshold(X, Y)
feature.idx <- res$feature
if (feature.idx == 1) {
  plot <- plot + geom_vline(xintercept = res$thres) 
} else if (feature.idx == 2) {
  plot <- plot + geom_hline(yintercept = res$thres) 
}
plot
