source('intervals.R')
intervals <- list(c(0.09, 0.17), c(0.24, 0.37))
data <- intervals_2D(50, intervals, epsilon = 0.1, seed = 12345)
plot(data$X[,1], data$X[,2], col = ifelse(data$Y, 'darkred', 'blue'), pch = 19)

source('findThreshold.R')
res <- findThreshold(data$X, data$Y)
t <- res$thres
res$minerror
abline(h=t)

library(ggplot2)
subsets<-split(diamonds, diamonds$cut, drop=TRUE)
diamonds.subset <- rbind(subsets[[1]], subsets[[2]])
diamonds.X <- cbind(diamonds.subset$depth, diamonds.subset$price)
diamonds.Y <- diamonds.subset$cut
res <- findThreshold(diamonds.X, diamonds.Y)

plot <- ggplot(data = diamonds.subset, aes(x = depth, y = price, color = cut)) + 
  geom_point() +
  geom_vline(xintercept = res$thres)
plot

