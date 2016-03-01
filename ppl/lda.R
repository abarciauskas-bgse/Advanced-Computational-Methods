stroke <- white.pixels.thinned
# i <- 1
# stroke <- matrix(unlist(unique(strokes[[i]])), nrow = length(unique(strokes[[i]])), ncol = 2, byrow = TRUE)
apply(stroke, 1, plot.point)
x <- stroke[,1]
y <- stroke[,2]
px.sd <- 0.25
library(MASS)
fm2 <- lda(y ~ ns(x, df=5))
fm3 <- lda(x ~ ns(y, df=5))

x.test <- seq(min(x), max(x), length.out = 100)
y.test <- seq(min(y), max(y), length.out = length(y))

x.out <- x.test + rnorm(100, mean = 0, sd = px.sd)
y.preds <- predict(fm2, data.frame(x = x.out))

y.out <- y.test + rnorm(length(y), mean = 0, sd = px.sd)
x.preds <- predict(fm3, data.frame(y.out))

y.num <- as.numeric(as.character(y.preds$class)) + rnorm(100, mean = 0, sd = px.sd)
x.num <- as.numeric(as.character(x.preds$class)) + rnorm(length(y), mean = 0, sd = px.sd)

plot(white.pixels.thinned, pch = 19)
points(x.out, y.num, pch = 19, col = 'blue')
points(x.num, y.out, pch = 19, col = 'blue')


# should transform it in other ways, stretched up and down and in and out
# basically different variances and variances varying by x,y
#