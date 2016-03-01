
plot(white.pixels.thinned, pch = 19)
for (i in 1:length(strokes)) {
  if (length(strokes[[i]]) >= 10) {
    stroke <- matrix(unlist(unique(strokes[[i]])), nrow = length(unique(strokes[[i]])), ncol = 2, byrow = TRUE)
    
    apply(stroke, 1, plot.point)
    x <- stroke[,1]
    y <- stroke[,2]
    library(MASS)
    fm2 <- lda(y ~ ns(x, df=5))
    fm1 <- lm(y ~ bs(x, degree = 3, df = 5))
    
    # plot(x, y, xlab = "xs", ylab = "ys",
    #      xlim = range(white.pixels.thinned[,1]),
    #      ylim = range(white.pixels.thinned[,2]),
    #      pch = 19)
    ht <- seq(min(x), max(x), length.out = 100)
    lines(ht, predict(fm1, data.frame(x = ht)), col = 'red')
    preds <- predict(fm2, data.frame(x = ht))
    y.num <- as.numeric(as.character(preds$class))
    points(ht, y.num, pch = 19, col = 'blue')
    fm3 <- lm(y.num ~ bs(ht, degree = 3, df = 5))
    # this is silly but it looks cool.
    lines(ht, fm3$fitted.values, col = 'orange')
  }
}
