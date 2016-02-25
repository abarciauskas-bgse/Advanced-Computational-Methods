intervals_1D <- function(noObs, intervals, epsilon = 0.5, 
                         seed = round(runif(1)*100000)) {
    set.seed(seed)

    # create 1D input points, where each observation is drawn independently
    # from the uniform distribution, interval from 0 to 1
    X <- runif(noObs)
    X <- sort(X)

    # create intervals dynamically with the help of parsing expressions,
    # and verify which inputs fall into them
    condition <- character()
    for (i in 1:length(intervals)) {
        if (i==1) {
            express <- paste0("(X > ", intervals[[i]][1], " & X < ",
                              intervals[[i]][2],")" )
        } else {
            express <- paste0(" | (X > ", intervals[[i]][1], " & X < ",
                              intervals[[i]][2],")" )
        } 
        condition <- paste0(condition, express)
    }
    withinIntervals <- eval(parse(text = condition))

    
    # create classes
    # 1 if within the intervals with probability 0.5 + epsilon, 
    # 0 if outside of the intervals with probability 0.5 - epsilon
    Y <- rep(NA, noObs)
    Y[withinIntervals]  <- rbinom(sum(withinIntervals),  1, 0.5 + epsilon)
    Y[!withinIntervals] <- rbinom(sum(!withinIntervals), 1, 0.5 - epsilon)

    return(list(X=X, Y=Y))
}

intervals <- list(c(0.09, 0.17), c(0.24, 0.37))
data <- intervals_1D(50, intervals, 0.4, 12345)

library(ggplot2)
ggplot(data=as.data.frame(data), aes(x=X, y=Y, color=factor(Y))) +
geom_point(shape=124, size=4) + 
scale_color_manual("Class", values=c("blue", "red")) +
coord_fixed(ratio=1/4) +
theme_bw() +
annotate("rect", 
         xmin = intervals[[1]][1], 
         xmax = intervals[[1]][2], 
         ymin = 0, ymax = 1, alpha = .2) +
annotate("rect", 
         xmin = intervals[[2]][1], 
         xmax = intervals[[2]][2], 
         ymin = 0, ymax = 1, alpha = .2)

K <- 2
classResults <- cTree(K, data$X, data$Y)
