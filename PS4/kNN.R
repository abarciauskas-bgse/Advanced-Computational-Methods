if (!require('dplyr')) install.packages('dplyr')
library(dplyr)
if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('ggplot2')) install.packages('ggplot2')
library(ggplot2)

my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

kNN <- function(X, y, train.set = NULL, 
                k = 1, p = 2, type="train") {
    
    # test the inputs
    library(assertthat)
    not_empty(X); not_empty(y); 
    if (type == "train") {
        assert_that(nrow(X) == length(y))
    }
    is.string(type); assert_that(type %in% c("train", "predict"))
    is.count(k); 
    assert_that(p %in% c(1, 2, Inf))
    if (type == "predict") {
        assert_that(not_empty(train.set) & 
                    ncol(train.set) == ncol(X) & 
                    nrow(train.set) == length(y))
    }

    # Compute the distance between each point and all others 
    noObs <- nrow(X)
    
    # if we are making predictions on the test set based on the train.set, 
    # we compute distances between each test observation and observations
    # in our train.set
    if (type == "train") {
        distMatrix <- matrix(NA, noObs, noObs)
        for (obs in 1:noObs) {
            
            # values of the current observation
            x.current <- as.numeric(X[obs,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of noObs x ncol(x.current)
            x.current.expanded <- matrix(x.current, nrow = noObs, ncol = 2, byrow = TRUE)

            # computing distances between the current x and everyone else in the training set
            #
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(X - 
                                      x.current.expanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(X - x.current.expanded), 1, max)
            }  
        }
    } else if (type == "predict") {
        notrain.set <- nrow(train.set)
        distMatrix <- matrix(NA, noObs, notrain.set)
        for (obs in 1:noObs) {
           
            # feature values for the current observation
            x.current <- as.numeric(X[obs,])
            # create a matrix to compute the distance of each observation from the current x
            # having dimension of notrain.set (which now behaves as the training set) x ncol(x.current)
            x.current.expanded <- matrix(x.current, nrow = notrain.set, ncol = 2, 
                                    byrow = TRUE)

            # computing distances between the probe and exemplars in the train.set
            if (p %in% c(1,2)) {
                distMatrix[obs, ] <- (rowSums((abs(train.set - 
                                      x.current.expanded))^p) )^(1/p)
            } else if (p==Inf) {
                distMatrix[obs, ] <- apply(abs(train.set - x.current.expanded), 1, max)
            }  
        }
    }
    
    # Sort the distances in increasing numerical order and pick the first 
    # k elements
    neighbors <- apply(distMatrix, 1, order) 

    # Compute and return the most frequent class in the k nearest neighbors
    predLabels <- rep(NA, noObs)
    prob <- rep(NA, noObs)
    for (obs in 1:noObs) {
        predLabels[obs] <- my.mode(y[neighbors[1:k, obs]])
        # sum the total neighbors predicting the predicted label
        prob[obs] <- sum(y[neighbors[1:k,obs]] == predLabels[obs])/k
    }

    # examine the performance, available only if training
    if (type == "train") {
        errorCount <- table(predLabels, y)
        accuracy <- mean(predLabels == y)
    } else if (type == "predict") {
        errorCount <- NA
        accuracy <- NA
    }

    # return the results
    return(list(predLabels = predLabels, 
                prob = prob,
                accuracy = accuracy,
                errorCount = errorCount))
}

# k class 2 dimensional mixture of Gaussians
genGaussMix <- function(noObs = c(100, 100, 100), 
                        noGaussians = 10, 
                        mixtureProb = rep(1/noGaussians, noGaussians),
                        seed = 2222) {
    
    set.seed(seed)

    # producing means of our bivariate Gaussians
    meansC1 <- rmvnorm(noGaussians, mean = c(0,1), sigma = diag(2))
    meansC2 <- rmvnorm(noGaussians, mean = c(1,2), sigma = diag(2))
    meansC3 <- rmvnorm(noGaussians, mean = c(2,3), sigma = diag(2))

    # for each observation we first randomly select one Gaussian and then 
    # generate a point according to the parameters of that Gaussian
    whichGaussianC1 <- sample(nrow(meansC1), noObs[1], 
                              mixtureProb, replace = TRUE)
    whichGaussianC2 <- sample(nrow(meansC2), noObs[2], 
                              mixtureProb, replace = TRUE)
    whichGaussianC3 <- sample(nrow(meansC3), noObs[3], 
                              mixtureProb, replace = TRUE)

    # now drawing samples from selected bivariate Gaussians
    drawsC1 <- whichGaussianC1 %>% 
        sapply(function(x) rmvnorm(1, mean = meansC1[x,], 
               sigma = diag(2)/5)) %>% t()
    drawsC2 <- whichGaussianC2 %>% 
        sapply(function(x) rmvnorm(1, mean = meansC2[x,], 
               sigma = diag(2)/5)) %>% t()
    drawsC3 <- whichGaussianC3 %>% 
        sapply(function(x) rmvnorm(1, mean = meansC3[x,],
                                   sigma = diag(2)/5)) %>% t()
    # combining and labeling
    dataset <- data.frame(rbind(drawsC1, drawsC2, drawsC3), 
                          label = c(rep("C1", noObs[1]), rep("C2", noObs[2]), rep("C3", noObs[3])), 
                          y = c(rep(0, noObs[1]), rep(1, noObs[2]), rep(2, noObs[3])),
                          stringsAsFactors = FALSE)
    return(dataset)
}


# plotting function
plot2dClasses <- function(dataset) {
    ggplot(data = dataset, 
           aes(x = X1, y = X2, colour = factor(y))) + 
    geom_point(size = 2, shape = 4) +
    xlab("X1") +
    ylab("X2") +
    theme_bw() + 
    theme(text=element_text(family="Helvetica")) +
    scale_color_manual("Class", 
        values = c("0" = "blue", "1" = "red"))
}

dataset <- genGaussMix()
datasetTest <- genGaussMix(noObs=c(200, 200, 200), seed=1234)

# specify parameters
k <- 1  # odd number
p <- 2  # Manhattan (1), Euclidean (2) or Chebyshev (Inf)

# training 
trainResults <- kNN(X = dataset[,1:2], y = dataset[,4], 
                    k = k, p = p, type = "train")
head(trainResults$predLabels)
head(trainResults$prob)

# accuracy
trainResults$errorCount
trainResults$accuracy

# test
testResults <- kNN(X = datasetTest[,1:2], y = dataset[,4], 
                   train.set = dataset[,1:2], 
                   k = k, p = p, type = "predict")

head(testResults$predLabels)
head(testResults$prob)

# accuracy
mean(testResults$predLabels == datasetTest[,4])


# ----
# choosing k
# ----

# we examine the classifier performance on the test set for many k's
k <- c(1,3,5,7,9,11,15,17,23,25,35,45,55,83,101,151 ); 
p <- 2


errorTrain <- errorTest <- rep(NA, length(k))
for (iter in 1:length(k)) {
    # get the training error
    errorTrain[iter] <- 1 - kNN(X = dataset[,1:2], 
                                y = dataset[,4], 
                                k = k[iter], p = p, 
                                type = "train")$accuracy
    # get the test error
    predLabels <- kNN( X = datasetTest[,1:2], 
                             y = dataset[,4],
                             train.set = dataset[,1:2], 
                             k = k[iter], p = p, 
                             type = "predict")$predLabels
    errorTest[iter] <- mean(predLabels!=datasetTest[,4])
}


# reshaping the data a bit
plottingData <- 
    data.frame(k, Train=errorTrain, Test=errorTest) %>%
    reshape2::melt(id="k", 
                   variable.name = "Sample", 
                   value.name = "Error")

# plotting the generalization error for those k's
ggplot(data = plottingData, 
       aes(x = factor(k), y = Error, colour=Sample, group=Sample)) + 
geom_line() + 
geom_point() +
xlab("k -Number of nearest neighbors") +
ylab("Misclassification error") +
theme_bw(base_size = 14, base_family = "Helvetica") + 
scale_color_manual("Sample", 
    values = c("Train" = "blue", "Test" = "red"))
