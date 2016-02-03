if (!require('caret')) install.packages('caret')
if (!require('e1071')) install.packages('e1071')
library(caret)
library(e1071)

# Train model
knnModel <- train(Species ~., 
                  data = iris, 
                  method = 'knn')

# Create grid of values
lgrid <- expand.grid(Petal.Length=seq(1, 5, by=0.1), 
                     Petal.Width=seq(0.1, 1.8, by=0.1),
                     Sepal.Length = 5.4,
                     Sepal.Width=3.1)

# Predict grid of values
knnPredGrid <- predict(knnModel, newdata=lgrid)
knnPredGrid = as.numeric(knnPredGrid) # 1 2 3


(pl = seq(1, 5, by=0.1))
(pw = seq(0.1, 1.8, by=0.1))
probs <- matrix(knnPredGrid, length(pl), 
                length(pw))

contour(pl, pw, probs, labels="", xlab="", ylab="", main=
          "X-nearest neighbour", axes=FALSE)

gd <- expand.grid(x=pl, y=pw)

points(gd, pch=".", cex=5, col=probs)
box()   
