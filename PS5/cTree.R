#setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS5/')
source('findThreshold.R')
if (!require('formula.tools')) install.packages('formula.tools')

# next we develop the main function that will use findThreshold function
# to find split points locally but decide between them in a greedy way
#
cTree <- function(formula, data, depth = 3, minPoints = 1, costFnc = 'Entropy') {
    Y.var <- get.vars(lhs(formula))
    X.vars <- get.vars(rhs(formula))
    Y <- data[,Y.var]
    X <- data[,X.vars]

    global.thresholds <- list()
    subregions <- list()
    probs <- rep(1,length(Y))
    for (d in 1:depth) {
      if (length(subregions) == 0) {
        # determine first cut to make
        global.thresholds[[1]] <- findThreshold(X, Y, costFnc)
        # split data
        # add to subregions
        current.threshold <- global.thresholds[[1]]
        thres.breaks <- c(min(X[,current.threshold$feature]),
                          current.threshold$thres,
                          max(X[,current.threshold$feature]))
        # FIXME: if there is a parent, first split by parent
        X.first.cuts <- cut(X[,current.threshold$feature],include.lowest = TRUE, thres.breaks)
        subregions <- by(X, X.first.cuts, FUN=I)
        first.cut.left.rows <- as.numeric(rownames(subregions[[1]][1]))
        first.cut.right.rows <- as.numeric(rownames(subregions[[2]][1]))
        Y.preds <- rep(NA, length(Y))
        Y.preds[first.cut.left.rows] <- current.threshold$labels[1]
        Y.preds[first.cut.right.rows] <- current.threshold$labels[2]
        (initial.error <- sum(as.numeric(Y) != Y.preds)/length(Y))
      } else {
        thresholds.for.this.depth <- list()
        global.errors.for.depth <- c()
        subregions.for.this.depth <- c()
        Y.preds.for.this.depth <- matrix(Y.preds)
        # find threshold in each subregion which minimizes local error
        # select threshold which minimizes global error
        for (subregion.idx in 1:length(subregions)) {
          # find threshold for this region
          curr.X <- subregions[subregion.idx][[1]]
          curr.X.rows <- as.numeric(rownames(curr.X))
          curr.Y <- Y[curr.X.rows]
          # if curr.Y is not pure
          # and there is more than min points in the region
          if ((length(unique(curr.Y)) > 1) && (length(curr.Y) > minPoints)) {
            # split the subregion into two subregions
            curr.threshold <- findThreshold(curr.X, curr.Y, costFnc)
            thres.breaks <- c(min(curr.X[,curr.threshold$feature])-1e-6,
                              curr.threshold$thres,
                              max(curr.X[,curr.threshold$feature])+1e-6)
            X.current.cuts <- cut(curr.X[,curr.threshold$feature],include.lowest = TRUE, thres.breaks)
            curr.subregions <- by(curr.X, X.current.cuts, FUN=I)
            curr.X.left.rows <- as.numeric(rownames(curr.subregions[[1]][1]))
            curr.X.right.rows <- as.numeric(rownames(curr.subregions[[2]][1]))
            Y.preds.for.this.depth <- cbind(Y.preds.for.this.depth, Y.preds)
            Y.preds.for.this.depth[curr.X.left.rows,subregion.idx] <- curr.threshold$labels[1]
            Y.preds.for.this.depth[curr.X.right.rows,subregion.idx] <- curr.threshold$labels[2]
            Y.preds.current <- Y.preds.for.this.depth[,subregion.idx]
            global.error.for.this.cut <- sum(Y.preds.current != as.numeric(Y))/length(Y)
            # make predictions for global based on this split
            curr.threshold$global.error <- global.error.for.this.cut
            global.errors.for.depth <- append(global.errors.for.depth, global.error.for.this.cut)
            subregions.for.this.depth <- append(subregions.for.this.depth, subregion.idx)
            thresholds.for.this.depth[[(length(thresholds.for.this.depth)+1)]] <- curr.threshold
          }
        }
        
        # Find the lowest error threshold and add that subregion to the global subregions
        best.thresh.for.depth <- which.min(global.errors.for.depth)
        threshold.to.add <- thresholds.for.this.depth[best.thresh.for.depth]
        # replace subregions[best.thres.for.depth] with split on this threshold
        region.to.split <- subregions.for.this.depth[best.thresh.for.depth]
        X.to.split <- subregions[region.to.split][[1]]
        curr.X.rows <- as.numeric(rownames(X.to.split))
        thres.breaks <- c(min(X.to.split[,curr.threshold$feature])-1e-6,
                          curr.threshold$thres,
                          max(X.to.split[,curr.threshold$feature])+1e-6)
        X.current.cuts <- cut(X.to.split[,curr.threshold$feature],include.lowest = TRUE, thres.breaks)
        curr.subregions <- by(X.to.split, X.current.cuts, FUN=I)
        new.subregions <- subregions
        new.subregions[region.to.split] <- curr.subregions[1]
        # not sure this is okay but we can try it...
        new.subregions[length(new.subregions)+1] <- curr.subregions[2]
        subregions <- new.subregions
        Y.preds <- Y.preds.for.this.depth[,region.to.split]
        
        mistakes <- sum(Y.preds.for.this.depth[curr.X.rows,region.to.split] != as.numeric(Y[curr.X.rows]))
        cut.prob <- 1-(mistakes)/length(curr.X.rows)
        probs[curr.X.rows] <- rep(cut.prob, length(curr.X.rows))
      }
    }

    # Now to cut up the data
    return(list(predLabels = Y.preds, prob = probs))
}

# res <- cTree(formula = as.formula('Species ~ Petal.Width + Sepal.Width'),data =iris)
# sum(as.numeric(iris$Species) != res$predLabels)/nrow(iris)
# mean(res$prob)
