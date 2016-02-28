if (!require('data.tree')) install.packages('data.tree')
#setwd('~/Box Sync/abarciausksas/myfiles/Advanced Computational Methods/PS5/')
source('findThreshold.R')

# findThresholds will recurse on tree structure to return tree of thresholds
findThresholds <- function(X, Y, current.tree = NA, current.node = NA, current.node.idx = 1, current.depth = 0, max.depth = 3, regions = list()) {
  threshold <- findThreshold(X,Y)
  if (is.na(threshold[[1]])) {
    current.node.left <- NA
    current.node.right <- NA
    if (!(typeof(current.tree) == 'environment')) {
      current.node <- paste0(features[threshold$feature], ' <> ', threshold$thres)
      current.tree <- Node$new(current.node)
    } else {
      # add two nodes, one less, one greater
      current.node.left <- paste0(features[threshold$feature], ' < ', threshold$thres)
      current.node.right <- paste0(features[threshold$feature], ' >= ', threshold$thres)
      current.tree$current.node$AddChild <- current.node.left
      current.tree$current.node$AddChild <- current.node.right
    }
  
    node.left.idx <- current.node.idx + 1
    node.right.idx <- current.node.idx + 2
    regions[[node.left.idx]] <- threshold$data.left
    regions[[node.right.idx]] <- threshold$data.right
  } else {
    return(list(tree = current.tree, regions = regions))
  }

  left <- NA
  right <- NA
  if (current.depth == max.depth) {
    return(list(tree = current.tree, regions = regions))
  } else {
    left <- findThresholds(threshold$data.left$X,
                           threshold$data.left$Y,
                           current.tree = current.tree,
                           current.node = current.node.left,
                           current.node.idx = node.left.idx,
                           current.depth = current.depth + 1,
                           max.depth = max.depth,
                           regions = regions)

    right <- findThresholds(threshold$data.right$X,
                            threshold$data.right$Y,
                            current.tree = current.tree,
                            current.node = current.node.right,
                            current.node.idx = node.right.idx,
                            current.depth = current.depth + 1,
                            max.depth = max.depth,
                            regions = regions)
  }
}


features <- c('Petal.Width','Sepal.Width')
X <- iris[,features]
Y <- iris[,'Species']
source('findThreshold.R')
regions <- list()
regions[[1]] <- NA
res <- findThresholds(X, Y, regions = regions)
