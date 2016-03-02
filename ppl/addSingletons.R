# for all ints in all.thinned.ints, add singletons and num.singletons
source('findSingletons.R')

for (int.idx in 1:length(all.thinned.ints)) {
  curr.points <- all.thinned.ints[[int.idx]]['points'][[1]]
  singletons <- find.singletons(curr.points)
  all.thinned.ints[[int.idx]]['num.singletons'] <- nrow(singletons)
  # length row
  all.thinned.ints[[int.idx]]['first.singleton'] <- 16*(singletons[1,1]-1) + singletons[1,2]
}

first.singletons <- lapply(all.thinned.ints, function(num) {
  c(num$label, num$first.singleton)
})

int.singletons.mat <- matrix(unlist(first.singletons), nrow = length(first.singletons), ncol = 2, byrow = TRUE)
plot(int.singletons.mat[,1], int.singletons.mat[,2], pch = 19)
