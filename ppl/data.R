setwd('~/Downloads/')
if (!require('R.matlab')) install.packages('R.matlab')
library(R.matlab)

d <- readMat('data_background_small1.mat')
names(d) # the only safe thing to do

timing <- d$timing
drawings <- d$drawings
# so it appears each of drawings, images, and timing is setup like
# [[<langauge_index>]][[<character_index>]][[??]]
d$names # 5 language sets