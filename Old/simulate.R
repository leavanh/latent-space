## load packages

library(latentnet)
library(degreenet)
library(mvtnorm)

## set seed

set.seed(09101999)

## simulate

pos <- rmvnorm(50, c(0,0))
modS1<- ergmm(x ~ pos)

simulate(pos)
