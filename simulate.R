## load packages

library(latentnet)
library(degreenet)
library(mvtnorm)

## set seed

set.seed(09101999)

## simulate

S1 <- rmvnorm(50, c(0,0))
modS1<- ergmm(S1 ~ euclidean(d=2, G=3))

plot(simulate(modS1))