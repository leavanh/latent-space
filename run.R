## Set up
# load packages

library(latentnet)
library(tidyverse)

# load functions

source("functions.R")


## generate networks

set.seed(09101999)

# make the networks
# 10, 50 and 100 nodes
# up to 6 dimensions

p <- rsphere(10, dim = 6)
n <- gen_network(p)
plot(n$network)
m <- fit_models(n)
