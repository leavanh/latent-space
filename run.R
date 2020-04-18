## Set up
# load packages

library(latentnet)
library(tidyverse)

# load functions

source("functions.R")


# generate networks

set.seed(09101999)

p <- rsphere(10, dim = 6)
n <- gen_network(p)
plot(n$network)
m <- fit_models(n)
