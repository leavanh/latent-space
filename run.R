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

simulation <- gen_fit_all(n = c(10, 50, 100), dim = 6)

save(simulation, file = "simulation.RData") # save
load("simulation.RData") # load