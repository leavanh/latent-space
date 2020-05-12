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
# 2,4,6,8 dimensions
# unif and normal distribution
# use mle

sim_2_unif <- gen_fit_all(c(10, 50, 100), dim = 2,
                          distribution = "unif", tofit = "mle")
sim_4_unif <- gen_fit_all(c(10, 50, 100), dim = 4,
                          distribution = "unif", tofit = "mle")
sim_6_unif <- gen_fit_all(c(10, 50, 100), dim = 6,
                          distribution = "unif", tofit = "mle")
sim_8_unif <- gen_fit_all(c(10, 50, 100), dim = 8,
                          distribution = "unif", tofit = "mle")

sim_2_normal <- gen_fit_all(c(10, 50, 100), dim = 2,
                          distribution = "normal", tofit = "mle")
sim_4_normal <- gen_fit_all(c(10, 50, 100), dim = 4,
                            distribution = "normal", tofit = "mle")
sim_6_normal <- gen_fit_all(c(10, 50, 100), dim = 6,
                            distribution = "normal", tofit = "mle")
sim_8_normal <- gen_fit_all(c(10, 50, 100), dim = 8,
                            distribution = "normal", tofit = "mle")

simulation <- list(sim_2_unif, sim_2_normal,
                   sim_4_unif, sim_4_normal,
                   sim_6_unif, sim_6_normal,
                   sim_8_unif, sim_8_normal)
  
save(simulation, file = "simulation.RData") # save
load("simulation.RData") # load

comp_distances(simulation$`100_nodes_6_dim`$network, 
               simulation$`100_nodes_6_dim`$models)
