## Set up
# load packages

library(latentnet)
library(tidyverse)
library(mvtnorm)

# load functions

source("functions.R")


## generate networks

set.seed(2020)

# make the networks
# 20, 50 and 100 nodes
# 2,4,6,8 dimensions
# unif and normal distribution
# use mle

start_time <- Sys.time()

simulation_unif <- gen_fit_all(n = c(20, 50, 100), dim = c (2, 4, 6, 8), 
                               distribution = "unif", tofit = "mle")
save(simulation_unif, file = "simulation_unif.RData") # save

end_time <- Sys.time()
time_diff_unif <- end_time-start_time

start_time <- Sys.time()

simulation_normal <- gen_fit_all(n = c(20, 50, 100), dim = c (2, 4, 6, 8), 
                               distribution = "normal", tofit = "mle")
save(simulation_normal, file = "simulation_normal.RData")

end_time <- Sys.time()
time_diff_normal <- end_time-start_time

start_time <- Sys.time()

simulation_groups <- gen_fit_all(n = c(20, 50, 100), dim = c (2, 4, 6, 8), 
                                 distribution = "groups", n_groups = 3,
                                 tofit = "mle")
save(simulation_groups, file = "simulation_groups.RData")

end_time <- Sys.time()
time_diff_groups <- end_time-start_time

time_diff_unif
time_diff_normal
time_diff_groups

# load("simulation.RData") # load
# 
# comp_distances(simulation$`100_nodes_6_dim`$network, 
#                simulation$`100_nodes_6_dim`$models)
