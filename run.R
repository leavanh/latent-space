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

start_time <- print(Sys.time())

simulation_unif <- gen_fit_all(n = c(10, 50, 100), dim = c (2, 4, 6), 
                               distribution = "unif", tofit = "mle")
simulation_normal <- gen_fit_all(n = c(10, 50, 100), dim = c (2, 4, 6), 
                               distribution = "normal", tofit = "mle")

save(simulation_unif, file = "simulation_unif.RData") # save
save(simulation_normal, file = "simulation_normal.RData")

end_time <- print(Sys.time())
print(end_time-start_time)

#load("simulation.RData") # load

#comp_distances(simulation$`100_nodes_6_dim`$network, 
#               simulation$`100_nodes_6_dim`$models)
