## Set up
# load packages

library(latentnet)
library(tidyverse)
library(mvtnorm)
library(doRNG)
library(parallel)
library(doParallel)

# load functions

source("functions.R")


#### Generate networks ####

# parallelise

cl <- makeCluster(38)
registerDoParallel(cl)

# make the networks
# 20, 50, 100, 200 nodes
# 2,4,6,8 dimensions
# unif, normal distribution and groups of 2,3,4
# use mle
# repeat 10 times each

rep <- 10

set.seed(09101999)

start_time <- Sys.time()

simulation_unif <- foreach(i = 1:rep,
                           .packages = c("latentnet", "tidyverse",
                           "mvtnorm")) %dopar%
                    gen_fit_all(n = c(20, 50, 100, 200), dim = c (2, 4, 6, 8),
                          distribution = "unif")

save(simulation_unif, file = "simulation_unif.RData") # save

end_time <- Sys.time()
time_diff_unif <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_normal <- foreach(i = 1:rep,
                             .packages = c("latentnet", "tidyverse",
                                           "mvtnorm")) %dopar% gen_fit_all(
                            n = c(20, 50, 100, 200), dim = c (2, 4, 6, 8),
                            distribution = "normal")
save(simulation_normal, file = "simulation_normal.RData")

end_time <- Sys.time()
time_diff_normal <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups2 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                                              n = c(20, 50, 100, 200),
                                              dim = c (2, 4, 6, 8),
                                              distribution = "groups",
                                              n_groups = 2)
save(simulation_groups2, file = "simulation_groups2.RData")

end_time <- Sys.time()
time_diff_groups2 <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups3 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar%
                        gen_fit_all(n = c(20, 50, 100, 200), 
                                    dim = c (2, 4, 6, 8),
                             distribution = "groups", n_groups = 3)
save(simulation_groups3, file = "simulation_groups3.RData")

end_time <- Sys.time()
time_diff_groups3 <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups4 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                             n = c(20, 50, 100, 200), dim = c (2, 4, 6, 8),
                                 distribution = "groups", n_groups = 4)
save(simulation_groups4, file = "simulation_groups4.RData")

end_time <- Sys.time()
time_diff_groups4 <- end_time-start_time

time_diff_unif
time_diff_normal
time_diff_groups2
time_diff_groups3
time_diff_groups4

stopCluster(cl)

#### Compare the results ####

load("simulation_unif.RData") # load
load("simulation_normal.RData") # load
load("simulation_groups2.RData") # load
load("simulation_groups3.RData") # load
load("simulation_groups4.RData") # load

## simulate a network for each fitted model

simulation_unif_new <- sim_network(simulation_unif)
simulation_normal_new <- sim_network(simulation_normal)
simulation_groups2_new <- sim_network(simulation_groups2)
simulation_groups3_new <- sim_network(simulation_groups3)
simulation_groups4_new <- sim_network(simulation_groups4)

## compare and make a df

unif_df <- prod_df(simulation_unif_new, "unif", standardize = TRUE)
normal_df <- prod_df(simulation_normal_new, "normal", standardize = TRUE)
groups2_df <- prod_df(simulation_groups2_new, "2 groups", standardize = TRUE)
groups3_df <- prod_df(simulation_groups3_new, "3 groups", standardize = TRUE)
groups4_df <- prod_df(simulation_groups4_new, "4 groups", standardize = TRUE)


results_df <- bind_rows(unif_df, normal_df, groups2_df, groups3_df, groups4_df)

results_df$distribution <- factor(results_df$distribution, 
                                  levels = c("unif", "normal", "2 groups", 
                                             "3 groups", "4 groups"))
results_mean_df <- results_df %>%
  group_by(distribution, nodes, org_dim, fit_dim) %>%
  summarise(mean_time = mean(time),
            mean_distance_diff = mean(distance_diff),
            mean_network_diff = mean(network_diff),
            sd_time = sd(time),
            sd_distance_diff = sd(distance_diff),
            sd_network_diff = sd(network_diff)) %>%
  ungroup()

