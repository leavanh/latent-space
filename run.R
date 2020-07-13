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


## generate networks

# parallelise

# cl <- makeCluster(38)
# registerDoParallel(cl)

# make the networks
# 20, 50 and 100 nodes
# 2,4,6,8 dimensions
# unif, normal distribution and groups of 2,3,4
# use mle
# repeat 10 times each

# rep <- 10
# 
# set.seed(09101999)
# 
# start_time <- Sys.time()
# 
# simulation_unif <- foreach(i = 1:rep,
#                            .packages = c("latentnet", "tidyverse",
#                            "mvtnorm")) %dopar%
#                     gen_fit_all(n = c(20, 50, 100), dim = c (2, 4, 6, 8),
#                           distribution = "unif")
# 
# save(simulation_unif, file = "simulation_unif.RData") # save
# 
# end_time <- Sys.time()
# time_diff_unif <- end_time-start_time
# 
# set.seed(09101999)
# 
# start_time <- Sys.time()
# 
# simulation_normal <- foreach(i = 1:rep,
#                              .packages = c("latentnet", "tidyverse",
#                                            "mvtnorm")) %dopar% gen_fit_all(
#                             n = c(20, 50, 100), dim = c (2, 4, 6, 8),
#                             distribution = "normal")
# save(simulation_normal, file = "simulation_normal.RData")
# 
# end_time <- Sys.time()
# time_diff_normal <- end_time-start_time
# 
# set.seed(09101999)
# 
# start_time <- Sys.time()
# 
# simulation_groups2 <- foreach(i = 1:rep,
#                               .packages = c("latentnet", "tidyverse",
#                                             "mvtnorm")) %dopar% gen_fit_all(
#                                               n = c(20, 50, 100), dim = c (2, 4, 6, 8),
#                                               distribution = "groups", n_groups = 2)
# save(simulation_groups2, file = "simulation_groups2.RData")
# 
# end_time <- Sys.time()
# time_diff_groups2 <- end_time-start_time
# 
# set.seed(09101999)
# 
# start_time <- Sys.time()
# 
# simulation_groups3 <- foreach(i = 1:rep,
#                               .packages = c("latentnet", "tidyverse",
#                                             "mvtnorm")) %dopar%
#                         gen_fit_all(n = c(20, 50, 100), dim = c (2, 4, 6, 8),
#                              distribution = "groups", n_groups = 3)
# save(simulation_groups3, file = "simulation_groups3.RData")
# 
# end_time <- Sys.time()
# time_diff_groups3 <- end_time-start_time
# 
# set.seed(09101999)
# 
# start_time <- Sys.time()
# 
# simulation_groups4 <- foreach(i = 1:rep,
#                               .packages = c("latentnet", "tidyverse",
#                                             "mvtnorm")) %dopar% gen_fit_all(
#                              n = c(20, 50, 100), dim = c (2, 4, 6, 8),
#                                  distribution = "groups", n_groups = 4)
# save(simulation_groups4, file = "simulation_groups4.RData")
# 
# end_time <- Sys.time()
# time_diff_groups4 <- end_time-start_time
# 
# time_diff_unif
# time_diff_normal
# time_diff_groups2
# time_diff_groups3
# time_diff_groups4
# 
# stopCluster(cl)

load("simulation_unif.RData") # load
load("simulation_normal.RData") # load
load("simulation_groups2.RData") # load
load("simulation_groups3.RData") # load
load("simulation_groups4.RData") # load

unif_df <- prod_df(simulation_unif, "unif", standardize = TRUE)
normal_df <- prod_df(simulation_normal, "normal", standardize = TRUE)
groups2_df <- prod_df(simulation_groups2, "2 groups", standardize = TRUE)
groups3_df <- prod_df(simulation_groups3, "3 groups", standardize = TRUE)
groups4_df <- prod_df(simulation_groups4, "4 groups", standardize = TRUE)


results_df <- bind_rows(unif_df, normal_df, groups2_df, groups3_df, groups4_df)

results_df$distribution <- factor(results_df$distribution, 
                                  levels = c("unif", "normal", "2 groups", "3 groups",
                                             "4 groups"))
results_mean_df <- results_df %>%
  group_by(distribution, nodes, org_dim, fit_dim) %>%
  summarise(mean_time = mean(time),
            mean_distance_diff = mean(distance_diff),
            sd_time = sd(time),
            sd_distance_diff = sd(distance_diff)) %>%
  ungroup()