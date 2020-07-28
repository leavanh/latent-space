## Set up
# load packages

library(latentnet)
library(purrr)
library(tidyverse)
library(vegan)
library(mvtnorm)
library(doRNG)
library(parallel)
library(doParallel)

# load functions

source("functions.R")


#### Generate networks ####

# parallelise

cl <- makeCluster(18)
registerDoParallel(cl)

# make the networks
# 20, 50, 100, 200 nodes
# 2,4,6 dimensions
# unif, normal distribution and groups of 2,3,4
# use mle
# repeat 5 times each

rep <- 5

set.seed(09101999)

start_time <- Sys.time()

simulation_unif <- foreach(i = 1:rep,
                           .packages = c("latentnet", "tidyverse",
                           "mvtnorm")) %dopar%
                    gen_fit_all(n = c(20, 50, 100, 200), dim = c(2, 4, 6),
                          distribution = "unif")

save(simulation_unif, file = "./simulations/simulation_unif.RData") # save

end_time <- Sys.time()
time_diff_unif <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_normal <- foreach(i = 1:rep,
                             .packages = c("latentnet", "tidyverse",
                                           "mvtnorm")) %dopar% gen_fit_all(
                            n = c(20, 50, 100, 200), dim = c(2, 4, 6),
                            distribution = "normal")
save(simulation_normal, file = "./simulations/simulation_normal.RData")

end_time <- Sys.time()
time_diff_normal <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups2 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                                              n = c(20, 50, 100, 200),
                                              dim = c(2, 4, 6),
                                              distribution = "groups",
                                              n_groups = 2)
save(simulation_groups2, file = "./simulations/simulation_groups2.RData")

end_time <- Sys.time()
time_diff_groups2 <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups3 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar%
                        gen_fit_all(n = c(20, 50, 100, 200), 
                                    dim = c(2, 4, 6),
                             distribution = "groups", n_groups = 3)
save(simulation_groups3, file = "./simulations/simulation_groups3.RData")

end_time <- Sys.time()
time_diff_groups3 <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups4 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                             n = c(20, 50, 100, 200), dim = c(2, 4, 6),
                                 distribution = "groups", n_groups = 4)
save(simulation_groups4, file = "./simulations/simulation_groups4.RData")

end_time <- Sys.time()
time_diff_groups4 <- end_time-start_time

time_diff_unif
time_diff_normal
time_diff_groups2
time_diff_groups3
time_diff_groups4

stopCluster(cl)

#### Compare the results ####

load("./simulations/simulation_unif.RData") # load
load("./simulations/simulation_normal.RData") # load
load("./simulations/simulation_groups2.RData") # load
load("./simulations/simulation_groups3.RData") # load
load("./simulations/simulation_groups4.RData") # load

## compare and make a df

list(simulation_unif,
     simulation_normal,
     simulation_groups2,
     simulation_groups3,
     simulation_groups4) %>%
  lapply(prod_df, "change") -> results_list # list with all results

# change to the right distribution

results_list[[1]]$distribution <- "unif"
results_list[[2]]$distribution <- "normal"
results_list[[3]]$distribution <- "2 groups"
results_list[[4]]$distribution <- "3 groups"
results_list[[5]]$distribution <- "4 groups"

results_df <- bind_rows(results_list) # as one df

# distribution as factor

results_df$distribution <- factor(results_df$distribution, 
                                  levels = c("unif", "normal", "2 groups", 
                                             "3 groups", "4 groups"))

# noed as factor

results_df$nodes <- factor(results_df$nodes, 
                                  levels = c("20", "50", "100", "200"))


# get mean results

results_mean_df <- results_df %>%
  group_by(distribution, nodes, org_dim, fit_dim) %>%
  summarise(mean_time = mean(time),
            mean_distance_diff_scale = mean(distance_diff_scale),
            mean_distance_diff_stand = mean(distance_diff_stand),
            mean_distance_diff_proc = mean(distance_diff_proc),
            sd_time = sd(time),
            sd_distance_diff_scale = sd(distance_diff_scale),
            sd_distance_diff_stand = sd(distance_diff_stand),
            sd_distance_diff_proc = sd(distance_diff_proc)) %>%
  ungroup()
