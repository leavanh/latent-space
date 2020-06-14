## Set up
# load packages

library(latentnet)
library(tidyverse)
library(mvtnorm)

# load functions

source("functions.R")


## generate networks

set.seed(09101999)

# make the networks
# 20, 50 and 100 nodes
# 2,4,6,8 dimensions
# unif, normal distribution and groups of 3,4,5
# use mle
# repeat 10 times each

rep <- 10

start_time <- Sys.time()

simulation_unif <- lapply(seq_len(rep), gen_fit_all, 
                          n = c(20, 50, 100), dim = c (2, 4, 6, 8),
                          distribution = "unif", tofit = "mle")
save(simulation_unif, file = "simulation_unif.RData") # save

end_time <- Sys.time()
time_diff_unif <- end_time-start_time

start_time <- Sys.time()

simulation_normal <- lapply(seq_len(rep), gen_fit_all, 
                            n = c(20, 50, 100), dim = c (2, 4, 6, 8),
                            distribution = "normal", tofit = "mle")
save(simulation_normal, file = "simulation_normal.RData")

end_time <- Sys.time()
time_diff_normal <- end_time-start_time

start_time <- Sys.time()

simulation_groups3 <- lapply(seq_len(rep), gen_fit_all,
                             n = c(20, 50, 100), dim = c (2, 4, 6, 8),
                             distribution = "groups", n_groups = 3,
                             tofit = "mle")
save(simulation_groups3, file = "simulation_groups3.RData")

end_time <- Sys.time()
time_diff_groups3 <- end_time-start_time

start_time <- Sys.time()

simulation_groups4 <- lapply(seq_len(rep), gen_fit_all, 
                             n = c(20, 50, 100), dim = c (2, 4, 6, 8),
                                 distribution = "groups", n_groups = 4,
                                 tofit = "mle")
save(simulation_groups4, file = "simulation_groups4.RData")

end_time <- Sys.time()
time_diff_groups4 <- end_time-start_time

start_time <- Sys.time()

simulation_groups5 <- lapply(seq_len(rep), gen_fit_all,
                             n = c(20, 50, 100), dim = c (2, 4, 6, 8),
                                 distribution = "groups", n_groups = 5,
                                 tofit = "mle")
save(simulation_groups5, file = "simulation_groups5.RData")

end_time <- Sys.time()
time_diff_groups5 <- end_time-start_time

time_diff_unif
time_diff_normal
time_diff_groups3
time_diff_groups4
time_diff_groups5

load("simulation_unif.RData") # load
load("simulation_normal.RData") # load
load("simulation_groups3.RData") # load
load("simulation_groups4.RData") # load
load("simulation_groups5.RData") # load

unif_df <- prod_df(simulation_unif, "unif")
normal_df <- prod_df(simulation_normal, "normal")
groups_df <- prod_df(simulation_groups, "groups")

results_df <- bind_rows(unif_df, normal_df, groups_df)

results_df$distribution <- factor(results_df$distribution, 
                                  levels = c("unif", "normal", "groups"))
