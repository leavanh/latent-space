#### Generate networks ####

# parallelise

cl <- makeCluster(18)
registerDoParallel(cl)

# make the networks
# 20, 50, 100, 200 nodes
# 2,4,6,8 dimensions
# unif, normal distribution and groups of 2,3,4
# use mle
# repeat 5 times each

rep <- 5
nodes_vec <- c(20, 50, 100, 200)
dim_vec <- c(2, 4, 6, 8)


set.seed(09101999)

start_time <- Sys.time()

simulation_unif <- foreach(i = 1:rep,
                           .packages = c("latentnet", "tidyverse",
                                         "mvtnorm")) %dopar%
  gen_fit_all(n = nodes_vec, dim = dim_vec,
              distribution = "unif")

save(simulation_unif, file = "./simulations/simulation_unif.RData") # save

end_time <- Sys.time()
time_diff_unif <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_normal <- foreach(i = 1:rep,
                             .packages = c("latentnet", "tidyverse",
                                           "mvtnorm")) %dopar% gen_fit_all(
                                             n = nodes_vec, dim = dim_vec,
                                             distribution = "normal")
save(simulation_normal, file = "./simulations/simulation_normal.RData")

end_time <- Sys.time()
time_diff_normal <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups2 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                                              n = nodes_vec,
                                              dim = dim_vec,
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
  gen_fit_all(n = nodes_vec, 
              dim = dim_vec,
              distribution = "groups", n_groups = 3)
save(simulation_groups3, file = "./simulations/simulation_groups3.RData")

end_time <- Sys.time()
time_diff_groups3 <- end_time-start_time

set.seed(09101999)

start_time <- Sys.time()

simulation_groups4 <- foreach(i = 1:rep,
                              .packages = c("latentnet", "tidyverse",
                                            "mvtnorm")) %dopar% gen_fit_all(
                                              n = nodes_vec, dim = dim_vec,
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