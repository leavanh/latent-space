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

results_list[[1]]$distribution <- "uniform"
results_list[[2]]$distribution <- "normal"
results_list[[3]]$distribution <- "2 groups"
results_list[[4]]$distribution <- "3 groups"
results_list[[5]]$distribution <- "4 groups"

results_df <- bind_rows(results_list) # as one df

# distribution as factor

results_df$distribution <- factor(results_df$distribution, 
                                  levels = c("uniform", "normal", "2 groups", 
                                             "3 groups", "4 groups"))

# nodes factor

results_df$nodes <- factor(results_df$nodes, 
                           levels = as.character(c(20, 50, 100, 200)))


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

# make a latex-table

results_mean_df %>%
  select(distribution, nodes, org_dim, fit_dim, mean_distance_diff_proc,
         sd_distance_diff_proc) %>%
  rename(original_dimension = "org_dim", fitted_dimension = "fit_dim",
         mean_DoD = "mean_distance_diff_proc", 
         sd_DoD = "sd_distance_diff_proc") %>%
  kbl(format = "latex", 
      booktabs = TRUE) %>%
    kable_styling(latex_options = "striped",
                  full_width = TRUE) %>%
  capture.output(file = "table.txt")

