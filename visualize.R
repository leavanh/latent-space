## Here I visualize the results

theme_set(theme_bw(base_size = 14)) # set theme
dist_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")
time_palette <- c("#ABD9E9", "#74ADD1", "#4575B4", "#313695")

#### Distance of differences ####
# ------------------------------------------------------------------------------

## mean distance of differences

ggplot(results_mean_df,
                   aes(fit_dim, mean_distance_diff_proc, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Difference of Distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Mean Difference of Distances",
       color = "Original \ndimension") +
  facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_mean.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_mean.png", width = 210, height = 280, units = "mm")

ggplot(results_mean_df,
       aes(fit_dim, mean_distance_diff_proc_scaled, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Difference of Distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances divided by the number of nodes",
       x = "Fitted dimension",
       y = "Mean Difference of Distances",
       color = "Original \ndimension") +
  facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_mean_free.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_mean_free.png", width = 210, height = 280, units = "mm")



# sd difference of distances

ggplot(results_mean_df,
                 aes(fit_dim, sd_distance_diff_proc, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Variance of the Difference of Distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension") +
  facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_sd.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_sd.png", width = 210, height = 280, units = "mm")

ggplot(results_mean_df,
       aes(fit_dim, sd_distance_diff_proc_scaled, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Variance of the Difference of Distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances divided by the number of nodes",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension") +
  facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_sd_free.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_sd_free.png", width = 210, height = 280, units = "mm")

# ------------------------------------------------------------------------------
