## Here I visualize the results

theme_set(theme_bw()) # set theme
dist_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")
time_palette <- c("#ABD9E9", "#74ADD1", "#4575B4", "#313695")

#### Distance of differences ####
# ------------------------------------------------------------------------------

## mean distance of differences

dod_mean <- ggplot(results_mean_df,
                   aes(fit_dim, mean_distance_diff_proc, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Results: Difference of distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Mean difference of distances",
       color = "Original \ndimension")

dod_mean  + facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_mean.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_mean.png", width = 210, height = 280, units = "mm")

dod_mean + facet_grid(nodes ~ distribution, scales = "free_y")

ggsave(file = "./Plots/Dod_mean_free.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_mean_free.png", width = 210, height = 280, units = "mm")



# sd difference of distances

dod_sd <- ggplot(results_mean_df,
                 aes(fit_dim, sd_distance_diff_proc, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Variance of the difference of distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension")

dod_sd + facet_grid(nodes ~ distribution)

ggsave(file = "./Plots/Dod_sd.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_sd.png", width = 210, height = 280, units = "mm")

dod_sd + facet_grid(nodes ~ distribution, scale = "free_y")

ggsave(file = "./Plots/Dod_sd_free.pdf", width = 210, height = 297, units = "mm")
ggsave(file = "./Plots/Dod_sd_free.png", width = 210, height = 280, units = "mm")

# ------------------------------------------------------------------------------
