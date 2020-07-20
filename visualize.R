## Here I visualize the results

theme_set(theme_bw()) # set theme
dist_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")
net_palette <- c("#ABD9E9", "#74ADD1", "#4575B4", "#313695")

# mean distance of differences

ggplot(results_mean_df,
       aes(fit_dim, mean_distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = dist_palette) +
  #ylim(c(0, 30)) +
  labs(title = "Results: Difference of distances",
       x = "Fitted dimension",
       y = "Mean difference of distances",
       color = "Original \ndimension")
ggsave(file = "Did_mean.pdf", width = 210, height = 297, units = "mm")

# sd difference of distances

ggplot(results_mean_df,
       aes(fit_dim, sd_distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = dist_palette) +
  #ylim(c(0, 30)) +
  labs(title = "Variance of the difference of distances",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension")
ggsave(file = "Did_sd.pdf", width = 210, height = 297, units = "mm")

# mean difference of networks

ggplot(results_mean_df,
       aes(fit_dim, mean_network_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = net_palette) +
  # ylim(c(0, 60)) +
  labs(title = "Results: Difference of networks",
       x = "Fitted dimension",
       y = "Mean difference",
       color = "Original \ndimension")
ggsave(file = "Din_mean.pdf", width = 210, height = 297, units = "mm")

# sd difference of networks

ggplot(results_mean_df,
       aes(fit_dim, sd_network_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = net_palette) +
  # ylim(c(0, 60)) +
  labs(title = "Variance of the difference of networks",
        x = "Fitted dimension",
        y = "Standard deviation",
        color = "Original \ndimension")
ggsave(file = "Din_sd.pdf", width = 210, height = 297, units = "mm")
