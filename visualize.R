## Here I visualize the results

theme_set(theme_bw()) # set theme
diff_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")
time_palette <- c("#ABD9E9", "#74ADD1", "#4575B4", "#313695")

ggplot(results_mean_df,
       aes(fit_dim, mean_distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = diff_palette) +
  #ylim(c(0, 30)) +
  labs(title = "Results: Difference of distances",
       x = "Fitted dimension",
       y = "Mean difference of distances",
       color = "Original \ndimension")
ggsave(file="Did_mean.pdf", width = 210, height = 297, units = "mm")

ggplot(results_mean_df,
       aes(fit_dim, sd_distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = diff_palette) +
  #ylim(c(0, 30)) +
  labs(title = "Variance of the difference of distances",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension")
ggsave(file="Did_sd.pdf", width = 210, height = 297, units = "mm")

ggplot(results_mean_df,
       aes(fit_dim, mean_time, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = time_palette) +
  # ylim(c(0, 60)) +
  labs(title = "Results: Time to fit",
       x = "Fitted dimension",
       y = "Mean time (in seconds)",
       color = "Original \ndimension")

ggplot(results_mean_df,
       aes(fit_dim, sd_time, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = time_palette) +
  # ylim(c(0, 60)) +
  labs(title = "Variance of Time to fit",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension")
