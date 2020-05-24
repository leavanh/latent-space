## Here I visualize the results

theme_set(theme_bw()) # set theme
diff_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")

ggplot(results_df,
       aes(fit_dim, distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  facet_grid(distribution ~ nodes) +
  scale_color_manual(values = diff_palette) +
  ylim(c(0, 32)) +
  labs(title = "Results: Difference of distances",
       x = "Fitted dimension",
       y = "Difference of distances",
       color = "Original \ndimension")

