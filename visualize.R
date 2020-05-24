## Here I visualize the results

theme_set(theme_bw()) # set theme
c_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")

# make all Plots in a loop

plots <- list()

for(i in 1:nlevels(unif_df$nodes)) {
  n <- as.integer(levels(unif_df$nodes)[[j]])
  subset_data <- subset(unif_df, nodes == n)
  plots[[i]] <- ggplot(unif_df,
                 aes(fit_dim, distance_diff, color = org_dim, group = org_dim)) +
    geom_point() +
    geom_line() +
    facet_grid(.~ nodes) +
    scale_color_manual(values = c_palette) +
    ylim(c(0, 32)) +
    labs(title = "Unif distribution",
         x = "Fitted dimension",
         y = "Difference of distances",
         color = "Original \ndimension")
}





