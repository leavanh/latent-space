## Here I visualize the results

# load packages
library(RColorBrewer)

theme_set(theme_bw()) # set theme
c_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")

# make all Plots in a loop

for(n_nodes in 1:nlevels(unif_df$nodes)) {
  n <- as.integer(levels(unif_df$nodes)[[n_nodes]])
  subset_data <- subset(unif_df, nodes == n)
  print(ggplot(subset_data,
      aes(fit_dim, distance_diff, color = org_dim, group = org_dim)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c_palette) +
    ylim(c(0, 32)) +
    labs(title = paste(n, "nodes"),
         x = "Fitted dimension",
         y = "Difference of distances",
         color = "Original dimension")
  )
}
