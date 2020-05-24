## Here I visualize the results

# load packages
library(RColorBrewer)
library(cowplot)

theme_set(theme_bw()) # set theme
c_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")

# make all Plots in a loop

plots <- list()

for(j in 1:nlevels(unif_df$nodes)) {
  n <- as.integer(levels(unif_df$nodes)[[j]])
  subset_data <- subset(unif_df, nodes == n)
  plot <- ggplot(subset_data,
      aes(fit_dim, distance_diff, color = org_dim, group = org_dim)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c_palette) +
    ylim(c(0, 32)) +
    labs(title = paste(n, "nodes"),
         x = "Fitted dimension",
         y = "Difference of distances",
         color = "Original \ndimension")
  legend <- get_legend(plot)
  plots[[j]] <- plot + 
    theme(legend.position = "none") +
    theme(plot.margin = margin(6, 0, 6, 0))
}

plots[[nlevels(unif_df$nodes) + 1]] <- legend

plot_grid(plotlist = plots, rel_widths = c(1, 1, 1, 0.4), nrow = 1)

