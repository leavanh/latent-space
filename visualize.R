## Here I visualize the results

theme_set(theme_bw()) # set theme
dist_palette <- c("#FEE090", "#FDAE61", "#F46D43", "#D73027")
time_palette <- c("#ABD9E9", "#74ADD1", "#4575B4", "#313695")

#### Distance of differences ####
# ------------------------------------------------------------------------------

## mean distance of differences

did_mean <- ggplot(results_mean_df,
                   aes(fit_dim, mean_distance_diff_scale, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Results: Difference of distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Mean difference of distances",
       color = "Original \ndimension")

did_mean  + facet_grid(nodes ~ distribution)

ggsave(file = "Did_mean.pdf", width = 210, height = 297, units = "mm")

did_mean + facet_grid(nodes ~ distribution, scales = "free_y")

ggsave(file = "Did_mean_free.pdf", width = 210, height = 297, units = "mm")


# sd difference of distances

did_sd <- ggplot(results_mean_df,
                 aes(fit_dim, sd_distance_diff, color = org_dim, group = org_dim)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = dist_palette) +
  labs(title = "Variance of the difference of distances",
       subtitle = "Euclidean distance between the true distances between 
       the nodes and the fitted distances",
       x = "Fitted dimension",
       y = "Standard deviation",
       color = "Original \ndimension")

did_sd + facet_grid(nodes ~ distribution)

ggsave(file = "Did_sd.pdf", width = 210, height = 297, units = "mm")

did_sd + facet_grid(nodes ~ distribution, scale = "free_y")

ggsave(file = "Did_sd_free.pdf", width = 210, height = 297, units = "mm")

# ------------------------------------------------------------------------------


# #### Distance of networks ####
# # ------------------------------------------------------------------------------
# 
# # mean distance of networks
# 
# din_eucl_mean <- ggplot(results_mean_df,
#                         aes(fit_dim, mean_network_diff_eucl, color = org_dim, group = org_dim)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = net_palette) +
#   labs(title = "Results: Difference of networks",
#        subtitle = "Euclidean distance between the true sociomatrix and the one
#        simulated from the fitted model",
#        x = "Fitted dimension",
#        y = "Mean distance",
#        color = "Original \ndimension")
# 
# din_eucl_mean  + facet_grid(nodes ~ distribution)
# 
# ggsave(file = "Din_eucl_mean.pdf", width = 210, height = 297, units = "mm")
# 
# din_eucl_mean + facet_grid(nodes ~ distribution, scales = "free_y")
# 
# ggsave(file = "Din_eucl_mean_free.pdf", width = 210, height = 297, units = "mm")
# 
# 
# # sd distance of networks
# 
# din_eucl_sd <- ggplot(results_mean_df,
#                       aes(fit_dim, sd_network_diff_eucl, color = org_dim, group = org_dim)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = net_palette) +
#   labs(title = "Variance of the Difference of networks",
#        subtitle = "Euclidean distance between the true sociomatrix and the one
#        simulated from the fitted model",
#        x = "Fitted dimension",
#        y = "Standard deviation",
#        color = "Original \ndimension")
# 
# din_eucl_sd + facet_grid(nodes ~ distribution)
# 
# ggsave(file = "Din_eucl_sd.pdf", width = 210, height = 297, units = "mm")
# 
# din_eucl_sd + facet_grid(nodes ~ distribution, scale = "free_y")
# 
# ggsave(file = "Din_eucl_sd_free.pdf", width = 210, height = 297, units = "mm")
# 
# 
# # mean percentage of same nodes
# 
# din_perc_mean <- ggplot(results_mean_df,
#                         aes(fit_dim, mean_network_diff_perc, color = org_dim, group = org_dim)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = net_palette) +
#   labs(title = "Results: Difference of networks",
#        subtitle = "Percentage of same nodes between the true sociomatrix and the one
#        simulated from the fitted model",
#        x = "Fitted dimension",
#        y = "Mean percentage",
#        color = "Original \ndimension")
# 
# din_perc_mean  + facet_grid(nodes ~ distribution)
# 
# ggsave(file = "Din_perc_mean.pdf", width = 210, height = 297, units = "mm")
# 
# din_perc_mean + facet_grid(nodes ~ distribution, scales = "free_y")
# 
# ggsave(file = "Din_perc_mean_free.pdf", width = 210, height = 297, units = "mm")
# 
# 
# # sd percentage of same nodes
# 
# din_perc_sd <- ggplot(results_mean_df,
#                       aes(fit_dim, sd_network_diff_perc, color = org_dim, group = org_dim)) +
#   geom_point() +
#   geom_line() +
#   scale_color_manual(values = net_palette) +
#   labs(title = "Variance of the Difference of networks",
#        subtitle = "Percentage of same nodes between the true sociomatrix and the one
#        simulated from the fitted model",
#        x = "Fitted dimension",
#        y = "Standard deviation",
#        color = "Original \ndimension")
# 
# din_perc_sd + facet_grid(nodes ~ distribution)
# 
# ggsave(file = "Din_perc_sd.pdf", width = 210, height = 297, units = "mm")
# 
# din_perc_sd + facet_grid(nodes ~ distribution, scale = "free_y")
# 
# ggsave(file = "Din_perc_sd_free.pdf", width = 210, height = 297, units = "mm")
# 
# # ------------------------------------------------------------------------------