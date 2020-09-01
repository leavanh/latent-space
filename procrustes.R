### Visualize how Procrustes works

theme_set(theme_bw(base_size = 14)) # set theme
set.seed(3) # set seed

## Get original points

P <- rsphere(20, distribution = "unif")
colnames(P) <- c("x", "y")
rownames(P) <- 1:20

## Fit model

N <- gen_network(P)
M <- ergmm(N$network ~ euclidean(d = 2), tofit = "mle")
P_hat <- M$mle$Z
colnames(P_hat) <- c("x", "y")
rownames(P_hat) <- 1:20

## Procrustes

procrustes(P, P_hat, scale = TRUE) %>%
  fitted -> P_hat_star
colnames(P_hat_star) <- c("x", "y")
rownames(P_hat_star) <- 1:20

### Visualize

points <- rbind(cbind(type = "P", P), 
                cbind(type = "P_hat", P_hat),
                cbind(type = "P_hat_star", P_hat_star))
points$x <- as.numeric(points$x)
points$y <- as.numeric(points$y)
points$type <- factor(points$type)

ggplot(points) + 
  geom_point(aes(x, y, color = type), alpha = 0.5) +
  labs(title = "Visualization of Procrustes rotation",
       subtitle = "Uniform distribution (n = 20)",
       x = "x",
       y = "y") +
  scale_color_discrete(breaks = c("P", "P_hat", "P_hat_star"),
                      labels = c("true points", "fitted points", 
                                 "fitted points\nafter Procrustes rotation")) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

ggsave(file = "./Plots/Procrustes.png", width = 200, height = 150, units = "mm")

