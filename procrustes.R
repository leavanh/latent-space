### Visualize how Procrustes works

theme_set(theme_bw()) # set theme
set.seed(123) # set seed

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

points <- rbind(cbind(type = "true points", P), 
                cbind(type = "fitted points", P_hat),
                cbind(type = "fitted points after procrustes rotation", P_hat_star))
points$x <- as.numeric(points$x)
points$y <- as.numeric(points$y)
points$type <- as.factor(points$type)

ggplot(points) + 
  geom_point(aes(x, y, color = type), alpha = 0.5) +
  labs(title = "Visualization of the Procrustes rotation",
       subtitle = "Uniform distribution (n = 20)",
       x = "x",
       y = "y",
       color = "Points")

ggsave(file = "./Plots/Procrustes.png", width = 200, height = 200, units = "mm")

