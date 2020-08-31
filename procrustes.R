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
colnames(P_hat) <- c("x_hat", "y_hat")
rownames(P_hat) <- 1:20

## Procrustes

procrustes(P, P_hat, scale = TRUE) %>%
  fitted -> P_hat_star
colnames(P_hat_star) <- c("x_hat_star", "y_hat_star")
rownames(P_hat_star) <- 1:20

### Visualize

# before

before <- rbind(P, P_hat)

ggplot(unif, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  labs(title = "Uniform distribution (n = 20)",
       subtitle = "Before Procrustes rotation",
       color = "Original \ndimension")
