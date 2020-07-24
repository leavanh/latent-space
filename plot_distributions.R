### Visualize what the distributions look like

theme_set(theme_bw()) # set theme
set.seed(09101999) # set seed

## Unif

unif <- rsphere(200, distribution = "unif")
colnames(unif) <- c("x", "y")
ggplot(unif, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed()


## Normal

normal <- rsphere(200, distribution = "normal")
colnames(normal) <- c("x", "y")
ggplot(normal, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed()


## 2 Groups

groups_2 <- rsphere(200, distribution = "groups", n_groups = 2)
colnames(groups_2) <- c("x", "y")
ggplot(groups_2, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed()


## 3 Groups

groups_3 <- rsphere(200, distribution = "groups", n_groups = 3)
colnames(groups_3) <- c("x", "y")
ggplot(groups_3, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed()


## 4 Groups

groups_4 <- rsphere(200, distribution = "groups", n_groups = 4)
colnames(groups_4) <- c("x", "y")
ggplot(groups_4, aes(x, y)) + 
  geom_point() + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed()
