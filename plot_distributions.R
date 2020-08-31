### Visualize what the distributions look like

theme_set(theme_bw(base_size = 15)) # set theme
set.seed(123) # set seed

## Unif

unif <- rsphere(200, distribution = "unif")
colnames(unif) <- c("x", "y")
ggplot(unif, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  ggtitle("Uniform distribution (n = 200)")
ggsave(file = "./Plots/Uniform.png", width = 200, height = 200, units = "mm")


## Normal

normal <- rsphere(200, distribution = "normal")
colnames(normal) <- c("x", "y")
ggplot(normal, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  ggtitle("Normal distribution (n = 200)")
ggsave(file = "./Plots/Normal.png", width = 200, height = 200, units = "mm")


## 2 Groups

groups_2 <- rsphere(200, distribution = "groups", n_groups = 2)
colnames(groups_2) <- c("x", "y")
ggplot(groups_2, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  ggtitle("2 normally distributed groups (n = 200)")
ggsave(file = "./Plots/2Groups.png", width = 200, height = 200, units = "mm")


## 3 Groups

groups_3 <- rsphere(200, distribution = "groups", n_groups = 3)
colnames(groups_3) <- c("x", "y")
ggplot(groups_3, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  ggtitle("3 normally distributed groups (n = 200)")
ggsave(file = "./Plots/3Groups.png", width = 200, height = 200, units = "mm")


## 4 Groups

groups_4 <- rsphere(200, distribution = "groups", n_groups = 4)
colnames(groups_4) <- c("x", "y")
ggplot(groups_4, aes(x, y)) + 
  geom_point(alpha = 0.5) + 
  scale_x_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75))+ 
  scale_y_continuous(breaks = c(-0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75),
                     limits = c(-0.75, 0.75)) +
  coord_fixed() +
  ggtitle("4 normally distributed groups (n = 200)")
ggsave(file = "./Plots/4Groups.png", width = 200, height = 200, units = "mm")
