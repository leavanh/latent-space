### Visualize how Procrustes works

theme_set(theme_bw()) # set theme
set.seed(123) # set seed

## Get original points

P <- rsphere(20, distribution = "unif")
colnames(P) <- c("x", "y")
rownames(P) <- 1:20
