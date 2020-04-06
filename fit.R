## load packages

library("latentnet")

## Model fitting
# net is the network

net_fit <- ergmm(net ~ euclidean(d = 2), # 2-dimensional
                 verbose = TRUE # print diagnostics
)

# plot

plot(net_fit,
     vertex.col = node_colors, # color nodes by gender
     vertex.cex = (age)/5, # size nodes by their age
     labels = TRUE, # show the node names
     )
