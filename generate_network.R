gen_net <- function(n = 100, # number of nodes
                    G = 3, # number of groups
                    G_perc = rep(1 / G, times = G), # vector % group 
                    G_dens = 0.1, # prob of edge in a group
                    betw_dens = 0.05, # prob of edge between groups
) {
  G_size <- as.vector(rmultinom(1, n, prob = G_perc)) # absolute group sizes
  
  for(i in 1:G) {
    
  }
}

