gen_net <- function(n = 100, # number of nodes
                    G = 3, # number of groups
                    G_perc = rep(1 / G, times = G), # vector % group 
                    G_dens = 0.1, # prob of edge in a group
                    betw_dens = 0.05, # prob of edge between groups
) {
  G_size <- as.vector(rmultinom(1, n, prob = G_perc)) # absolute group sizes
  
  # sociomatrix to be filled
  sociomatrix <- matrix(data = NA, nrow = n, ncol = n)
  
  # position is 1 in beginning
  position_start <- 1
  
  for(i in 1:G) {
    G_size_i <- G_size[i] # get size of group
    position_end <- position_start + G_size_i -1
    
    # get edges for in group
    G_edges_i <- sample(c(0, 1), G_size_i*G_size_i, replace = TRUE, 
                       prob = c((1 - G_dens), G_dens))
    
    # get edges for between groups
    betw_edges_i <- sample(c(0, 1), G_size_i*(n - G_size_i), replace = TRUE, 
                      prob = c((1 - betw_dens), betw_dens))
    
    # fill the sociomatrix
    sociomatrix[position_start:position_end,
                position_start:position_end] <- G_edges_i
    sociomatrix[position_start:position_end,
                -(position_start:position_end)] <- betw_edges_i
    
    # continue  with next group
    position_start <- position_end + 1
    i <- i+1
  }
}

