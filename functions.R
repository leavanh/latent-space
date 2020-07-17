### All the self-defined functions I use

## rsphere()
# generate random points within a sphere ---------------------------------------

rsphere <- function(
  n,  # number of points to generate
  dim = 2,  # dimension in which to generate sphere
  distribution = "unif", # distribution to use
  sd = 1, # if distribution not unif whats the sd
  n_groups = 3, # if groups, how many?
  sd_group = 0.1*sd/n_groups # sd for each group
) 
{
  points_df <- as.data.frame(matrix(NA, nrow = 1, ncol = dim))
  
  # use the distribution
  if(distribution == "unif") { 
    while(nrow(points_df) < n + 1) {
      point <- runif(dim, min = -0.5, max = 0.5) # point within cube
      dist <- dist(rbind(point, rep(0, dim)), method = "euclidean") # distance to center
      if(dist <= 0.5) { # only keep points in sphere
        points_df <- rbind(points_df, point)
      }
    }
  } else if(distribution == "normal") {
      points <- rmvnorm(n, mean = rep(0, dim), sigma = sd*diag(dim))
      dist <- apply(points, MARGIN = 1, dist, rep(0, dim), 
                    method = "euclidean") # distance to center
      max_dist <- max(dist)
      points <- points/(max_dist*2) # scale the points, so the max dist is 1
      points_df <- rbind(points_df, points)
  } else if(distribution == "groups") {
      g_means <- as.data.frame(rmvnorm(n_groups, mean = rep(0, dim),
                                       sigma = sd*diag(dim))) # get groupmeans
      g_sizes <- as.vector(rmultinom(1, n, # get groupsizes
                                     prob = rep(n/(n_groups*100), times = n_groups))) 
      for(i in 1:n_groups) {
        mean <- unlist(g_means[i,]) # which mean?
        size <- g_sizes[i] # which size?
        points <- rmvnorm(size, mean = mean, sigma = sd_group*diag(dim))
        points_df <- rbind(points_df, points)
      }
      dist <- apply(points_df, MARGIN = 1, dist, rep(0, dim), 
                    method = "euclidean") # distance to center
      max_dist <- max(dist, na.rm = TRUE)
      points_df <- points_df/(max_dist*2) # scale the points, so the max dist is 1
  } else warning("Use a valid distribution")
  points_df <- points_df[-1,] # delete first row (full of NAs)
  rownames(points_df) <- 1:n # rename rows
  colnames(points_df) <- c(1:dim) # rename columns
  return(points_df)
}

## -----------------------------------------------------------------------------


## gen_network()
# generate a network out of the points -----------------------------------------

gen_network <- function(
  points,  # df of points
  directed = FALSE # should the ties be symmetric
) 
{
  n <- nrow(points) # get number of nodes
  
  # get the distances between all points
  distance <- as.matrix(dist(points, method = "euclidean"))
  
  gen_tie <- function(distance) {rbernoulli(1, 1 - distance)}
  
  sociomatrix <- apply(distance, c(1, 2), gen_tie) # generate the sociomatrix
  
  diag(sociomatrix) <- FALSE # diagonal has no ties
  
  # if we want undirected ties, we use only the lower triangle of the 
  # sociomatrix and 'flip' it
  if(directed == FALSE) {
    sociomatrix[lower.tri(sociomatrix)] <- 
      t(sociomatrix)[lower.tri(sociomatrix)]
  }
  
  network <- as.network(x = sociomatrix, # the network object
                        directed = directed, # is the network directed
                        loops = FALSE, # no self ties
                        vertex.attr = c(1:n), # name the nodges
                        matrix.type = "adjacency") # the type of input
  
  return(list(network = network,
              n = n,
              probabilities = 1 - distance,
              sociomatrix = sociomatrix,
              dimensions = length(points)
  ))
}

## -----------------------------------------------------------------------------

## fit models()
# fit latent models with same and less dim -------------------------------------

fit_models <- function(
  net_list # the network list gen_network returns
  )
{
  # retrieve all important information from net_list
  network <- net_list$network
  n <- net_list$n
  dim <- net_list$dimensions
  
  model_list <- vector(mode = "list", length = dim - 1) # empty list
  
  for(i in 2:dim) {
    start <- Sys.time()
    model <- ergmm(network ~ euclidean(d = i), tofit = "mle") # fit model
    end <- Sys.time()
    model_list[[i-1]] <- list(model = model, time = end-start)  # add to list
    names(model_list)[i-1] <- paste(i, "dim", "fit", sep = "_") # name
  }
  
  return(list(
    models = model_list,
    network = net_list
  ))
}

## -----------------------------------------------------------------------------

## gen_fit_all()
# generate and fit many models at the same time --------------------------------

gen_fit_all <- function(
  n, # vector of number of nodes
  dim, # vector of dimensions
  ...
) 
{
  # empty list models
  end_model_list <- vector(mode = "list", length = length(n)) 
  help_model_list <-  vector(mode = "list", length = length(dim)) 
  
  for(i in 1:length(n)) {
    for(j in 1:length(dim)) {
      points <- rsphere(n = n[i], dim = dim[j], ...)
      network <- gen_network(points)
      models <- fit_models(network)
      help_model_list[[j]] <- models # add to list
      names(help_model_list)[j] <- c(paste(n[i], "nodes", dim[j], "dim",
                                           sep = "_")) # name
    }
    end_model_list[[i]] <- help_model_list # add to list
    names(end_model_list)[i] <- c(paste(n[i], "nodes", sep = "_")) # name
  }
  return(end_model_list)
}

## -----------------------------------------------------------------------------

## comp_distance()
# get the difference between the real and the fitted distance -----------------

comp_distance <- function(
  network, # true network
  model, # model too compare with
  metric = "euclidean", # which distance metric to use
  standardize = FALSE # scale or standardize?
  )
{
  distance_network <- 1 - network$probabilities # true distances
  positions_model <- model$mle$Z
  distance_model <- as.matrix(dist(positions_model, method = metric)) 
  # fitted distances
  if(standardize == FALSE) {# scale
    distance_model_scale <- max(distance_network)*distance_model/max(distance_model)
    diff_matrix <- distance_model_scale - distance_network
  } else if(standardize == TRUE) {# standardize
    distance_model_stand <- (distance_model  -mean(distance_model))/
      sd(distance_model)
    distance_network_stand <- (distance_network - mean(distance_network))/
      sd(distance_network)
    diff_matrix <- distance_model_stand - distance_network_stand
  }
  if(metric == "euclidean") {                     
    difference <- sqrt(sum(diff_matrix*diff_matrix))
  } else if(metric == "manhattan") {
    difference <- sum(abs(diff_matrix))  
  } else warning("Choose different metric")
  return(difference)
}

## -----------------------------------------------------------------------------

## prod_df()
# organize the results in a clean way ------------------------------------------

prod_df <- function(
  simulation, # a list with true networks and the fitted models
  distribution, # you have to manually enter the distribution
  method = "euclidean", # comp networks with which method?
  ...
) {
  
  rep <- length(simulation) # how often repeated?
  
  df <- data.frame(matrix(vector(), 0, 6), stringsAsFactors = FALSE)
  
  for(i in 1:rep) {
    for(id_nodes in 1:length(simulation[[i]])) { # go through all diff nodes
      nodes <- str_extract(names(simulation[[i]][id_nodes]), pattern = "^\\d+")
      for(id_org_dim in 1:length(simulation[[i]][[id_nodes]])) { # all diff org dim
        org_dim <- str_extract(names(simulation[[i]][[id_nodes]][id_org_dim]),
                               pattern = "(?<=_)\\d+(?=_dim)")
        for(id_fit_dim in 
            1:length(simulation[[i]][[id_nodes]][[id_org_dim]]$models[])) { # fit dim
          a <- simulation[[i]][[id_nodes]][[id_org_dim]]$models[[id_fit_dim]]
          fit_dim <- str_extract(
            names(simulation[[i]][[id_nodes]][[id_org_dim]]$models[id_fit_dim]),
                              pattern = "^\\d+(?=_dim)")
          time <- a$time
          n <- simulation[[i]][[id_nodes]][[id_org_dim]]$network
          m <- a$model
          t <- n$network
          s <- a$sim_network
          distance_diff <- comp_distance(n, m, ...)
          network_diff <- comp_networks(t, s, method)
          
          # put row together and add to df
          df <- rbind(df, 
                      cbind(distribution, nodes, org_dim, 
                            fit_dim, time, distance_diff, network_diff))
        }
      }
    }
  }
  
  # make right type
  df <- transform(df, distribution = as.character(distribution),
                  nodes = as.factor(nodes), 
                  org_dim = as.factor(org_dim),
                  fit_dim = as.factor(fit_dim), 
                  time = as.numeric(levels(time))[time], 
                  distance_diff = as.numeric(levels(distance_diff))[distance_diff],
                  network_diff = as.numeric(levels(network_diff))[network_diff])
  
  # change nodes levels
  
  levels(df$nodes) <- as.character(sort(as.integer(levels(df$nodes))))
  
  return(df)
}


## -----------------------------------------------------------------------------

## sim_network()
# simulate network from the fitted models --------------------------------

sim_network <- function(
  simulation # a list with true networks and the fitted models
) {
  
  rep <- length(simulation) # how often repeated?
  for(i in 1:rep) {
    for(id_nodes in 1:length(simulation[[i]])) { # go through all diff nodes
      for(id_org_dim in 1:length(simulation[[i]][[id_nodes]])) { # all diff org dim
        for(id_fit_dim in 
            1:length(simulation[[i]][[id_nodes]][[id_org_dim]]$models[])) { # fit dim
          m <- simulation[[i]][[id_nodes]][[id_org_dim]]$models[[id_fit_dim]]$model
          sim_network <- simulate(m$model, seed = 09101999, par = m$mle)
          
          # add to the list
          sim_network -> simulation[[i]][[id_nodes]][[id_org_dim]]$models[[id_fit_dim]][["sim_network"]]
        }
      }
    }
  }
  
  return(simulation)
}

## -----------------------------------------------------------------------------

## comp_networks()
# compare the true network to the simulated network ----------------------------
comp_networks <- function(
  true_network, # the true underlying network
  sim_network, # the simulated network
  method = "euclidean" # with metric for comparing?
) {
  
  # get the sociomatrices
  t <- as.sociomatrix(true_network)
  s <- as.sociomatrix(sim_network)
  
  if(method == "euclidean") {
    
    metric <- sqrt(sum((s-t)*(s-t)))
    
  } else if(method == "percentage") {
  
    # compare the matrices
    d <- t == s # where are the differences?
    metric <- sum(d)/length(d) # higher -> better
  
  
  }
  
  return(metric)
}

## -----------------------------------------------------------------------------


