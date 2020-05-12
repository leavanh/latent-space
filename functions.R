### All the self-defined functions I use

## rsphere(n, dim, distribution, r)
# generate random points within a sphere ---------------------------------------

rsphere <- function(
  n,  # number of points to generate
  dim = 2,  # dimension in which to generate sphere
  distribution = "unif", # distribution to use
  r = 0.5   # radius of the sphere
) 
{
  points <- as.data.frame(matrix(NA, nrow = 1, ncol = dim + 1))
  while(nrow(points) < n + 1) {
    
    # use the distribution
    if(distribution == "unif") { 
      point <- runif(dim, min = -0.5, max = 0.5) # point within cube
      dist <- norm(point, type = "2") # distance to center
      if(dist <= r) { # only keep points in sphere
        points <- rbind(points, c(point, dist))
      }
    } else if(distribution == "normal") {
      point <- rnorm(dim, mean = 0, sd = 0.5)
      dist <- norm(point, type = "2") # distance to center
      if(dist <= r) { # only keep points in sphere
        points <- rbind(points, c(point, dist))
      }
    } else if(distribution == "exponential") { 
      point <- rexp(dim, rate = 4) # always positive -> shift center (falsche Lösung)
      dist <- dist(rbind( # distance to shifted center
        point,
        rep(r, times = dim))) # shifted center
      if(dist <= r) { # only keep points in sphere
        points <- rbind(points, c(point, dist))
      }
    } else warning("Use a valid distribution")
  }
  points <- points[-1,] # delete first row (full of NAs)
  rownames(points) <- 1:n # rename rows
  colnames(points) <- c(1:dim, "distance") # rename columns
  return(points)
}

## -----------------------------------------------------------------------------


## gen_network(points, directed)
# generate a network out of the points -----------------------------------------

gen_network <- function(
  points,  # df of points
  directed = FALSE # should the ties be symmetric
) 
{
  n <- nrow(points) # get number of nodes
  
  # get the distances between all points
  distance <- as.matrix(dist(subset(points, select = - c(distance))))
  
  # generate empty sociomatrix
  sociomatrix <- matrix(NA, nrow = nrow(points), ncol = nrow(points))
  
  for(i in 1:n) {
    for(j in 1:n) {
      tie_prob <- 1- distance[i,j] # get prob for a tie
      tie <- rbernoulli(1, tie_prob) # generate a tie
      sociomatrix[i,j] <- tie # add to the sociomatrix
    }
  }
  
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
              probabilities = distance,
              sociomatrix = sociomatrix,
              dimensions = length(points) - 1
  ))
}

## -----------------------------------------------------------------------------

## fit models()
# fit latent models with same and less dim -------------------------------------

# problem with burning in to be fixed

fit_models <- function(
  net_list, # the network list gen_network returns
  ...
  )
{
  # retrieve all important information from net_list
  network <- net_list$network
  n <- net_list$n
  dim <- net_list$dimensions
  
  model_list <- vector(mode = "list", length = dim - 1) # empty list
  
  for(i in 2:dim) {
    model <- ergmm(network ~ euclidean(d = i), ...) # fit model
    model_list[[i-1]] <- model # add to list
    names(model_list)[i-1] <- paste(i, "dim", "fit", sep = "_") # name
    i <- i + 1
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
  dim, # up to what dimension
  ...
) 
{
  model_list <- vector(mode = "list", length = length(n)) # empty list models
  
  for(i in 1:length(n)) {
    points <- rsphere(n = n[i], dim = dim, ...)
    network <- gen_network(points)
    models <- fit_models(network, ...)
    model_list[[i]] <- models # add to list
    names(model_list)[i] <- c(paste(n[i], "nodes", dim, "dim", sep = "_")) # name
    i = i + 1
  }
  
  return(model_list)
}

## -----------------------------------------------------------------------------

## comp_distances()
# get the difference between the real and the fitted distances -----------------

comp_distances <- function(
  network, # true network
  models, # models too compare with (a list)
  mle = TRUE # fitted using mle?
  )
{
  n_models <- length(models)
  difference_list <- vector(mode = "list", length = n_models) # empty list
  distance_network <- network$probabilities # true distances
  for(i in 1:n_models) {
    if(mle == TRUE) {
    positions_model <- models[[i]]$mle$Z
    } else warning("Have you fitted with mle?")
  distance_model <- as.matrix(dist(positions_model)) # fitted distances
  distance_model_s <- distance_model/max(distance_model) # scale
                      
  diff_matrix <- distance_model_s - distance_network
  difference <- sqrt(
    sum(
      diff_matrix*diff_matrix
      )
  )
  difference_list[[i]] <- difference # add to list
  names(difference_list)[i] <- c(
    paste(names(models)[i], "difference", sep = "_")) # name
  i = i + 1
  }
  return(difference_list)
}

## -----------------------------------------------------------------------------
