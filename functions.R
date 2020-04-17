### All the self-defined functions I use

## generate random points within a sphere --------------------------------------

rsphere <- function(
  n,  # number of points to generate
  r = 0.5,   # radius of the sphere
  dim = 3  # dimension in which to generate sphere
  ) 
{
  points <- as.data.frame(matrix(NA, nrow = 1, ncol = dim + 1))
  while(n < n + 1) {
   point <- runif(dim, min = -0.5, max = 0.5) # point within cube
   dist <- norm(point, type = "2") # distance to center
   if(dist <= r) { # only keep points in sphere
     points <- rbind(points, c(point, dist))
   }
  }
  points <- points[-1,] # delete first row (full of NAs)
  rownames(points) <- 1:n # rename rows
  colnames(points) <- c(1:dim, "distance") # rename columns
  return(points)
}

## -----------------------------------------------------------------------------

## generate a network out of the points ----------------------------------------

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
      tie_prob <- distance[i,j] # get prob for a tie
      tie <- rbernoulli(1, tie_prob) # generate a tie
      sociomatrix[i,j] <- tie # add to the sociomatrix
      j <- j + 1
    }
    i <- i + 1
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
 
  return(network)
}

## -----------------------------------------------------------------------------

