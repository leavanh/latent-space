## All the self defined functions I use

# generate random points within a sphere

rsphere <- function(
  n,  # number of points to generate
  r = 0.5,   # radius of the sphere
  dim = 3  # dimension in which to generate sphere
  ) 
{
  points <- as.data.frame(matrix(NA, nrow = 1, ncol = dim + 1))
  while(nrow(points) < n + 1) {
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

