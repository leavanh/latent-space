## load packages

library("statnet")
library("latentnet")

# set seed for reproducibility

set.seed(09101999)

## specify the sociomatrix

n_nodes <- 10

# create a gender variable

gender <- c(rep("Female",n_nodes/2),rep("Male",n_nodes/2))

# create an age variable

age <- round(rnorm(n_nodes,20,3))

# create the matrix

edges <- sample(0:1, n_nodes*n_nodes, replace = TRUE, prob = c(0.5, 0.5))

my_sociomatrix <- matrix(edges,
                         nrow = n_nodes, # nrow must be same as ncol
                         ncol = n_nodes)

diag(my_sociomatrix) <- 0 # no self-edges

## creating a network object

net <- as.network(x = my_sociomatrix, # the network object
                  directed = TRUE, # specify whether the network is directed
                  loops = FALSE, # do we allow self ties (should not allow them)
                  matrix.type = "adjacency" # the type of input
)

# add 'names'

network.vertex.names(net) <- LETTERS[1:10]

# add attributes

set.vertex.attribute(net, # the name of the network object
                     "Gender", # the name we want to reference the variable by in that object
                     gender # the value we are giving that variable
)
set.vertex.attribute(net,"Age", age)

# take a look at the summary

summary.network(net, # the network we want to look at
                print.adj = FALSE # if TRUE then this will print out the whole adjacency matrix.
)

## visualize

# assign colors to genders

node_colors <- rep("",n_nodes)

for(i in 1:n_nodes){
  if(get.node.attr(net,"Gender")[i] == "Female"){
    node_colors[i] <- "maroon"
  }else{
    node_colors[i] <- "lightblue"
  }
}

# plot

plot.network(net, # our network object
             vertex.col = node_colors, # color nodes by gender
             vertex.cex = (age)/5, # size nodes by their age
             displaylabels = T, # show the node names
             label.pos = 5 # display the names directly over nodes
)

