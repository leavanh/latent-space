# Recovering Network Structure through Latent Space Models

## Abstract
The latent space model is based on the idea that actors exist in a social space. It fits a model to a network by assigning the actors positions. This thesis evaluates how well the model is able to recover the positions of the underlying network. To achieve this, a simulation study is conducted. Networks with varying underlying distributions, numbers of nodes and dimensions in space are simulated. Latent space models with different dimensions are fitted to them. Next, the distances between actors estimated by the model are compared to the true distances. It can be seen that the type of distribution and the number of nodes barely have an effect on how good the model recovers the network structure. What makes a difference is firstly the dimensions of the space the network was simulated in and secondly the difference between the true dimensions of the network and the ones used for the model. The model is able to best recover the network structure if the network and the model dimension are the same. It can also be observed that overfitting the model yields better results than underfitting.

## Repository Contents

The repository consists of the following files and folders:

Old: This folder contains previous versions of code or other files that are no longer in use. It is recommended to ignore this folder for the current project.
Plots: This folder is intended to store any plots or visualizations generated during the analysis.
simulations: This folder holds the simulation data used in the experiments.
.gitignore: This file specifies patterns of files or directories that should be ignored by Git when tracking changes.
README.md: The current file you are reading, which serves as a guide to the repository.
clean.R: This script contains code used to get compare the results in a clean way.
functions.R: This file contains custom functions or utility code that is used in the main analysis.
latent-space.Rproj: This is an RStudio project file, which helps organize the code and provides a consistent development environment.
plot_distributions.R: This script is responsible for visualizing the distributions.
procrustes.R: This file contains code for visualizing how procrustes works.
run.R: This script is the main entry point for running the analysis.
simulate.R: This file includes code for generating simulated network data based on specific latent space models or parameters.
visualize.R: This script is used to visualize the results.
