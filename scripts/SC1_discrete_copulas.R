##Extra simulations from reviewer feedback -> sanity check 1

### Discrete works to find back optimal ordering from Gaussian normal to discretized

#libraries
library(mvtnorm)
library(rvinecopulib)

set.seed(123)

#set covariance matrix
n_vars <- 3
parameters <- c(0.5, 0.3, 0.7)
sigma <- matrix(1, nrow = n_vars, ncol = n_vars)
sigma[lower.tri(sigma)] <- parameters
sigma[upper.tri(sigma)] <- parameters

#simulate gaussian distribution
sim_gauss <- rmvnorm(n = 1000, mean = rep(0, n_vars), sigma = sigma)

#create a discrete version of the normal distributed variables
sim_disc <- as.data.frame(sim_gauss)
for (j in 1:ncol(sim_disc)) {
  sim_disc[, j] <- as.ordered(cut(sim_gauss[, j], breaks = 7))
}
hist(as.numeric(sim_disc[, j]))

#fit vinecopula on discrete data and underlying gaussian data
vine_disc <- vine(sim_disc, copula_controls = list(family_set = "gaussian"))
vine_cont <- vine(sim_gauss, copula_controls = list(family_set = "gaussian"))

#compare the copulas (almost the same)
vine_disc$copula$pair_copulas
vine_cont$copula$pair_copulas
plot(vine_cont$copula, var_names = "use", edge_labels = "tau")
vine_cont$copula$names
plot(vine_disc$copula, var_names = "use", edge_labels = "tau")
vine_disc$copula$structure
vine_cont$copula$structure


#simulate data from copula
test_sim <- rvinecop(1000, vine_disc$copula)
test_disc <- rvine(1000, vine_disc)

#transform discrete to gaussian distribution
test_gaus <- test_sim
for (j in 1:ncol(test_gaus)) {
  test_gaus[, j] <- qnorm(test_sim[, j])
}

cov(test_gaus)[lower.tri(sigma)]
sigma[lower.tri(sigma)]

discr <- matrix(as.numeric(unlist(test_disc)), ncol = n_vars)
cov(discr)[lower.tri(sigma)]

discr_org <- matrix(as.numeric(unlist(sim_disc)), ncol = n_vars)
cov(discr_org)[lower.tri(sigma)]


