# Sanity check 2: More than 2 categories -> finding the best combination, by os vs brute force all possible combinations

library(mvtnorm)
library(rvinecopulib)
library(os.pca)
library(tidyverse)
library(gtools)
library(stringr)

#set covariance matrix
n_vars <- 2
parameters <- c(0.5)

sigma <- matrix(1, nrow = n_vars, ncol = n_vars)
sigma[lower.tri(sigma)] <- parameters
sigma[upper.tri(sigma)] <- parameters

#simulate gaussian distribution
sim_gauss <- rmvnorm(n = 1000, mean = rep(0, n_vars), sigma = sigma)

#create a discrete version of the normal distributed variables
sim_disc <- (sim_gauss)
for (j in 1:ncol(sim_disc)) {
  sim_disc[, j] <- as.ordered(cut(sim_gauss[, j], breaks = c(-Inf, -1, 0, 1, Inf)))
}

#test random orders
# OS with os.pca package -> retrieves true distribution
best_os <- os_pca(data = as.data.frame(sim_disc), level = c("nominal", "nominal"), ndim = 1, keep_data = TRUE)

#to rvinecopulib input
os_obj <- os_to_ordered(os_object = best_os)

#fit copula
fit_OS <- vine(data = os_obj, copula_controls = list(family_set = "gaussian"))

# look at all combinations of levels, fit copula to them
ncats <- 4
all_perms <- gtools::permutations(ncats, ncats)
dir1 <- apply(all_perms, 1, paste, collapse = "")
all_combs <- expand.grid(dir1, dir1)
get_split <- function(x){ 
  as.numeric(strsplit(as.character(x), split = "")[[1]])
}

sim_disc_combs <- as.data.frame(sim_disc)
for (i in 1:nrow(all_combs)) {
  sim_disc_combs[, 1] <- factor(sim_disc_combs[, 1], levels = get_split(all_combs[i, 1]))
  sim_disc_combs[, 2] <- factor(sim_disc_combs[, 2], levels = get_split(all_combs[i, 2]))
  vine_fit <- vine(sim_disc_combs, copula_controls = list(family = "gaussian"))
  all_combs$loglik[i] <- vine_fit$copula$loglik
  
  string_vine <- capture.output(vine_fit$copula) 
  all_combs$aic[i] <- word(string_vine[2], start = 18, end = 18, sep = fixed(" "))
}
all_combs$aic <- as.numeric(all_combs$aic)
final_result <- all_combs[!duplicated(round(all_combs$loglik), 2), ]


#try with 3 -> 1 numeric, 2 nominal
#set covariance matrix
n_vars <- 3
parameters <- c(0.5, 0.3, 0.7)
sigma <- matrix(1, nrow = n_vars, ncol = n_vars)
sigma[lower.tri(sigma)] <- parameters
sigma[upper.tri(sigma)] <- parameters

#simulate gaussian distribution
sim_gauss <- rmvnorm(n = 1000, mean = rep(0, n_vars), sigma = sigma)

#create a discrete version of the normal distributed variables
sim_both <- as.data.frame(sim_gauss)
for (j in 1:2) {
  sim_both[, j] <- as.ordered(cut(sim_gauss[, j], breaks = c(-Inf, -1, 0, 1, Inf)))
}

plot(sim_both[,2:3])

#### OS with os.pca package -> retrieves true distribution
library(os.pca)

input_os <- sim_both
input_os$V1 <- as.numeric(sim_both$V1)
input_os$V2 <- as.numeric(sim_both$V2)
best_os <- os_pca(data = as.data.frame(input_os), level = c("nominal", "nominal", "numerical"), ndim = 1, keep_data = TRUE)

order(best_os$os_catquants[1:4, ])
order(best_os$os_catquants[5:8, ])

#to rvinecopulib input
os_obj <- os_to_ordered(os_object = best_os)

fit_OS <- vine(data = os_obj, copula_controls = list(family_set = "gaussian"))
fit_OS$copula$loglik


#random order

sim_both$V1 <- factor(sim_both$V1, levels = levels(sim_both$V1)[c(3, 2, 4, 1)])


library(ggplot2)
source("scripts/functions/ggplot_functions.R")
ggplot(sim_both) +
  geom_boxplot(aes(x = V2, y = V3)) +
  theme_base()
  

