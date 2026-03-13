# create a copula with certain tau

#of interest 
# -> different copula functions (bicop), 
# -> different vinecop dimensions (d)
# -> different number of breaks? (n_breaks)

cop <- bicop_dist("clayton", 90, 1.8)
d <- 3
n_breaks <- 7
set.seed(123)

create_d_dim_vinecop <- function(d, bicop) {
  pcs <- list()
  mat_layer <- numeric()
  for (m in 1:d) {
    index <- d - m + 1
    pcs[[m]] <- rep(list(bicop), index)
    mat_i <- rep(0, d)
    mat_i[1:(index)] <- rep(m, index)
    mat_layer <- c(mat_layer, mat_i)
  }
  pcs <- pcs[-1]
  mat <- matrix(mat_layer, d, d, byrow = TRUE)
  vc <- vinecop_dist(pcs, mat)
  return(vc)
}


#create a discrete version of the variables
discretize_simulation <- function(sim, n_breaks, random_order = FALSE) {
  sim_disc <- as.data.frame(sim)
  for (j in 1:ncol(sim_disc)) {
    sim_cut <- cut(sim[, j], breaks = n_breaks)
    if (random_order) {
      sim_disc[, j] <- as.numeric(forcats::fct_shuffle(sim_cut))
    } else {
      sim_disc[, j] <- as.numeric(sim_cut)
    }
  }
  return(sim_disc)
}
"forcats" %in% rownames(installed.packages())

system.file(package='ggplot2')

vc <- create_d_dim_vinecop(d, cop)
sample_u <- as.data.frame(rvinecop(n = 1000, vc))
sample_disc <- discretize_simulation(sample_u, n_breaks, TRUE)


ndim <- ifelse(d > 2, 2, 1)
best_os <- os_pca(as.data.frame(sim_disc), rep("nominal", d), ndim, keep_data = TRUE)

best_os_numeric <- apply(os_to_ordered(best_os), 2, as.numeric)


diag(cor(best_os_numeric, sim_disc))

plot(best_os_numeric[, 1], sim_disc[, 1])



to_gaussian <- function(sim_u) {
  sim_gauss <- sim_u
  for (j in 1:ncol(sim_u)) {
    sim_gauss[, j] <- qnorm(sample1[, j]) 
  }
  return(sim_gauss)
}

sample_gaus <- to_gaussian(sample1)