#Marketing example: survey in San Francisco shopping mall with demographic information

#load all functions and libraries
library(rvinecopulib)
library(tidyverse)
library(patchwork)
library(os.pca)
source("scripts/functions/ggplot_functions.R")

#function for data cleaning
recode_marketing <- function(data, mark_cats) {
  for (column in colnames(data)) {
    data[, column] <- ordered(data[, column], labels = mark_cats[mark_cats$variable == column, "categorie"])
  }
  return(data)
}

set.seed(123)

#### Data ####
load("data/marketing.RData")
mark_cats <- read.csv("data/marketing_categories.csv", sep = ";")
mark_orig <- recode_marketing(marketing, mark_cats) %>% 
  drop_na() %>% 
  mutate(Householdu18 = fct_collapse(Householdu18, `Seven or more` = c("Seven", "Eight", "Nine or more")))

#### OS ####
output_nom <- os_pca(data = mark_orig, level = c(rep("nominal", ncol(mark_orig))), ndim = 10, homals_only = FALSE, keep_data = TRUE)
mark_os <- os_to_ordered(data = mark_orig, os_object = output_nom)

#### copula: fit, log-likelihood & simulate ####
start_time <- Sys.time()
cat(paste0("Start copula at ", start_time, "\n"))

#original ordering
fit_orig <- vine(mark_orig, cores = 12, copula_controls = list(family_set = "parametric"),
                 margins_controls = list(mult = 0.5))
cat(paste0("Finished original order at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

#os: nominal
fit_nom <- vine(mark_os, cores = 12, copula_controls = list(family_set = "parametric"),
                margins_controls = list(mult = 0.5))
cat(paste0("Finished OS at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

save(fit_orig, fit_nom,  file = "results/IE3_copulas.Rdata")

#results
load("results/IE3_copulas.Rdata")
cat(paste("Difference in fit (ell_ord - ell_org): ", fit_nom$loglik - fit_orig$loglik))

#simulation
#Simulation setting
B <- 20
n <- 500
set.seed(8769347)
sim_orig <- rvine(n*B, fit_orig) %>% 
  mutate(sim_nr = rep(1:B, each = n))
sim_nom <- rvine(n*B, fit_nom) %>% 
  mutate(sim_nr = rep(1:B, each = n))

save(sim_orig, sim_nom, file = "results/IE3_simulated_data.Rdata")

load("results/IE3_simulated_data.Rdata")

#### Summarize results ####

#### Figures ####
set.seed(123)
trans_plot_m <- plot_transformations(output_nom, order_facets = names(output_nom$data)[c(1:8, 10, 9, 11:14)]) +
  labs(tag = "a)") + theme(aspect.ratio = 1)
loading_plot_m <- plot_loadings(output_nom) + labs(tag = "b)")
obj_plot <- plot(fit_nom$copula, var_names = "use") + labs(tag = "c)", title = NULL, x = NULL, y = NULL) +
  theme_base() + theme(axis.text = element_blank(), axis.ticks = element_blank())

mf <- 1.4
pdf("results/figures/IE3_combined_plot.pdf", height = 10*mf, width = 8.3*mf)
trans_plot_m/(loading_plot_m + obj_plot)
dev.off()
