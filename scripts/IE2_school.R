#School example: Portuguese language grades in high school

#load all functions and libraries
library(rvinecopulib)
library(tidyverse)
library(patchwork)
library(os.pca)
source("scripts/functions/ggplot_functions.R")

#function for cleaning data
recode_education <- function(data, to_factor = FALSE) {
  if (to_factor) {
    for (vari in c("Fedu", "Medu", "freetime", "goout", "Dalc", "Walc", 
                   "traveltime", "famrel", "studytime", "health")) {
      data[, vari] <- as.ordered(data[, vari])
    }
  }
  for (vari in c("Fedu", "Medu")) {
    levels(data[, vari]) <- c("none",  "primary education \n(4th grade)", "primary education \n(5th to 9th grade)",
                              "secondary education", "higher education")
  }
  for (vari in c("freetime", "goout", "Dalc", "Walc")) {
    levels(data[, vari]) <- c("very low", "low", "medium", "high", "very high")
  }
  levels(data$traveltime) <- c("< 15 min.", "15 to 30 min.", "30 min to 1 hour", "> 1 hour")
  levels(data$famrel) <- c("very bad", "bad", "neutral", "good",  "excellent")
  levels(data$studytime) <-  c("< 2 hours", "2 to 5 hours",  "5 to 10 hours", "> 10 hours")
  levels(data$health) <- c("very bad", "bad","neutral", "good", "very good")
  
  return(data)
}

set.seed(97346592)

#### Data ####
discr <- c("age", "G1", "G2", "G3")
#load data
# original data Kaggle
edu_orig <- read.csv("data/student-por.csv") %>% 
  mutate(across(all_of(discr), ~ ordered(.x, levels = min(.x):max(.x)))) %>% 
  mutate(across(school:health, ~ as.ordered(.x))) %>% 
  select(-absences) %>% 
  recode_education()

not_discr <- setdiff(names(edu_orig), discr)

#### OS ####
#rank numeric (ordinal without ties)
levels_ord <- rep("nominal", ncol(edu_orig))
levels_ord[names(edu_orig) %in% discr] <- "ordinal"
output_ord <- os_pca(data = edu_orig, level = levels_ord, ndim = 10, keep_data = TRUE)
edu_ord <- os_to_ordered(data = edu_orig, os_object = output_ord)

#### copula: fit, log-likelihood & simulate ####
start_time <- Sys.time()
cat(paste0("Start copula at ", start_time, "\n"))
set.seed(123)
#original ordering, no os
fit_orig <- vine(edu_orig, cores = 4, 
                 copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished naive at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

start_time <- Sys.time()
#os: partly ordinal-> keep original values (treat ordinal as numeric)
fit_ord <- vine(edu_ord, cores = 4, 
                copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))

cat(paste0("Finished ord at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))
cat("save file:\n")
save(fit_orig, fit_ord, file = "results/IE2_copulas.Rdata")

cat("finished")

#Get results
load("results/IE2_copulas.Rdata")
cat(paste("Difference in fit (ell_ord - ell_org): ", fit_ord$loglik - fit_orig$loglik))

#simulations all
#Simulation setting
B <- 20
n <- nrow(edu_orig)
set.seed(8769347)
sim_orig <- rvine(n*B, fit_orig) %>% 
  mutate(sim_nr = rep(1:B, each = n))
sim_ord <- rvine(n*B, fit_ord) %>% 
  mutate(sim_nr = rep(1:B, each = n))

dat_all <- edu_orig %>%   
  mutate(data = "observed") %>% 
  bind_rows(sim_orig %>%  mutate(data = "simulated original order") %>%  mutate(across(-sim_nr, as.character))) %>% 
  bind_rows(sim_ord %>% mutate(data = "simulated OS") %>%  mutate(across(-sim_nr, as.character))) %>%
  filter(sim_nr == 1 |is.na(sim_nr))
dat_all$data <- factor(dat_all$data, levels = unique(dat_all$data))


#######
#plots
output_plot <- output_ord
rownames(output_plot$os_loadings)[9] <- "occupation women"
pdf("results/figures/OS1_example_transformation.pdf", height = 2.6, width = 8)
par(mfrow = c(1, 4))
plot.ordering(output_plot, plot_var = c(11))
plot.ordering(output_plot, plot_var = c(11), which_plot = 2)
plot.ordering(output_plot, plot_var = c(9))
plot.ordering(output_plot, plot_var = c(9), which_plot = 2)
par(mfrow = c(1, 1))
dev.off()

set.seed(1234)
ind_vars <-  c("Medu", "Fedu", "Mjob", "Fjob", "reason", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "Dalc", "Walc", "health")
trans_plot_m <- plot_transformations(output_ord, of_interest = ind_vars) + 
  labs(tag = "a)") + theme(aspect.ratio = 1)
loading_plot_m <- plot_loadings(output_ord, flip_x = TRUE) + labs(tag = "b)") 
obj_plot <- plot(fit_ord$copula, var_names = "use") + labs(tag = "c)", title = NULL, x = NULL, y = NULL) + 
  theme_base() + theme(axis.text = element_blank(), axis.ticks = element_blank())

mf <- 1.4
pdf("results/figures/IE2_combined_plot.pdf", height = 9*mf, width = 8.3*mf)
trans_plot_m/(loading_plot_m + obj_plot)
dev.off()

set.seed(123)
obj_plot_2 <- plot(fit_orig$copula, var_names = "use") + labs(title = NULL, x = NULL, y = NULL) + 
  theme_base() + theme(axis.text = element_blank(), axis.ticks = element_blank())
pdf("results/figures/IE2_vine_tree_orig.pdf", height = 4, width = 6)
print(obj_plot_2)
dev.off()