#Marketing example: survey in San Francisco shopping mall with demographic information

#load all functions and libraries
source("scripts/functions/functions_libraries.R")
set.seed(123)

#### Data ####
load("data/marketing.RData")
mark_cats <- read.csv("data/marketing_categories.csv", sep = ";")
mark_orig <- recode_marketing(marketing, mark_cats) %>% 
  drop_na() %>% 
  mutate(Householdu18 = fct_collapse(Householdu18, `Seven or more` = c("Seven", "Eight", "Nine or more")))

#### OS ####
output_nom <- os_pca(data = mark_orig, level = c(rep("nominal", ncol(mark_orig))), ndim = 2, homals_only = FALSE, keep_data = TRUE)
mark_os <- os_to_ordered(data = mark_orig, os_object = output_nom)$cat_data

ind_nom <- which(!names(mark_orig) %in% ("Sex"))
pdf(file = "results/figures/IE3_os_catquant.pdf", width = 10)
plot.os_catquants(output_nom, plot_vars = ind_nom, rcplot = c(3, 5))
dev.off()


#### copula: fit, log-likelihood & simulate ####
start_time <- Sys.time()
cat(paste0("Start copula at ", start_time, "\n"))

#original ordering
fit_orig <- vine(mark_orig, cores = 1,
                  copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished original order at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

#os: nominal
fit_nom <- vine(mark_os, cores = 1,
                copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished OS at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

save(fit_orig, fit_nom,  file = "results/IE3_copulas.Rdata")

load("results/IE3_copulas.Rdata")
fit_orig$loglik - fit_nom$loglik
AIC(fit_orig) - AIC(fit_nom)

#simulation
#Simulation setting
B <- 20
n <- 500
set.seed(8769347)
sim_orig <- rvine(n*B, fit_orig) %>% 
  mutate(sim_nr = rep(1:B, each = n))
sim_nom <- rvine(n*B, fit_nom) %>% 
  mutate(sim_nr = rep(1:B, each = n))



#### Summarize results ####
vars <- c("ethnic")
names(mark_orig)

plot.os_loadings(output_nom)


pdf(file = "results/figures/IE3_simulation_barplots.pdf", width = 7, height = 3)
#print(barplot_comparison(c("Occupation", "Household"), sim_nom %>% filter(sim_nr == 1), mark_orig))
print(barboxplot_comparison(c("Occupation", "Status"), sim_nom, mark_orig, sim_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), sim_nom, mark_orig, sim_orig))
print(barboxplot_comparison(c("Occupation", "Household"), sim_nom, mark_orig))
#print(barplot_comparison(c("Fedu", "Fjob"), sim_nom %>% filter(sim_nr == 1), mark_orig))
print(barboxplot_comparison(c("Income", "Ethnic"), sim_nom, mark_orig))
#print(barplot_comparison(c( "studytime", "sex"), sim_nom %>% filter(sim_nr == 1), mark_orig))
#print(barplot_comparison(c("Walc", "Dalc"), sim_nom %>% filter(sim_nr == 1), mark_orig))
dev.off()


#### Figures ####
trans_plot_m <- plot_transformations(output_nom, order_facets = names(output_nom$data)[c(1:8, 10, 9, 11:14)]) + labs(tag = "C")
loading_plot_m <- plot_loadings(output_nom) + labs(tag = "A") 
obj_plot <- plot(fit_nom$copula, var_names = "use", edge_labels = "tau") + labs(tag = "B", title = NULL, x = NULL, y = NULL) + 
  theme_base() + theme(axis.text = element_blank(), axis.ticks = element_blank())

mf <- 1.4
pdf("results/figures/IE3_combined_plot.pdf", height = 11.7*mf, width = 8.3*mf)
(loading_plot_m + obj_plot)/trans_plot_m
dev.off()


