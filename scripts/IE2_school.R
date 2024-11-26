#School example: Portuguese language grades in high school

#load all functions and libraries
source("scripts/functions/functions_libraries.R")
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

#### OS ####
#rank numeric (ordinal without ties)
levels_ord <- rep("nominal", ncol(edu_orig))
levels_ord[names(edu_orig) %in% discr] <- "numerical"
output_ord <- os_pca(data = edu_orig, level = levels_ord, ndim = 2, keep_data = TRUE)

#### copula: fit, log-likelihood & simulate ####
start_time <- Sys.time()
cat(paste0("Start copula at ", start_time, "\n"))
set.seed(8769347)
#original ordering, no os
fit_orig <- vine(edu_orig, cores = 1, 
                 copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished naive at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

#os: partly ordinal-> keep original values (treat ordinal as numeric)
edu_ord <- os_to_ordered(data = edu_orig, os_object = output_ord)
fit_ord <- vine(edu_ord$cat_data, cores = 1,
                copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))

cat("save file:\n")
save(fit_orig, fit_ord, file = "results/IE2_copulas.Rdata")
cat("finished")

#visualize
load("results/IE2_copulas.Rdata")
plot(fit_ord$copula, tree = 1, var_names = "use", edge_labels = "tau")
logliks <- data.frame(loglik = c(fit_orig$loglik, fit_ord$loglik), 
                      type =  c("simulated original", "simulated OS (nom)", "simulated OS (ord)"))
logliks$diff <- logliks$loglik - logliks$loglik[1]

fit_orig$loglik - fit_ord$loglik

#simulations all
#Simulation setting
B <- 20
set.seed(8769347)
sim_orig <- rvine(nrow(edu_orig)*B, fit_orig) %>% 
  mutate(sim_nr = rep(1:B, each = nrow(edu_orig)))
sim_ord <- rvine(nrow(edu_orig)*B, fit_ord) %>% 
  mutate(sim_nr = rep(1:B, each = nrow(edu_orig)))

dat_all <- edu_orig %>%   
  mutate(data = "observed") %>% 
  bind_rows(sim_orig %>%  mutate(data = "simulated original order") %>%  mutate(across(-sim_nr, as.character))) %>% 
  bind_rows(sim_ord %>% mutate(data = "simulated OS") %>%  mutate(across(-sim_nr, as.character))) %>%
  filter(sim_nr == 1 |is.na(sim_nr))
dat_all$data <- factor(dat_all$data, levels = unique(dat_all$data))


#######
#plots
ind_vars <-  c("Medu", "Fedu", "Mjob", "Fjob", "reason", "traveltime", "studytime", "failures", "schoolsup", "famsup", "famrel", "freetime", "goout", "Dalc", "Walc", "health")
ind_nom <- which(names(edu_orig) %in% ind_vars)

plot.os_loadings(output_ord, var_sel = 0.8)
pdf(file = "results/figures/IE2_os_catquant.pdf", width = 10, height = 4)
plot.os_catquants(output_ord, plot_vars = ind_nom, rcplot = c(2, 8))
dev.off()


print(barplot_comparison(c("Medu", "Mjob"), data_os = sim_ord %>% filter(sim_nr == 1), data_orig = edu_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), sim_nom, edu_orig, sim_orig))


######
plot.os_loadings(output_ord)
plot.os_loadings(output_nom)

plot.os_catquants(output_ord, plot_vars = which(output_ord$level == "numerical"), rcplot = c(2, 2))
plot.os_catquants(output_nom, plot_vars = which(output_ord$level == "numerical"), rcplot = c(2, 2))

pdf(file = "results/figures/IE2_os_catquant_both.pdf", width = 10)
plot.os_loadings(output_ord)
plot.os_loadings(output_nom)
plot.os_catquants(output_ord, plot_vars = c(3, 30, 31, 32, ind_nom), rcplot = c(3,6))
plot.os_catquants(output_nom, plot_vars = c(3, 30, 31, 32, ind_nom), rcplot = c(3,6))

dev.off()

###

#fit of vine copula
set.seed(123)
pdf(file = "results/figures/IE2_vine_tree_orig.pdf", height = 4)
plot(fit_orig$copula, var_names = "use")
dev.off()

set.seed(123)
pdf(file = "results/figures/IE2_vine_tree_os.pdf", height = 4)
plot(fit_ord$copula, var_names = "use", edge_labels = "tau")
dev.off()


set.seed(123)
pdf(file = "results/figures/IE2_vine_tree_os.pdf", height = 7, width = 10)
plot(fit_ord$copula, var_names = "use", edge_labels = "tau")
plot(fit_orig$copula, var_names = "use", edge_labels = "tau")
plot(fit_orig$copula, var_names = "use", edge_labels = "tau", tree = 2)
dev.off()



#TO-DO remove full nominal evaluation, keep with  age and grades as discrete variables. 
ind_vars <-  c("Medu", "Fedu", "Mjob", "Fjob", "reason", "traveltime", "studytime", "failures", "famrel", "freetime", "goout", "Dalc", "Walc", "health")
trans_plot_m <- plot_transformations(output_ord, of_interest = ind_vars) + labs(tag = "C")
loading_plot_m <- plot_loadings(output_ord) + labs(tag = "A") 
obj_plot <- plot(fit_ord$copula, var_names = "use", edge_labels = "tau") + labs(tag = "B", title = NULL, x = NULL, y = NULL) + 
  theme_base() + theme(axis.text = element_blank(), axis.ticks = element_blank())

mf <- 1.4
pdf("results/figures/IE2_combined_plot.pdf", height = 11.7*mf, width = 8.3*mf)
(loading_plot_m + obj_plot)/trans_plot_m
dev.off()




#### Figures ####

pdf(file = "results/figures/IE2_simulation_barplots2.pdf", width = 7, height = 3)
print(barplot_comparison(c("Medu", "Mjob"), sim_ord %>% filter(sim_nr == 1), edu_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), sim_ord, edu_orig, sim_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), sim_ord, edu_orig, sim_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), sim_orig, edu_orig))
print(barplot_comparison(c("Fedu", "Fjob"), sim_ord %>% filter(sim_nr == 1), edu_orig))
print(barboxplot_comparison(c("Fedu", "Fjob"), sim_ord, edu_orig))
print(barplot_comparison(c( "studytime", "sex"), sim_ord %>% filter(sim_nr == 1), edu_orig))
print(barplot_comparison(c("Walc", "Dalc"), sim_ord %>% filter(sim_nr == 1), edu_orig))
dev.off()

dat_G <- dat_all %>% 
  mutate(across(G1:G3, ~ as.numeric(.x)))

plot_G <- dat_G %>% 
  filter(data != "observed") %>% 
  ggplot(aes(fill = data)) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_fill_manual(values = c( "#E4AB01", "#3ABAC1", "white")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  #facet_grid(~ data) +
  theme_base() +
  theme(axis.text.x = element_blank()) + theme(panel.grid.major.y = element_line(color = "grey89"))

plot_G1 <- plot_G +
  geom_boxplot(aes(x = Mjob, y = G1), data = dat_G %>% filter(data != "observed"), fill = "grey89") +
  geom_boxplot(aes(x = Mjob, y = G1), show.legend = F, alpha = 0.4) + facet_grid(~ Mjob, scales = "free_x")

plot_G2 <- plot_G +
  geom_boxplot(aes(x = Fjob, y = G2), data = dat_G %>% filter(data != "observed"), fill = "grey89") +
  geom_boxplot(aes(x = Fjob, y = G2), alpha = 0.4) + facet_grid(~ Fjob, scales = "free_x")

plot_G3 <- plot_G +
  geom_boxplot(aes(x = famrel, y = G3), data = dat_G %>% filter(data != "observed"), fill = "grey89") +
  geom_boxplot(aes(x = famrel, y = G3), show.legend = F, alpha = 0.4) + facet_grid(~ famrel, scales = "free_x")

pdf("results/figures/IE2_simulation_boxplot_grades2.pdf", width = 9.5, height = 7)
plot_G1/plot_G2/plot_G3
dev.off()
