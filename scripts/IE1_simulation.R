#Simulation example: categorical (3 cats) and continuous

#load all functions and libraries
source("scripts/functions/functions_libraries.R")

set.seed(123)

#### Data ####
n <- 500
test_data <- data.frame(x1_cat = sample(c(1, 2, 8), size = n, replace = TRUE)) %>% 
  mutate(x2 = (x1_cat %% 2) * 5 + as.numeric(!(x1_cat %% 2)) * (-10 + x1_cat) + rnorm(n)) %>% 
  mutate(x1_cat = ordered(x1_cat, labels = c("A", "B", "C")))

#### OS with os.pca package
output_nom <- os_pca(data = test_data, level = c("nominal", "numerical"), ndim = 1, keep_data = TRUE)

#to rvinecopulib input
test_os <- os_to_ordered(os_object = output_nom)

#### copula: fit, log-likelihood & simulate ####
#Unscaled
fit_orig <- vine(data = test_data, copula_controls = list(family_set = "parametric"))

#optimal ordering
fit_OS <- vine(data = test_os, copula_controls = list(family_set = "parametric"))

#dummy example
names_dummy <- paste("x1_cat", c("A", "B", "C"), sep = "_")
test_os_dummy <- fastDummies::dummy_cols(test_os, select_columns = "x1_cat") %>% 
  select(-x1_cat) %>% 
  mutate(across(all_of(names_dummy), ordered))
fit_dummy <- vine(data = test_os_dummy, copula_controls = list(family_set = "parametric"))

### simulation
sim_naive <- rvine(1000, fit_orig) %>% 
  mutate(type = "Unscaled: A<B<C", x1_cat = as.character(x1_cat))

sim_OS <- rvine(1000, fit_OS) %>% 
  mutate(type = "OS: A<C<B", x1_cat = as.character(x1_cat))

sim_dummy <- rvine(1000, fit_dummy) %>% 
  mutate(type = "Dummy")
names(sim_dummy) <- c(fit_dummy$names, "type")

sim_dummy %>% select(all_of(names_dummy)) %>% 
  mutate(across(all_of(names_dummy), function(x) {as.numeric(x) - 1})) %>% 
  rowSums() %>% table()/1000

sim_dummy_clean <- sim_dummy %>%
  mutate(across(all_of(names_dummy), function(x) {as.numeric(x) - 1})) %>% 
  mutate(nr_cats = rowSums(select(., all_of(names_dummy)))) %>% 
  mutate(id = 1:n()) %>% 
  pivot_longer(all_of(names_dummy), names_to = "x1_cat", names_prefix = "x1_cat_") %>% 
  group_by(id) %>% 
  mutate(value = ifelse(nr_cats == 0, sample(c(0, 0, 1)), value)) %>% 
  filter(value > 0) %>% 
  mutate(x1_cat = ifelse(nr_cats == 2, sample(x1_cat, 1), x1_cat)) %>% 
  distinct() %>% ungroup() %>% 
  select(x1_cat, x2, type)


#### Figures ####
total_sim <- test_data %>% 
  mutate(type = "Observed", x1_cat = as.character(x1_cat)) %>% 
  bind_rows(sim_naive) %>% 
  bind_rows(sim_OS) %>% 
  bind_rows(sim_dummy_clean)
log_liks <- data.frame(type = factor(c("Unscaled: A<B<C", "OS: A<C<B"),
                                     levels = c("Observed", "Unscaled: A<B<C", "OS: A<C<B")),
                       loglik = paste("\u2113 =", round(c(fit_orig$loglik, fit_OS$loglik))))

plot_all <- total_sim %>% 
  mutate(type = factor(type, levels = c("Observed", "Unscaled: A<B<C", "Dummy", "OS: A<C<B"))) %>% 
  ggplot(aes(x = x1_cat, y = x2)) +
  geom_boxplot() +
  geom_text(data = log_liks, aes(label = loglik, x = 2.7, y = 7.5)) +
  facet_grid(~type) +
  labs(x = "X1 (categorical)", y = "X2") +
  theme_base() +
  theme(aspect.ratio = 1)

plot_quants <- data.frame(quantification = unique(output_nom$os_data$x1_cat), 
                          category = unique(output_nom$data$x1_cat)) %>% 
  ggplot(aes(x = category, y = quantification, group = 1)) +
  geom_step() +
  geom_point() +
  labs(x = "original categories", y = "optimal quantifications") +
  scale_x_discrete(expand = expansion(mult = 0.05)) +
  theme_base() +
  theme(aspect.ratio = 1)

cairo_pdf("results/figures/IE1_quantifications.pdf", width = 9.5, height = 3)
print(plot_quants + labs(tag = "a)") + plot_all + labs(tag = "b)")  + 
        plot_layout(widths = c(1, 4)))
dev.off()

