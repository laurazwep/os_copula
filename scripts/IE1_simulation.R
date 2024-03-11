#Simulation example: categorical (3 cats) and continuous

#load all functions and libraries
source("scripts/functions_libraries.R")
set.seed(123)

#### Data ####
n <- 500
test_data <- data.frame(x1_cat = sample(c(1, 2, 8), size = n, replace = TRUE)) %>% 
  mutate(x2 = (x1_cat %% 2) * 5 + as.numeric(!(x1_cat %% 2)) * (-10 + x1_cat) + rnorm(n)) %>% 
  mutate(x1_cat = ordered(x1_cat, labels = c("A", "B", "C")))

#### OS (by hand (IE1:sim), SPSS or homals) ####
ie_data <- test_data %>% 
  mutate(naive = x1_cat) %>% 
  mutate(OS = ordered(as.character(x1_cat), levels = c("A", "C", "B"))) %>% 
  mutate(binned = ordered(ifelse(x1_cat == "B" | x1_cat == "C", "B&C", "A"), 
                                         levels = c("A", "B&C")))

#### copula: fit, log-likelihood & simulate ####
#naive
fit_naive <- vine(data = ie_data[, c("x2", "naive")], copula_controls = list(family_set = "parametric"))
ll_naive <- calculate_corrected_loglik(ie_data, cat_vars = "x1_cat", cat_trans_vars = "naive", loglik = fit_naive$loglik)

sim_naive <- rvine(1000, fit_naive) %>% 
  rename(x1_cat = "naive") %>% 
  mutate(type = "Naive: A<B<C", x1_cat = as.character(x1_cat))

#os
fit_OS <- vine(data = ie_data[, c("x2", "OS")], copula_controls = list(family_set = "parametric"))
ll_OS <- calculate_corrected_loglik(ie_data, cat_vars = "x1_cat", cat_trans_vars = "naive", loglik = fit_OS$loglik)

sim_OS <- rvine(1000, fit_OS) %>% 
  rename(x1_cat = "OS") %>% 
  mutate(type = "OS: A<C<B", x1_cat = as.character(x1_cat))

#binned
fit_binned <- vine(data = ie_data[, c("x2","binned")], copula_controls = list(family_set = "parametric"))
ll_binned <- calculate_corrected_loglik(ie_data, cat_vars = "x1_cat", cat_trans_vars = "binned", loglik = fit_binned$loglik)
pkjs <- attr(ll_binned, "pkj")
probs_BC <- c(pkjs$p[pkjs$category == "B"], pkjs$p[pkjs$category == "C"])

sim_binned <- rvine(1000, fit_binned) %>% 
  mutate(binned = ifelse(binned != "B&C", "A", 
                         sample(c("B", "C"), sum(binned == "B&C"), replace = TRUE, prob = probs_BC))) %>% 
  rename(x1_cat = "binned") %>% 
  mutate(type = "Binned: A<B=C", x1_cat = as.character(x1_cat))

#### Summarize results ####
log_liks <- data.frame(type = factor(c("Naive: A<B<C", "OS: A<C<B", "Binned: A<B=C"),
                                     levels = c("Observed", "Naive: A<B<C", "OS: A<C<B", "Binned: A<B=C")),
                       loglik = paste("\u2113 =", round(c(ll_naive, ll_OS, ll_binned))))

total_sim <- test_data %>% 
  mutate(type = "Observed", x1_cat = as.character(x1_cat)) %>% 
  bind_rows(sim_naive) %>% 
  bind_rows(sim_OS) %>% 
  bind_rows(sim_binned)

#### Figures ####
plot_all <- total_sim %>% 
  mutate(type = factor(type, levels = c("Observed", "Naive: A<B<C", "OS: A<C<B", "Binned: A<B=C"))) %>% 
  ggplot(aes(x = x1_cat, y = x2)) +
  geom_boxplot() +
  geom_text(data = log_liks, aes(label = loglik, x = 3, y = 7.5)) +
  facet_grid(~type) +
  labs(x = "X1 (categorical)", y = "X2") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

plot_all_color <- total_sim %>% 
  filter(type != "Observed") %>% 
  mutate(type = factor(type, levels = c("Observed", "Naive: A<B<C", "OS: A<C<B", "Binned: A<B=C"))) %>% 
  ggplot(aes(x = x1_cat, y = x2, fill = type)) +
  geom_boxplot(data = total_sim %>% filter(type == "Observed") ) +
  geom_boxplot() +
  facet_grid(~x1_cat, scales = "free_x") +
  labs(x = "X1 (categorical)", y = "X2") +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

cairo_pdf("results/figures/IE1_barplot_comparison.pdf", width = 8, height = 3)
print(plot_all)
dev.off()
