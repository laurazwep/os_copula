#School example: Portuguese language grades in high school

#load all functions and libraries
source("scripts/functions_libraries.R")
set.seed(123)

#### Data ####
discr <- c("age", "failures", "G1", "G2", "G3")

#load data
# original data Kaggle
edu_orig <- read.csv("data/student-por.csv") %>% 
  mutate(across(all_of(discr), ~ ordered(.x, levels = min(.x):max(.x)))) %>% 
  mutate(across(school:health, ~ as.ordered(.x))) %>% 
  select(-absences)

#### OS (by hand (IE1:sim), SPSS or homals) ####
# Scaled data from CATPCA (SPSS) [REDO]
edu_os <- read.csv("results/EducationCat2.csv", header = TRUE)
names(edu_os) <- names(edu_orig)[c(1:26, 28:29, 30:32, 27)]

edu_os_clean <- edu_os %>%
  mutate(across(everything(), ~ as.ordered(.x)))

save(edu_orig, edu_os_clean, file = "data/clean/data_education_OS_naive.Rdata")

#### copula: fit, log-likelihood & simulate ####
#Simulation setting
B <- 20

start_time <- Sys.time()
cat(paste0("Start copula at ", start_time, "\n"))
#naive
fit_naive <- vine(edu_orig, cores = 1,
                   copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished agnostic at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))
ll_naive <- fit_naive$loglik

#simulation
sim_naive <- rvine(nrow(edu_orig)*B, fit_naive) %>% 
  mutate(sim_nr = rep(1:B, each = nrow(edu_orig)))

#os: nominal


#os: partly ordinal


#os: partly ordinal - binned
fit_binned <- vine(edu_os_clean, cores = 1,
                   copula_controls = list(family_set = "parametric"), margins_controls = list(mult = 1))
cat(paste0("Finished OS at ", Sys.time(), ", after ", round(Sys.time() - start_time, 2), " minutes \n"))

cat("save file:\n")
save(fit_naive, fit_binned, file = "results/illustrative_example.Rdata")
cat("finished")

#modify OS data to rename all categories
edu_os_named <- edu_os_clean
binned_cats <- NULL
full_trans_table <- NULL
for (column in colnames(edu_os_named)) {
  trans_table <- unique(data.frame(os = edu_os_named[, column], orig = edu_orig[, column])) %>% 
    arrange(os) %>% mutate(variable = column)
  if (nrow(trans_table) > length(levels(edu_os_named[, column]))) {
    binned_cats <- c(binned_cats, column)
    multiple <- as.character(trans_table$os[duplicated(trans_table$os)])
    ind_single <- !as.character(levels(edu_os_named[, column])) %in% multiple
    levels(edu_os_named[, column])[ind_single] <- as.character(trans_table$orig[!trans_table$os %in% multiple])
    for (doub in unique(multiple)) {
      ind_doub <- levels(edu_os_named[, column]) %in% doub
      levels(edu_os_named[, column])[ind_doub] <- paste(as.character(trans_table$orig[trans_table$os == doub]), collapse = "_")
    }
    edu_os_named[, paste0(column, "_org")] <- edu_orig[, column]
  } else {
    levels(edu_os_named[, column]) <- as.character(trans_table$orig)
  }
  full_trans_table <- rbind.data.frame(full_trans_table, trans_table)
}

ll_binned <- calculate_corrected_loglik(edu_os_named, cat_vars = paste0(binned_cats, "_org"), 
                                        cat_trans_vars = binned_cats, loglik = fit_binned$loglik)

#simulation
pkjs <- attr(ll_binned, "pkj") %>% 
  full_join(full_trans_table %>% rename(T_variable = variable, category = orig))

sim_os <- rvine(nrow(edu_orig)*B, fit_binned) %>% 
  mutate(sim_nr = rep(1:B, each = nrow(edu_orig)))
for (column in colnames(edu_orig)) {
  pk <- pkjs[pkjs$T_variable == column, ]
  if (all(!is.na(pk$variable))) {
    sim_os[, column] <- sapply(sim_os[, column], sample_marginal, pk = pk)
  } else {
    sim_os[, column] <- data.frame(os = sim_os[, column]) %>% 
      left_join(pkjs %>% mutate(os = as.character(os)), by = join_by(os)) %>% 
      select(category) %>% rename_with(~column, category)
  }
}


#### Summarize results ####
data_os <- recode_education(sim_os, to_factor = TRUE)
data_orig <- recode_education(edu_orig, to_factor = FALSE)
data_naive <-  recode_education(sim_naive, to_factor = TRUE)

dat_all <- data_orig %>%   
  mutate(data = "observed") %>% 
  bind_rows(data_os %>% mutate(data = "simulated OS")) %>% 
  bind_rows(data_naive %>%  mutate(data = "simulated naive")) %>% 
  filter(sim_nr == 1 |is.na(sim_nr))

log_liks <- data.frame(type = factor(c("Naive", "OS binned"),
                                     levels = c("Observed", "Naive", "OS nominal", "OS ordinal", "OS binned")),
                       loglik = paste("\u2113 =", round(c(ll_naive, ll_binned))))

#### Figures ####

pdf(file = "results/figures/IE2_simulation_barplots.pdf", width = 7, height = 3)
print(barplot_comparison(c("Medu", "Mjob"), data_os %>% filter(sim_nr == 1), data_orig))
print(barboxplot_comparison(c("Medu", "Mjob"), data_os, data_orig))
print(barplot_comparison(c("Fedu", "Fjob"), data_os %>% filter(sim_nr == 1), data_orig))
print(barboxplot_comparison(c("Fedu", "Fjob"), data_os, data_orig))
print(barplot_comparison(c( "studytime", "sex"), data_os %>% filter(sim_nr == 1), data_orig))
print(barplot_comparison(c("Walc", "Dalc"), data_os %>% filter(sim_nr == 1), data_orig))
dev.off()


plot_G <- dat_all %>% 
  mutate(across(G1:G3, ~ as.numeric(.x))) %>% 
  ggplot(aes(fill = data)) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_fill_manual(values = c("#969696", "#E4AB01", "#3ABAC1")) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  facet_grid(~ data) +
  theme_bw() + theme(strip.background = element_rect(fill = "white"))

plot_G1 <- plot_G +
  geom_boxplot(aes(x = Mjob, y = G1), show.legend = F)

plot_G2 <- plot_G +
  geom_boxplot(aes(x = Fjob, y = G2), show.legend = F)

plot_G3 <- plot_G +
  geom_boxplot(aes(x = famrel, y = G3), show.legend = F)

pdf("results/figures/IE2_simulation_boxplot_grades.pdf", width = 6, height = 8)
plot_G1/plot_G2/plot_G3
dev.off()