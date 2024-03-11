#libraries
library(rvinecopulib)
library(tidyverse)
library(patchwork)

#functions
calculate_corrected_loglik <- function(dat, cat_vars, cat_trans_vars, loglik = 0) {
  
  pjk <- var_names <- NULL
  for (j in 1:length(cat_vars)) {
    x_name <- cat_vars[j]
    Tx_name <- cat_trans_vars[j]
    category <- sort(unique(dat[[x_name]]))
    for (k in 1:length(category)) {
      pjk <- c(pjk, sum(dat[[x_name]] == category[k])/sum(grepl(category[k], dat[[Tx_name]])))
    }
    var_names <- c(var_names, paste0(x_name, ";", category))
  }
  columns <- gsub(";", "", var_names)
  dummy_form <- as.formula(paste("~", paste(cat_vars, collapse = " + "), "-1"))
  dummy_vars <- model.matrix(dummy_form, data = dat, 
                             contrasts.arg = lapply(dat[, cat_vars, drop = F], 
                                                    contrasts, contrasts = FALSE))
  dummy_vars <- dummy_vars[, columns]
  correction <- sum(dummy_vars %*% as.matrix(log(pjk)))
  loglik_corrected <- loglik + correction
  
  names(pjk) <- columns
  pjk_data <- merge(data.frame(p = pjk, variable = gsub(";.*", "", var_names),
                               category = gsub(".+;", "", var_names)),
                    data.frame(T_variable = cat_trans_vars, variable = cat_vars),
                    by = "variable", all.x = TRUE)
  class(loglik_corrected) <- "loglik_corr"
  attr(loglik_corrected, which = "pkj") <- pjk_data
  
  return(loglik_corrected)
}
print.loglik_corr <- function(x) {
  print(c(x))
}


sample_marginal <- function(x_os, pk) {
  k <- as.character(pk$os) == as.character(x_os)
  x_assigned <- sample(pk$category[k], 1, prob = pk$p[k])
  return(x_assigned)
}


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
  levels(data$traveltime) <- c("< 15 min.", "15 to 30 min.", "30 min. to 1 hour", "> 1 hour")
  levels(data$famrel) <- c("very bad", "bad", "neutral", "good",  "excellent")
  levels(data$studytime) <-  c("< 2 hours", "2 to 5 hours",  "5 to 10 hours", "> 10 hours")
  levels(data$health) <- c("very bad", "bad","neutral", "good", "very good")
  
  return(data)
}


barboxplot_comparison <- function(variables, data_os, data_orig) {
  all_combs <- crossing(unique(data_os[[variables[1]]]), unique(data_os[[variables[2]]]))
  colnames(all_combs) <- variables
  my_label <- function(x) {
    names(x)[[1]] <- stringr::str_wrap(gsub("_", " ", variables[[2]]), 10)
    x[[1]] <- stringr::str_wrap(gsub("_", " ", x[[1]]), 10)
    label_both(x, sep = "\n")
  }
  
  frequencies_os <- data_os %>% 
    select(all_of(variables), sim_nr) %>% 
    group_by(across(all_of(variables)), sim_nr) %>% 
    summarize(count = n()) %>% ungroup() %>% 
    group_by(sim_nr) %>% full_join(all_combs) %>% 
    mutate(count = ifelse(is.na(count), 0, count)) %>% 
    mutate(freq = count/sum(count)) %>% ungroup()
  
  frequencies_orig <- data_orig %>% 
    select(all_of(variables)) %>% 
    group_by(across(all_of(variables))) %>% 
    summarize(count = n()) %>% ungroup() %>% 
    mutate(freq = count/sum(count)) %>% ungroup()
  
  bar_box_plots <- frequencies_orig %>% 
    ggplot(aes(x = .data[[variables[1]]], y = freq)) +
    geom_bar(stat = "identity", position  = "dodge", aes(fill = "#969696")) +
    geom_vline(xintercept = seq(0.5, 100, by = 1), color = "gray93", linewidth = 0.5) +
    geom_boxplot(data = frequencies_os, alpha = 0.7, aes(fill = "#3ABAC1"), show.legend = F) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    scale_fill_identity(name = "data", guide = 'legend',labels = c("simulated", "observed")) +
    facet_grid(~ .data[[variables[2]]], labeller = my_label) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_bw() + 
    theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  return(bar_box_plots)
}

barplot_comparison <- function(variables, data_os, data_orig) {
  all_combs <- crossing(unique(data_os[[variables[1]]]), unique(data_os[[variables[2]]]), c("observed", "simulated"))
  colnames(all_combs) <- c(variables, "data")
  
  suppressMessages(
    frequencies <- data_os %>% 
      mutate(data = "simulated") %>% 
      bind_rows(data_orig %>% mutate(sim_nr = NA, data = "observed")) %>% 
      select(all_of(variables), data, sim_nr) %>% 
      group_by(across(all_of(variables)), data, sim_nr) %>% 
      summarize(count = n()) %>% ungroup() %>% 
      group_by(data, sim_nr) %>% full_join(all_combs) %>% 
      mutate(count = ifelse(is.na(count), 0, count)) %>% 
      mutate(freq = count/sum(count)) %>% ungroup()
  )
  
  my_label <- function(x) {
    names(x)[[1]] <- stringr::str_wrap(gsub("_", " ", variables[[2]]), 10)
    x[[1]] <- stringr::str_wrap(gsub("_", " ", x[[1]]), 10)
    label_both(x, sep = "\n")
  }
  
  bar_plots <- frequencies %>% 
    mutate(label_freq = format(round(freq, 2), nsmall = 2)) %>% 
    ggplot(aes(x = .data[[variables[1]]], y = freq, fill = data)) +
    geom_bar(stat = "identity", position  = "dodge") +
    geom_vline(xintercept = seq(0.5, 100, by = 1), color = "gray93", linewidth = 0.5) +
    scale_fill_manual(values = c("#969696", "#3ABAC1")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    facet_grid(~ .data[[variables[2]]], labeller = my_label) +
    theme_bw() + theme(panel.grid.major.x = element_blank(), strip.background = element_rect(fill = "white"))
  return(bar_plots)
}