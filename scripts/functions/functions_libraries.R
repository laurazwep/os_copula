#libraries
library(rvinecopulib)
library(tidyverse)
library(patchwork)
library(os.pca)

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
  levels(data$traveltime) <- c("< 15 min.", "15 to 30 min.", "30 min to 1 hour", "> 1 hour")
  levels(data$famrel) <- c("very bad", "bad", "neutral", "good",  "excellent")
  levels(data$studytime) <-  c("< 2 hours", "2 to 5 hours",  "5 to 10 hours", "> 10 hours")
  levels(data$health) <- c("very bad", "bad","neutral", "good", "very good")
  
  return(data)
}

recode_marketing <- function(data, mark_cats) {
  for (column in colnames(data)) {
    data[, column] <- ordered(data[, column], labels = mark_cats[mark_cats$variable == column, "categorie"])
  }
  return(data)
}



