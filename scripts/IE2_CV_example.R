#loglik vs logdensities

#libraries
library(rvinecopulib)
library(os.pca)
library(dplyr)
set.seed(2739184)

#function data prep
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

### data load ###
discr <- c("age", "G1", "G2", "G3")
edu_orig <- read.csv("data/student-por.csv") %>%
  mutate(across(all_of(discr), ~ ordered(.x, levels = min(.x):max(.x)))) %>%
  mutate(across(school:health, ~ as.ordered(.x))) %>%
  select(-absences) %>%
  recode_education()

#levels determined for os pca
levels_ord <- rep("nominal", ncol(edu_orig))
levels_ord[names(edu_orig) %in% discr] <- "ordinal"


## split into 5 folds
splits <- split(1:nrow(edu_orig), rep(1:5, length.out = nrow(edu_orig)))

lls <- data.frame(ll_orig = numeric(5), ll_ord = numeric(5), improvement = numeric(5))
for (i in 1:5) {
  train_orig <- edu_orig[-splits[[i]], ]
  test_orig <- edu_orig[splits[[i]], ]

  # os pca
  output_ord <- os_pca(data = train_orig, level = levels_ord, ndim = 10, keep_data = TRUE)
  train_ord <- os_to_ordered(data = train_orig, os_object = output_ord)
  test_ord <- os_to_ordered(data = test_orig, os_object = output_ord)

  # vine models
  fit_orig <- vine(
    train_orig,
    copula_controls = list(family_set = c("indep", "onepar", "twopar"),
                           trunc_lvl = 10),
    margins_controls = list(mult = 0.5),
    cores = 10
  )
  fit_ord <- vine(
    train_ord,
    copula_controls = list(family_set = c("indep", "onepar", "twopar"),
                           trunc_lvl = 10),
    margins_controls = list(mult = 0.5),
    cores = 10
  )


  ll_orig <- sum(log(dvine(test_orig, fit_orig)))
  ll_ord <- sum(log(dvine(test_ord, fit_ord)))
  lls[i, ] <- c(ll_orig, ll_ord, ll_ord - ll_orig)
  print(lls[i, ])
}

lls
