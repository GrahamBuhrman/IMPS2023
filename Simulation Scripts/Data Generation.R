### Data Generation - TIMSS 2019 Data Simulation ###   

# Packages ----------------------------------------------------------------

require(dplyr)

# Data Generation Functions -----------------------------------------------

generate_data <- function(beta_vec = c(20, 1.5, 0.2, 0.01, 1.5, 0.2, -0.25, -5, 0, 1, 1.5, 2.5, 2, 0, -1, -1.5, -2, -5, 0, -1.75, -2, -3.5, -5), 
                          trt_ceiling = 1.50, RCT = FALSE, prop_PRIV = 0.05, sigma_2 = 0.9, tau_2 = 0.1, J = 100, testing = FALSE) {
  
  # load covariates
  dat <- load_covariates()
  
  # organize variables
  covs_cat_final <- c("STUSEX", "PRIV", "ABSENT", "FREPL", "PARENTED")
  covs_cont <- c("HOMERES", "BELONG", "LIKEMATH", "CONFMATH", "ACADSUC")
  covs_cat_gen <- c("SEX", "PRIVATE", "ABSENT_onceweek", "ABSENT_oncetwoweeks", 
                    "ABSENT_oncemonth", "ABSENT_oncetwomonths", "ABSENT_never",
                    "PARED_univ", "PARED_postsecondary", "PARED_uppersecondary", 
                    "PARED_lowersecondary", "PARED_someprimary", "FREPL_tenth",
                    "FREPL_quarter", "FREPL_half", "FREPL_threequarter", "FREPL_overthreequarter")
  
  # run some checks
  stopifnot(J <= length(unique(dat$IDSCH)))
  stopifnot(floor(J * prop_PRIV) <= 14)
  
  # sample clusters
  tab <- subset(data.frame(table(dat$IDSCH, dat$PRIVATE)), Freq != 0) 
  tab <- tab[, c("Var1", "Var2")]
  rownames(tab) <- NULL
  priv_schools <- subset(tab, Var2 == 1)$Var1
  pub_schools <- subset(tab, Var2 == 0)$Var1
  J_priv <- floor(J * prop_PRIV)
  J_pub <-  J - J_priv
  priv_sample <- sample(priv_schools, size = J_priv, replace = FALSE)
  pub_sample <- sample(pub_schools, size = J_pub, replace = FALSE)
  clusters <- c(priv_sample, pub_sample)
  dat_sampled <- subset(dat, IDSCH %in% clusters)
  
  # selection model
  if(isTRUE(RCT)) {
    
    dat_sampled$t <- rbinom(nrow(dat_sampled), 1, 0.5)
    
  } else {
    
    trt_assign_priv <- ifelse(dat_sampled$PRIVATE == 1, 1, 0)
    trt_assign_absent <- ifelse(dat_sampled$ABSENT_never == 1 | dat_sampled$ABSENT_oncetwomonths == 1, 1, 0)
    trt_assign_vec <- -1.80 + (1.10 * trt_assign_priv) + (1.50 * trt_assign_absent)
    g <- sapply(trt_assign_vec, plogis)
    dat_sampled$trt <- rbinom(n = nrow(dat_sampled), size = 1, prob = g)
    
  }
  
  # treatment heterogeneity model
  likemath <- dat_sampled$LIKEMATH
  
  dat_sampled$ITE <- (ifelse(likemath >= 0 & likemath < 9, (trt_ceiling / 9) * likemath,
                            ifelse(likemath >= 9 & likemath < 12, trt_ceiling,
                                   ifelse(likemath >= 12 & likemath < 14, (7 * trt_ceiling) - ((trt_ceiling / 2) * likemath), 0)))) + 0.5
  
  # error models
  C <- model.matrix(~factor(dat_sampled$IDSCH)-1)
  u <- rnorm(ncol(C), mean = 0, sd = sqrt(tau_2))
  dat_sampled$e <- rnorm(nrow(dat_sampled), mean = 0, sd = sqrt(sigma_2))
  dat_sampled$U <- as.numeric(C %*% u)
  
  # get covariate model matrix
  X <- dat_sampled[, c(covs_cont, covs_cat_gen)]
  N <- nrow(X)
  Xmat <- cbind(rep(1, N), data.matrix(X))
  
  # outcome model
  dat_sampled$y0 <- as.numeric((Xmat %*% beta_vec) + dat_sampled$U + dat_sampled$e)
  dat_sampled$y1 <- as.numeric(dat_sampled$y0 + dat_sampled$ITE)
  dat_sampled$Y <- ifelse(dat_sampled$trt == 1, dat_sampled$y1, dat_sampled$y0)
  
  # oracle values
  GCATE <- dat_sampled %>% group_by(IDSCH) %>% summarize(GCATE = mean(ITE)) %>% ungroup()
  dat_sampled <- dplyr::left_join(dat_sampled, GCATE, by = "IDSCH")

  # return final data
  testing_vars <- c("y0", "y1", "U", "e")
  
  if(isTRUE(testing)) {
    
    dat_final <- dat_sampled
    
  } else {
    
    dat_final <- dat_sampled[, !names(dat_sampled) %in% c(testing_vars, covs_cat_gen)]
    
  }
  
  return(dat_final)
  
}
