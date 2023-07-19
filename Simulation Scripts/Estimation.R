### Estimation- TIMSS 2019 Data Simulation ###  

# Packages ----------------------------------------------------------------

require(WeightIt)
require(dplyr)
require(lme4)
require(BART)
require(dbarts)
require(stan4bart)
require(bcf)
require(multibart)
require(arm)
require(caret)

# Helper Functions --------------------------------------------------------

# get propensity scores
analysis_propensity <- function(dat) {
  
  fit <- WeightIt::weightit(trt ~ STUSEX + ABSENT + PARENTED + FREPL + PRIV + HOMERES + BELONG + LIKEMATH + CONFMATH + ACADSUC,
                            data = dat,
                            method = "bart",
                            estimand = "ATE")
  
  propensity_scores <- fit$ps
  
  return(propensity_scores)
  
}

# BCF random intercepts and slopes function
random_intercepts_slopes <- function (groups, treatment, original_colnames = TRUE) {
  
  # get dummy codes
  dummies = multibart::get_dummies(groups, original_colnames = original_colnames)
  
  # format data
  random_des = cbind(dummies, dummies * treatment)
  Q = matrix(0, nrow = ncol(random_des), ncol = 2)
  Q[1:ncol(random_des)/2, 1] = 1
  Q[(1 + ncol(random_des)/2):nrow(Q), 2] = 1
  
  # get random intercepts
  intercept_ix = get_group_indices(dummies)
  
  # get random treatments
  treatment_ix = get_group_indices(dummies * treatment)
  
  # package and send out results
  return(list(randeff_design = random_des, randeff_variance_component_design = as.matrix(Q), 
              group_dummies = dummies, intercept_ix = intercept_ix, 
              treatment_ix = treatment_ix))
}

# BCF random intercepts function
random_intercepts <- function (groups, original_colnames = TRUE) {
  
  # get dummy codes
  dummies <- multibart::get_dummies(groups, original_colnames = original_colnames)
  
  # format data
  random_des <- cbind(dummies)
  Q <- matrix(1, nrow = ncol(random_des), ncol = 1)
  
  # get random intercepts
  intercept_ix = get_group_indices(dummies)
  
  # package and send out results
  return(list(randeff_design = random_des, randeff_variance_component_design = as.matrix(Q), 
              group_dummies = dummies, intercept_ix = intercept_ix))
}

# fit a treatment model
fit_treatment_model <- function(X, D, dat, trt_val, draws, warmup, thin) {
  
  BART::gbart(x.train = X, y.train = D[dat$trt == trt_val],
              rm.const = FALSE,
              ndpost = draws,
              nskip = warmup,
              keepevery = thin)
  
}

# fit an outcome model
fit_outcome_model <- function(X, Y, draws, warmup, thin) {
  
  BART::gbart(x.train = X, y.train = Y, 
              rm.const = FALSE,
              ndpost = draws,
              nskip = warmup,
              keepevery = thin)
  
}

# Methods Functions -------------------------------------------------------

# define multi-level model method
analysis_MLM <- function(dat, one_run = FALSE) {
  
  # get data values
  Y <- as.numeric(dat$Y)
  IDSCH <- dat$IDSCH
  
  # get fit
  fit <- lme4::lmer(formula = Y ~ trt + trt:LIKEMATH + STUSEX + ABSENT + PARENTED + FREPL + PRIV + HOMERES + BELONG + LIKEMATH + CONFMATH + ACADSUC + (1 | IDSCH),
                    data = dat)
  
  # get cate estimate and interval
  cate <- as.numeric(fixef(fit)[2])
  cate_se <- as.numeric(arm::se.fixef(fit)[2])
  cate_int <- c(cate - (1.96 * cate_se), cate + (1.96 * cate_se))
  
  # create new data for prediction
  t0 <- model.frame(fit)
  t0$t <- 0
  
  t1 <- model.frame(fit)
  t1$t <- 1
  
  # predict icate
  ite_pred <- predict(fit, newdata = t1) - predict(fit, newdata = t0)
  
  # get group specific predictions
  group_predictions <- 
    tidyr::tibble(ite_pred, IDSCH) %>%
    dplyr::group_by(IDSCH) %>%
    dplyr::summarize(gcate = mean(ite_pred))
  
  group_compare <- dplyr::left_join(dat, group_predictions, by = "IDSCH")
  group_diff <- as.numeric(group_compare$gcate) - as.numeric(group_compare$GCATE)
  
  # format results
  if (one_run) {
    
    results <-
      dat %>%
      mutate(ite_pred = ite_pred)
    
  } else {
    
    results <- tidyr::tibble(CATE_est = cate,
                             CATE_true = mean(dat$ITE),
                             CATE_LB = cate_int[1],
                             CATE_UB = cate_int[2],
                             PEHE = sqrt(mean((ite_pred - dat$ITE)^2)) / sd(Y),
                             PEGSTE = sqrt(mean((group_diff)^2)) / sd(Y))
    
  }
  
  # package and send out results
  return(results)
  
}

# define X-BART method
analysis_XBART <- function(dat, one_run = FALSE) {
  
  # get propensity scores
  propensity_scores <- dat$weights
  
  # get data objects
  Y <- as.numeric(dat$Y)
  X <- dat %>% dplyr::select(-c(Y, IDSTU, weights, ITE, GCATE))
  X_mat <- X %>% dplyr::select(-trt) %>% bartModelMatrix() %>% as.data.frame()
  X_control <- dplyr::filter(X, trt == 0) %>% dplyr::select(-trt) %>% bartModelMatrix() %>% as.data.frame()
  X_treated <- dplyr::filter(X, trt == 1) %>% dplyr::select(-trt) %>% bartModelMatrix() %>% as.data.frame()
  group <- dat$IDSCH
  
  Y_control <- as.numeric(unlist(dplyr::filter(dat, trt == 0) %>% dplyr::select(Y)))
  Y_treated <- as.numeric(unlist(dplyr::filter(dat, trt == 1) %>% dplyr::select(Y)))
  
  # fit first phase outcome models
  outcome_control <- fit_outcome_model(X = X_control, Y = Y_control, 
                                       draws = 1000, warmup = 1000, thin = 1)
  
  outcome_treated <- fit_outcome_model(X = X_treated, Y = Y_treated,
                                       draws = 1000, warmup = 1000, thin = 1)
  
  # get imputed treatment effects
  D <- ifelse(dat$trt == 0,
              as.numeric(colMeans(predict(object = outcome_treated, newdata = X_mat))) - Y,
              Y - as.numeric(colMeans(predict(object = outcome_control, newdata = X_mat))))
  
  
  # fit second phase treatment models
  treatment_control <- fit_treatment_model(X = X_control, D = D, trt_val = 0, dat = dat,
                                           draws = 1000, warmup = 1000, thin = 1)
  
  treatment_treated <- fit_treatment_model(X = X_treated, D = D, trt_val = 1, dat = dat,
                                           draws = 1000, warmup = 1000, thin = 1)
  
  # get predictions
  predict_control <- predict(object = treatment_control, newdata = X_mat)
  predict_treated <- predict(object = treatment_treated, newdata = X_mat)
  
  # get CATE estimates
  samples_ite <- sweep(predict_control, MARGIN = 2, propensity_scores, `*`) + sweep(predict_treated, MARGIN = 2, (1 - propensity_scores), `*`)
  samples_cate <- apply(samples_ite, 1, mean)
  cate <- mean(samples_cate)
  cate_int <- quantile(samples_cate, c(0.025, 0.975))
  
  # get ITE estimates 
  ite_pred <- apply(samples_ite, 2, mean)
  
  # get group predictions
  group_predictions <- unlist(lapply(split(ite_pred, group), mean)) %>% data.frame()
  group_predictions$IDSCH <- row.names(group_predictions)
  colnames(group_predictions) <- c("gcate", "IDSCH")
  group_compare <- dplyr::left_join(dat, group_predictions, by = "IDSCH")
  group_diff <- as.numeric(group_compare$gcate) - as.numeric(group_compare$GCATE)
  
  # format results
  if (one_run) {
    
    results <-
      dat %>%
      mutate(ite_pred = ite_pred)
    
  } else {
    
    results <- tidyr::tibble(CATE_est = cate,
                             CATE_true = mean(dat$ITE),
                             CATE_LB = cate_int[1],
                             CATE_UB = cate_int[2],
                             PEHE = sqrt(mean((ite_pred - dat$ITE)^2)) / sd(Y),
                             PEGSTE = sqrt(mean((group_diff)^2)) / sd(Y))
    
  }
  
  # package and send out results
  return(results)
  
}

# define stan4bart method
analysis_StanBART <- function(dat, one_run = FALSE) {
  
  # get data objects
  Y <- as.numeric(dat$Y)
  
  # fit stan4bart model to data
  fit <- stan4bart::stan4bart(formula = 
                                Y ~ bart(. - IDSTU - IDSCH - ITE - GCATE - weights) + 
                                (1 | IDSCH),
                              data = dat,
                              treatment = trt,
                              chains = 2,
                              cores = 1,
                              iter = 2000,
                              warmup = 1000,
                              skip = 1,
                              stan_args = list(verbose = FALSE),
                              bart_args = list(keepTrees = TRUE, 
                                               n.burn = 1000,
                                               n.thin = 1,
                                               n.trees = 200, 
                                               n.threads = 1, 
                                               n.chains = 1))
  
  # get estimates
  samples_mu_train <- dbarts::extract(fit)
  samples_mu_test <- dbarts::extract(fit, sample = "test")
  samples_icate <- (samples_mu_train - samples_mu_test) * (2 * fit$frame[[fit$treatment]] - 1)
  samples_cate <- apply(samples_icate, 2, mean)
  cate <- mean(samples_cate)
  cate_int <- quantile(samples_cate, c(0.025, 0.975))
  
  # get ITE estimates 
  ite_pred <- apply(samples_icate, 1, mean)
  
  # get group predictions
  group <- dat$IDSCH
  group_predictions <- unlist(lapply(split(ite_pred, group), mean)) %>% data.frame()
  group_predictions$IDSCH <- row.names(group_predictions)
  colnames(group_predictions) <- c("gcate", "IDSCH")
  group_compare <- dplyr::left_join(dat, group_predictions, by = "IDSCH")
  group_diff <- as.numeric(group_compare$gcate) - as.numeric(group_compare$GCATE)
  
  # format results
  if (one_run) {
    
    results <-
      dat %>%
      mutate(ite_pred = ite_pred)
    
  } else {
    
    results <- tidyr::tibble(CATE_est = cate,
                             CATE_true = mean(dat$ITE),
                             CATE_LB = cate_int[1],
                             CATE_UB = cate_int[2],
                             PEHE = sqrt(mean((ite_pred - dat$ITE)^2)) / sd(Y),
                             PEGSTE = sqrt(mean((group_diff)^2)) / sd(Y))
    
  }
  
  # package and send out results
  return(results)
  
}

# define BCF method
analysis_BCF <- function(dat, one_run = FALSE) {
  
  # create data objects
  X_control <- dat %>% 
    dplyr::select(-c(Y, IDSTU, trt, weights, ITE, GCATE)) %>%
    multibart::mb_modelmatrix()
  
  X_moderate <- dat %>%
    dplyr::select(LIKEMATH) %>%
    multibart::mb_modelmatrix()
  
  trt <- dat$trt
  Y <- as.numeric(dat$Y)
  pihat <- as.numeric(dat$weights)
  group <- dat$IDSCH
  
  # fit model
  fit_BCF <- multibart::bcf_binary(y = Y, z = trt, 
                                   x_control = X_control, 
                                   x_moderate = X_moderate,
                                   pihat = pihat,
                                   base_moderate = .95, 
                                   ntree_moderate = 100,
                                   power_moderate = 2, 
                                   sd_moderate = .5, 
                                   nburn = 1000, nsim = 1000, nthin = 1,
                                   include_pi = "control")
  
  # extract mu and tau from model
  fitted_mu <- fit_BCF$control_fit
  fitted_tau <- fit_BCF$moderate_fit
  
  # get cate estimates
  samples_ite <- multibart::get_forest_fit(fitted_tau, X_moderate)
  samples_cate <- apply(samples_ite, 1, mean)
  cate <- mean(samples_cate)
  cate_int <- quantile(samples_cate, c(0.025, 0.975))
  
  # get ITE estimates
  ite_pred <- apply(samples_ite, 2, mean)
  
  # get group predictions
  group_predictions <- unlist(lapply(split(ite_pred, group), mean)) %>% data.frame()
  group_predictions$IDSCH <- row.names(group_predictions)
  colnames(group_predictions) <- c("gcate", "IDSCH")
  group_compare <- dplyr::left_join(dat, group_predictions, by = "IDSCH")
  group_diff <- as.numeric(group_compare$gcate) - as.numeric(group_compare$GCATE)
  
  
  # format results
  if (one_run) {
    
    results <-
      dat %>%
      mutate(ite_pred = ite_pred)
    
  } else {
    
    results <- tidyr::tibble(CATE_est = cate,
                             CATE_true = mean(dat$ITE),
                             CATE_LB = cate_int[1],
                             CATE_UB = cate_int[2],
                             PEHE = sqrt(mean((ite_pred - dat$ITE)^2)) / sd(Y),
                             PEGSTE = sqrt(mean((group_diff)^2)) / sd(Y))
    
  }
  
  # package and send out results
  return(results)
  
}

# define MBCF method
analysis_MBCF <- function(dat, one_run = FALSE) {
  
  # create data objects
  X_control <- dat %>% 
    dplyr::select(-c(Y, IDSTU, IDSCH, trt, weights, ITE, GCATE)) %>%
    multibart::mb_modelmatrix()
  
  X_moderate <- dat %>%
    dplyr::select(LIKEMATH) %>%
    multibart::mb_modelmatrix()
  
  trt <- dat$trt
  Y <- as.numeric(dat$Y)
  pihat <- as.numeric(dat$weights)
  group <- dat$IDSCH
  
  # random effects
  random_effect_setup <- random_intercepts(groups = dat$IDSCH, 
                                           original_colnames = TRUE)
  
  # fit model
  fit_mBCF <- multibart::bcf_binary(y = Y, z = trt, 
                                    x_control = X_control, 
                                    x_moderate = X_moderate,
                                    pihat = pihat,
                                    randeff_design = random_effect_setup$randeff_design, 
                                    randeff_variance_component_design = random_effect_setup$randeff_variance_component_design, 
                                    base_moderate = .95, 
                                    ntree_moderate = 100,
                                    power_moderate = 2, 
                                    sd_moderate = .5, 
                                    nburn = 1000, nsim = 1000, nthin = 1,
                                    include_pi = "control")
  
  # extract mu and tau from model
  fitted_mu <- fit_mBCF$control_fit
  fitted_tau <- fit_mBCF$moderate_fit
  
  # get SATE estimates
  samples_ite <- multibart::get_forest_fit(fitted_tau, X_moderate)
  samples_cate <- apply(samples_ite, 1, mean)
  cate <- mean(samples_cate)
  cate_int <- quantile(samples_cate, c(0.025, 0.975))
  
  # get ITE estimates
  ite_pred <- apply(samples_ite, 2, mean)
  
  # get group predictions
  group <- dat$IDSCH
  group_predictions <- unlist(lapply(split(ite_pred, group), mean)) %>% data.frame()  
  group_predictions$IDSCH <- row.names(group_predictions)
  colnames(group_predictions) <- c("gcate", "IDSCH")
  group_compare <- dplyr::left_join(dat, group_predictions, by = "IDSCH")
  group_diff <- as.numeric(group_compare$gcate) - as.numeric(group_compare$GCATE)
  
  # format results
  if (one_run) {
    
    results <-
      dat %>%
      mutate(ite_pred = ite_pred)
    
  } else {
    
    results <- tidyr::tibble(CATE_est = cate,
                             CATE_true = mean(dat$ITE),
                             CATE_LB = cate_int[1],
                             CATE_UB = cate_int[2],
                             PEHE = sqrt(mean((ite_pred - dat$ITE)^2)) / sd(Y),
                             PEGSTE = sqrt(mean((group_diff)^2)) / sd(Y))
    
  }
  
  # package and send out results
  return(results)
  
}

# Estimation Function -----------------------------------------------------

# define estimation procedure
estimate <- function(dat, one_run = FALSE) {
  
  # get propensity scores for input data
  dat$weights <- analysis_propensity(dat = dat)
  gc()
  
  # run multi-level model
  MLM_fit <- analysis_MLM(dat = dat)
  gc()
  
  # run X-BART
  XBART_fit <- analysis_XBART(dat = dat)
  gc()
  
  # run stan4bart
  StanBART_fit <- analysis_StanBART(dat = dat)
  gc()
  
  # run BCF
  BCF_fit <- analysis_BCF(dat = dat)
  gc()
  
  # run MBCF
  MBCF_fit <- analysis_MBCF(dat = dat)
  gc()
  
  # package results
  dplyr::bind_rows(`MLM` = MLM_fit,
                   `X-BART` = XBART_fit,
                   `Stan4bart` = StanBART_fit, 
                   `BCF` = BCF_fit,
                   `MBCF` = MBCF_fit,
                   .id = "method")
  
}





