### Simulation Driver - TIMSS 2019 Data Simulation ###  


# Packages ----------------------------------------------------------------

require(dplyr)
require(purrr)

# Source Scripts ----------------------------------------------------------

source("Simulation Scripts/Load Covariates.R")
source("Simulation Scripts/Data Generation.R")
source("Simulation Scripts/Estimation.R")

# Simulation Driver -------------------------------------------------------

run_sim <- function(beta_0, beta_1, beta_2, beta_3, beta_4, beta_5,
                    beta_6, beta_7, beta_8, beta_9, beta_10, beta_11,
                    beta_12, beta_13, beta_14, beta_15, beta_16, beta_17,
                    beta_18, beta_19, beta_20, beta_21, beta_22,
                    trt_ceiling, RCT, prop_PRIV, ICC, J, testing = FALSE, one_run, 
                    iterations, seed = seed) {
  
  # check if seed is set and set seed if not set already
  if (!is.null(seed)) set.seed(seed)
  
  # stop the function if ICC is not between 0 and 1
  stopifnot(ICC >= 0 && ICC < 1)
  
  # stop the function if one_run is TRUE but iterations greater than 1
  if (one_run == TRUE) {stopifnot(iterations == 1)}
  
  # create beta vector for data generation input
  beta_vec <- c(beta_0, beta_1, beta_2, beta_3, beta_4, beta_5,
                beta_6, beta_7, beta_8, beta_9, beta_10, beta_11,
                beta_12, beta_13, beta_14, beta_15, beta_16, beta_17,
                beta_18, beta_19, beta_20, beta_21, beta_22)
  
  # run iterations
  purrr::map(1:iterations, ~ {
    
    # data generation
    dat <- generate_data(beta_vec = beta_vec, trt_ceiling = trt_ceiling, RCT = RCT, prop_PRIV = prop_PRIV, sigma_2 = 1 - ICC, tau_2 = ICC, J = J, testing = testing)
    
    # estimation procedure
    estimate(dat = dat, one_run = one_run)
    
  }) %>% dplyr::bind_rows(.id = "iteration")
  
}
