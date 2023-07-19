#--------------------------------------------------------------------------                                                                         
# Estimation        TIMSS 2019 Data                                      
#--------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

library(brms)
library(lme4)


# Seed --------------------------------------------------------------------

set.seed(2023067)
seed = 2023067


# Read Data ---------------------------------------------------------------

dat_analysis <- readRDS("TIMSS/complete_analysis_data.rds")



# Get Estimates -----------------------------------------------------------

fit_estimates_test <- brm(formula = (MATHACH/10) ~ 1 + HOMERES + BELONG + LIKEMATH + CONFMATH + ACADSUC + STUSEX + ABSENT + PARENTED + FREPL + PRIV + (1 | IDSCH),
                          data = dat_analysis,
                          chains = 4,
                          seed = seed,
                          warmup = 1000,
                          iter = 8000,
                          thin = 2,
                          cores = 4)

fit_estimates_test <- brm(formula = (MATHACH/10) ~ 1 + HOMERES + BELONG + LIKEMATH + CONFMATH + ACADSUC + (1 | IDSCH),
                          data = dat_analysis,
                          chains = 4,
                          seed = seed,
                          warmup = 1000,
                          iter = 8000,
                          thin = 2,
                          cores = 4)

