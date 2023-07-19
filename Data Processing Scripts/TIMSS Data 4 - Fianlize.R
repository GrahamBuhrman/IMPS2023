#--------------------------------------------------------------------------                                                                         
# Finalizing Data         TIMSS 2019 Data                                      
#--------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

require(dplyr)
require(tidyr)
require(naniar)


# Seed --------------------------------------------------------------------

set.seed(2023062)
seed = 2023062


# Read Data ---------------------------------------------------------------

dat_incomplete <- readRDS("TIMSS/missing_data.rds")
res_imputed <- readRDS("TIMSS/imputation_results.rds")
dat_processed <- readRDS("TIMSS/processed_data.rds")
dat_complete <- mice::complete(res_imputed)

# Finalize Data -----------------------------------------------------------

scale2 <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

dat_weights <- dat_processed %>% select(c(IDSTU, STUWGT:STUWGTFAC))
  
# Incomplete Data

dat_working_incomplete <-
  dat_incomplete %>%
  filter(PARENTED != 6) %>%
  mutate(IDSCH = as.factor(IDSCH),
         IDSTU = as.factor(IDSTU),
         STUSEX = as.factor(STUSEX), 
         TESTLANGSPOKEN = as.factor(TESTLANGSPOKEN),
         ABSENT = as.factor(ABSENT),
         PARENTED = as.factor(PARENTED),
         SCHREGION = as.factor(SCHREGION),
         OUTCLASS = as.factor(OUTCLASS),
         FREPL = as.factor(FREPL),
         PRIV = as.factor(PRIV)) 

dat_working_incomplete_scaled <- 
  dat_working_incomplete %>%
  mutate(across(where(is.numeric), scale2))

dat_analysis_incomplete <- inner_join(dat_working_incomplete, dat_weights, by = "IDSTU", multiple = "all")

# Complete Data

dat_working_complete <- 
  dat_complete %>%
  filter(PARENTED != 6) %>%
  mutate(IDSCH = as.factor(IDSCH),
         IDSTU = as.factor(IDSTU),
         STUSEX = as.factor(STUSEX), 
         TESTLANGSPOKEN = as.factor(TESTLANGSPOKEN),
         ABSENT = as.factor(ABSENT),
         PARENTED = as.factor(PARENTED),
         SCHREGION = as.factor(SCHREGION),
         OUTCLASS = as.factor(OUTCLASS),
         FREPL = as.factor(FREPL),
         PRIV = as.factor(PRIV))

dat_working_complete_scaled <- 
  dat_working_complete %>%
  mutate(across(where(is.numeric), scale2))

dat_analysis_complete <- inner_join(dat_working_complete, dat_weights, by = "IDSTU", multiple = "all")


# Get Covariate Space -----------------------------------------------------

dat_covariate_space <-
  dat_analysis_complete %>%
  select(IDSTU, IDSCH, MATHACH, STUSEX, ABSENT, PARENTED, FREPL, PRIV, HOMERES, BELONG, LIKEMATH, CONFMATH, ACADSUC) %>%
  fastDummies::dummy_cols(select_columns = c("STUSEX", "ABSENT", "PARENTED", "FREPL", "PRIV"))


# Save Data ---------------------------------------------------------------

saveRDS(dat_analysis_complete, "TIMSS/complete_analysis_data.rds")
saveRDS(dat_analysis_incomplete, "TIMSS/incomplete_analysis_data.rds")
saveRDS(dat_covariate_space, "TIMSS/TIMSS_covariates.rds")

