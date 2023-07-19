### Load Covariates - TIMSS 2019 Data ###       

# Define Covariate Loading Function ---------------------------------------

load_covariates <- function() {
  
  # get data
  dat <- readRDS("Simulation Data/TIMSS_covariates.rds")
  
  # process variables
  dat$IDSCH <- as.factor(dat$IDSCH)
  dat$SEX <- dat$STUSEX_1
  dat$PRIVATE <- dat$PRIV_1
  dat$ABSENT_onceweek <- dat$ABSENT_1
  dat$ABSENT_oncetwoweeks <- dat$ABSENT_2
  dat$ABSENT_oncemonth <- dat$ABSENT_3
  dat$ABSENT_oncetwomonths <- dat$ABSENT_4
  dat$ABSENT_never <- dat$ABSENT_5
  dat$PARED_univ <- dat$PARENTED_1
  dat$PARED_postsecondary <- dat$PARENTED_2
  dat$PARED_uppersecondary <- dat$PARENTED_3
  dat$PARED_lowersecondary <- dat$PARENTED_4
  dat$PARED_someprimary <- dat$PARENTED_5
  dat$FREPL_tenth <- dat$FREPL_0
  dat$FREPL_quarter <- dat$FREPL_1
  dat$FREPL_half <- dat$FREPL_2
  dat$FREPL_threequarter <- dat$FREPL_3
  dat$FREPL_overthreequarter <- dat$FREPL_4
  
  # organize variables
  covs_cat_final <- c("STUSEX", "PRIV", "ABSENT", "FREPL", "PARENTED")
  covs_cont <- c("HOMERES", "BELONG", "LIKEMATH", "CONFMATH", "ACADSUC")
  covs_cat_gen <- c("SEX", "PRIVATE", "ABSENT_onceweek", "ABSENT_oncetwoweeks", 
                "ABSENT_oncemonth", "ABSENT_oncetwomonths", "ABSENT_never",
                "PARED_univ", "PARED_postsecondary", "PARED_uppersecondary", 
                "PARED_lowersecondary", "PARED_someprimary", "FREPL_tenth",
                "FREPL_quarter", "FREPL_half", "FREPL_threequarter", "FREPL_overthreequarter")
  
  # select variables 
  dat <- dat[, c("IDSTU", "IDSCH", covs_cat_final, covs_cat_gen, covs_cont)]
  
  # return data
  return(dat)
  
}
