#--------------------------------------------------------------------------                                                                         
# Processing         TIMSS 2019 Data                                      
#--------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

require(dplyr)
require(haven)
require(tidyr)
require(mice)
require(miceadds)
require(micemd)
require(future)
require(furrr)


# Seed --------------------------------------------------------------------

set.seed(2023315)
seed = 2023315


# Read Data ---------------------------------------------------------------

dat_raw <- readRDS("TIMSS/TIMSS_dat_raw.rds")


# Process Data ------------------------------------------------------------

dat_processed <-
  dat_raw %>%
  dplyr::select(
    IDSCH = IDSCHOOL,
    IDSTU = IDSTUD,
    STUWGT = TOTWGT.x,
    SCHWGT = SCHWGT,
    SUMSTUWGT = STOTWGTU,
    SCHWGTADJ = WGTADJ1.x,
    STUWGTADJ = WGTADJ3.x,
    SCHWGTFAC = WGTFAC1.x,
    STUWGTFAC = WGTFAC3.x,
    MATHACH = BSMMAT01.x,
    STUSEX = ITSEX,
    TESTLANGSPOKEN = BSBG03,
    ABSENT = BSBG10,
    HOMERES = BSBGHER,
    BELONG = BSBGSSB,
    LIKEMATH = BSBGSLM,
    CONFMATH = BSBGSCM,
    PARENTED = BSDGEDUP,
    SCHREGION = BCBG05B,
    ACADSUC = BCBGEAS,
    OUTCLASS,
    FREPL,
    PUBPRIV) %>%
  sapply(X = ., FUN = zap_labels) %>%
  tidyr::as_tibble() %>%
  dplyr::mutate(across(everything(), \(x) na_if(x, "Omitted or Invalid"))) %>%
  dplyr::mutate(across(everything(), \(x) na_if(x, "99"))) %>%
  dplyr::mutate(across(everything(), \(x) na_if(x, "9"))) %>%
  mutate(IDSCH = as.factor(IDSCH),
         IDSTU = as.factor(IDSTU),
         STUWGT = as.numeric(STUWGT),
         SCHWGT = as.numeric(SCHWGT),
         SUMSTUWGT = as.numeric(SUMSTUWGT),
         SCHWGTADJ = as.numeric(SCHWGTADJ),
         STUWGTADJ = as.numeric(STUWGTADJ),
         SCHWGTFAC = as.numeric(SCHWGTFAC),
         STUWGTFAC = as.numeric(STUWGTFAC),
         MATHACH = as.numeric(MATHACH),
         STUSEX = as.numeric(STUSEX) - 1,
         TESTLANGSPOKEN = as.numeric(TESTLANGSPOKEN),
         ABSENT = as.numeric(ABSENT),
         HOMERES = as.numeric(HOMERES),
         BELONG = as.numeric(BELONG),
         LIKEMATH = as.numeric(LIKEMATH),
         CONFMATH = as.numeric(CONFMATH),
         PARENTED = as.numeric(PARENTED),
         SCHREGION = as.numeric(SCHREGION),
         ACADSUC = as.numeric(ACADSUC),
         OUTCLASS = case_when(OUTCLASS == "No" ~ 0,
                              OUTCLASS == "Yes" ~ 1),
         FREPL = case_when(FREPL == "< 10%" ~ 0,
                           FREPL == "10-24.9%" ~ 1,
                           FREPL == "25-49.9%" ~ 2,
                           FREPL == "50-74.9%" ~ 3,
                           FREPL == "> 75%" ~ 4),
         PRIV = case_when(PUBPRIV == "Public" ~ 0,
                          PUBPRIV == "Private" ~ 1)) %>%
  dplyr::select(-PUBPRIV)

miss_all <- 
  dat_processed %>% 
  group_by(IDSCH) %>% 
  naniar::miss_var_summary() %>% 
  filter(pct_miss == 100)

dat_missing <-
  dat_processed %>%
  filter(!IDSCH %in% miss_all$IDSCH) %>%
  dplyr::select(-c(STUWGT:STUWGTFAC)) %>%
  mutate(IDSCH = as.integer(IDSCH)) %>%
  as.data.frame()

# Save Data ---------------------------------------------------------------

saveRDS(dat_processed, "TIMSS/processed_data.rds")
saveRDS(dat_missing, "TIMSS/missing_data.rds")


# Impute Missing Data -----------------------------------------------------

ind_clust <- 1

pm <- mice(dat_missing, m = 1, maxit = 0)$pred
pm[ind_clust, ind_clust] <- 0
pm[-ind_clust, ind_clust] <- -2

res_mice <- futuremice(dat_missing, n.core = parallel::detectCores() - 1, parallelseed = seed, m = 100, defaultMethod = "pmm", predictorMatrix = pm, maxit = 150)


# Save Imputation Results -------------------------------------------------

saveRDS(res_mice, "TIMSS/imputation_results.rds")
