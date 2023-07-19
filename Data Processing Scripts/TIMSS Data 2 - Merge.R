#--------------------------------------------------------------------------                                                                         
# Merging TIMSS 2019 and US Specific Data         TIMSS 2019 Data                                      
#--------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

require(haven)
require(dplyr)


# Read Data ---------------------------------------------------------------

# TIMSS 2019 Data
file_list <- as.list(list.files("TIMSS", pattern = ".sav$", full.names = TRUE))
sav_dat_list <- lapply(file_list, read_sav)

# US Specific Data
US_school_data <- readRDS("TIMSS/US_school_data.rds")
US_student_data <- readRDS("TIMSS/US_student_data.rds")

colnames(US_school_data) <- c("IDSCHOOL", "LEP", "AVGINC_SCHOOLAREA", "FREPL", "PUBPRIV")
colnames(US_student_data) <- c("IDSCHOOL", "IDSTUD", "OUTCLASS", "OUTCLUB", "DAYSABSENT", "REPATELEM", "REPEATMID")


# Merge Data --------------------------------------------------------------

dat_level_1 <- dplyr::left_join(sav_dat_list[[2]], sav_dat_list[[3]], by = c("IDSCHOOL", "IDSTUD"))
dat_combi_TIMSS <- dplyr::left_join(dat_level_1, sav_dat_list[[1]], by = "IDSCHOOL")

dat_level_1_US <- dplyr::left_join(dat_combi_TIMSS, US_student_data, by = c("IDSCHOOL", "IDSTUD"))
dat_final <- dplyr::left_join(dat_level_1_US, US_school_data, by = "IDSCHOOL")

saveRDS(dat_final, "TIMSS/TIMSS_dat_raw.rds")



