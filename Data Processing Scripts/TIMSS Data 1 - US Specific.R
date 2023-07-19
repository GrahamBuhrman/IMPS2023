#--------------------------------------------------------------------------                                                                         
# US Specific Data Script         TIMSS 2019 Data                                      
#--------------------------------------------------------------------------


# Packages ----------------------------------------------------------------

require(asciiSetupReader)


# Read Data School --------------------------------------------------------

make_sps_setup(
  file_name = "TIMSS/TIMSS Achievement/setup_SCHOOL",
  col_positions = c("4-7", "8-9", "12", "21", "22"),
  col_names = c("SCHOOLID", "LEP", "AVGINCOME_SCHOOLAREA", "FREPL", "PUBPRIV"),
  col_labels = c("SCHOOLID", "LEP", "AVGINCOME_SCHOOLAREA", "FREPL", "PUBPRIV"),
  value_labels = c("LEP = ",
                   "1 = 0%",
                   "2 = 1-5%",
                   "3 = 6-10%",
                   "4 = 11-25%",
                   "5 = 26-50%",
                   "6 = 51-75%",
                   "7 = 76-90%",
                   "8 = > 90%",
                   "9 = Omitted or Invalid",
                   "AVGINCOME_SCHOOLAREA = ",
                   "1 = High",
                   "2 = Medium",
                   "3 = Low",
                   "9 = Omitted or Invalid",
                   "FREPL = ",
                   "1 = < 10%",
                   "2 = 10-24.9%",
                   "3 = 25-49.9%",
                   "4 = 50-74.9%",
                   "5 = > 75%",
                   "9 = Omitted or Invalid",
                   "PUBPRIV = ",
                   "1 = Public",
                   "2 = Private",
                   "9 = Omitted or Invalid"),
  missing_values = c("LEP = 99", "AVGINCOME_SCHOOLAREA = 9", "FREPL = 9", "PUBPRIV = 9")
)

dat_school <- 
  read_ascii_setup(
  data = "TIMSS/TIMSS Achievement/T8_SCHOOL19.dat",
  setup_file = "TIMSS/TIMSS Achievement/setup_SCHOOL.sps")


# Read Data Student -------------------------------------------------------

make_sps_setup(
  file_name = "TIMSS/TIMSS Achievement/setup_STUDENT",
  col_positions = c("4-7", "8-15", "44", "45", "49", "50", "51"),
  col_names = c("SCHOOLID", "STUDENTID","OUTSCHOOL_CLASS", "OUTSCHOOL_CLUB", "DAYSABSENT", "REPEATELEM", "REPEATMID"),
  col_labels = c("SCHOOLID", "STUDENTID", "OUTSCHOOL_CLASS", "OUTSCHOOL_CLUB", "DAYSABSENT", "REPEATELEM", "REPEATMID"),
  value_labels = c("OUTSCHOOL_CLASS = ",
                   "1 = Yes",
                   "2 = No",
                   "9 = Omitted or Invalid",
                   "OUTSCHOOL_CLUB = ",
                   "1 = Yes",
                   "2 = No",
                   "9 = Omitted or Invalid",
                   "REPEATELEM = ",
                   "1 = Yes",
                   "2 = No",
                   "9 = Omitted or Invalid",
                   "REPEATMID = ",
                   "1 = Yes",
                   "2 = No",
                   "9 = Omitted or Invalid"),
  missing_values = c("OUTSCHOOL_CLASS = 9", "OUTSCHOOL_CLUB = 9", "DAYSABSENT = 9", "REPEATELEM = 9", "REPEATMID = 9")
)

dat_student <- 
  read_ascii_setup(
    data = "TIMSS/TIMSS Achievement/T8_STUDENT19.dat",
    setup_file = "TIMSS/TIMSS Achievement/setup_STUDENT.sps")


# Save Data Files ---------------------------------------------------------

saveRDS(dat_school, "TIMSS/US_school_data.rds")
saveRDS(dat_student, "TIMSS/US_student_data.rds")



