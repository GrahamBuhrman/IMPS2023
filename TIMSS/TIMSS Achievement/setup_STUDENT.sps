TIMSS/TIMSS Achievement/setup_STUDENT

This setup file was created using the R package asciiSetupReader(version 2.4.0) on 2023-06-01 20:26:22. For any feedback or problems (or if the file looks odd), please make a report on https://github.com/jacobkap/asciiSetupReader/issues. For more information on this package see here: https://jacobkap.github.io/asciiSetupReader/.


data list
SCHOOLID             4-7
STUDENTID            8-15
OUTSCHOOL_CLASS      44
OUTSCHOOL_CLUB       45
DAYSABSENT           49
REPEATELEM           50
REPEATMID            51
.

variable labels
SCHOOLID             "SCHOOLID"
STUDENTID            "STUDENTID"
OUTSCHOOL_CLASS      "OUTSCHOOL_CLASS"
OUTSCHOOL_CLUB       "OUTSCHOOL_CLUB"
DAYSABSENT           "DAYSABSENT"
REPEATELEM           "REPEATELEM"
REPEATMID            "REPEATMID"
.

value labels
OUTSCHOOL_CLASS      
'1'                  "Yes"
'2'                  "No"
'9'                  "Omitted or Invalid"
OUTSCHOOL_CLUB       
'1'                  "Yes"
'2'                  "No"
'9'                  "Omitted or Invalid"
REPEATELEM           
'1'                  "Yes"
'2'                  "No"
'9'                  "Omitted or Invalid"
REPEATMID            
'1'                  "Yes"
'2'                  "No"
'9'                  "Omitted or Invalid"
.

missing values
OUTSCHOOL_CLASS = 9      (OUTSCHOOL_CLUB = 9, DAYSABSENT = 9, REPEATELEM = 9, REPEATMID = 9)     
.


execute
