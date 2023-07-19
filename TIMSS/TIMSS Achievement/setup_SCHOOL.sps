TIMSS/TIMSS Achievement/setup_SCHOOL

This setup file was created using the R package asciiSetupReader(version 2.4.0) on 2023-06-01 20:26:21. For any feedback or problems (or if the file looks odd), please make a report on https://github.com/jacobkap/asciiSetupReader/issues. For more information on this package see here: https://jacobkap.github.io/asciiSetupReader/.


data list
SCHOOLID                  4-7
LEP                       8-9
AVGINCOME_SCHOOLAREA      12
FREPL                     21
PUBPRIV                   22
.

variable labels
SCHOOLID                  "SCHOOLID"
LEP                       "LEP"
AVGINCOME_SCHOOLAREA      "AVGINCOME_SCHOOLAREA"
FREPL                     "FREPL"
PUBPRIV                   "PUBPRIV"
.

value labels
LEP                       
'1'                       "0%"
'2'                       "1-5%"
'3'                       "6-10%"
'4'                       "11-25%"
'5'                       "26-50%"
'6'                       "51-75%"
'7'                       "76-90%"
'8'                       "> 90%"
'9'                       "Omitted or Invalid"
AVGINCOME_SCHOOLAREA      
'1'                       "High"
'2'                       "Medium"
'3'                       "Low"
'9'                       "Omitted or Invalid"
FREPL                     
'1'                       "< 10%"
'2'                       "10-24.9%"
'3'                       "25-49.9%"
'4'                       "50-74.9%"
'5'                       "> 75%"
'9'                       "Omitted or Invalid"
PUBPRIV                   
'1'                       "Public"
'2'                       "Private"
'9'                       "Omitted or Invalid"
.

missing values
LEP = 99      (AVGINCOME_SCHOOLAREA = 9, FREPL = 9, PUBPRIV = 9)     
.


execute
