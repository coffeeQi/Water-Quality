#YSI sites to NPSTORET StationIDs with qualifiers and rejections



# library and set up ------------------------------------------------------
library(tidyverse)
library(readr)
library(lubridate)
library(dplyr)
library(purrr)
library(unpivotr)
library(readxl)
library(janitor)
library(anytime)
library(readxl)
library(xlsx)
library(strex)
library(hms)
library(arcgis)
#run line below to install arcgis package as of 3.16.2025
# install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))
#https://developers.arcgis.com/r-bridge/installation/


#choose directory
##desktop directory
# directory <- "C:/Users/areynolds/OneDrive - skokomish.org/"

##laptop directory
directory <- "C:/Users/Alena Reynolds/OneDrive - skokomish.org/"


# Bring in and configure calibration/Precheck/Postcheck data ---------------------------------

# not sure what entirely this does
set_arc_token(token)

# get the layer url you are trying to pull from
##this is SWQM QC table "0" or the first parent table
rul0<-"https://services6.arcgis.com/Rie25qW2R0NGMjcn/arcgis/rest/services/service_4f30060ede72457ba71c321f2a44982f/FeatureServer/0" 
##this is SWQM QC table "1" or the second parent/child table
rul1<-"https://services6.arcgis.com/Rie25qW2R0NGMjcn/arcgis/rest/services/service_4f30060ede72457ba71c321f2a44982f/FeatureServer/1"

# creates the pull url, include all query and token to access the data
layer <- arc_open(rul0, token)
layer1 <- arc_open(rul1, token)

# uses the pull url to import your data
data0<-arc_select(layer)
data1 <- arc_select(layer1)

datatble0 <- data0 %>% 
  rename(data0_globalID=globalid) %>% 
  mutate(DATE= as.Date(survey_date)) %>% 
  select(data0_globalID,DATE)

datatble1 <- data1 %>% 
  rename(data0_globalID=parentglobalid) %>% 
  select(data0_globalID, parameter_choice,standard_choice, standard_value,
         is_this_a_cal, cal_value,precheck_value, postcheck_value, criteriacal_do, 
         criteriacal_cond, criteriacal_pH, criteriacal_precheck_cond, criteriacal_precheck_pH)

swqm_qc_survey <- left_join(datatble0,datatble1,by='data0_globalID')

pivot_swqm_qc <- pivot_longer(swqm_qc_survey, cols = c(10:14), names_to = 'Criteria_method',
                              values_to = 'Criteria')

swqm_qc_criteria <- pivot_swqm_qc %>% 
  filter(Criteria!="NA") %>% 
  mutate(cross=case_when((parameter_choice=="dissolved_oxygen") ~'ODO',
                         (parameter_choice=="conductivity") ~'Cond',
                         (standard_choice== "ph_7") ~'pH',
                         (standard_choice== "ph_4") ~'pH',
                         TRUE ~ ''))

# Bring in and configure YSI results --------------------------------------

#set file path to be pulled to appropriate one you want to import
YSIcsvfiles <- list.files(path=paste0(directory,"Water Quality/Sampling/Results/KOR DSS exports/WY2025/postKorUpdate"),
                          pattern = ".csv", full.names = TRUE, recursive = FALSE)

#create an empty container for all the files to be appended 
bind_data <- NULL

#assigns f to each csv file
for(f in YSIcsvfiles){
  # print("file=",f)
  #reads in each csv file
  # alt + - = shortcut for arrow
  # f <- paste0(directory,"Water Quality/Sampling/Results/KOR DSS exports/WY2025/postKorUpdate/rready.2024.11.19.csv")
  
  Header <- read_csv(f, col_names = FALSE,  n_max = 1)

  #get rid of stupid wonky characters - used '\u00b5' instead of '\uFFFD'
  HeaderNWC <- str_split_i(Header, "-", 1)
  # HeaderNWC <- str_remove(Header,'\u00b5')
  data <- read_csv(f,col_names = HeaderNWC, skip = 1)
  dataselect <- data |> 
    select(c(1:2,4,7:9,11,13:14,18,19,20))|>
    clean_names()
  dataselect$date <- mdy(dataselect$date)
  # dataselect$TIME <- hms(dataselect$TIME)
  #pivot data so it is in row major format
  dataselectpivotlonger <- pivot_longer(dataselect, cols = c(4:12), names_to = 'Parameter',
                                        values_to = 'Result') |> 
    mutate(filename = f)|> 
    rename(SITE=site_name, DATE=date) 
  
  bind_data <- bind_rows(bind_data,dataselectpivotlonger)
  
  
}# end of csv for loop

#export to do a manual check
#write.csv(bind_data,file="F:/OneDrive - skokomish.org/Documents/R/manualcheck.csv")

#average duplicates
averaged_bind_data <- bind_data %>% 
  group_by(DATE,SITE,Parameter) %>% 
  summarise(Count = n(),average= round(mean(Result,na.rm = TRUE)), 5) %>% 
  ungroup()

#,time=mean(TIME) for averaging time for dups
#class(averaged_bind_data$time)

#join averages with other data
aveplusall_bind_data <- left_join(averaged_bind_data,bind_data,
                                  by=c("SITE","Parameter", "DATE"))

#remove dups by average
USE_THIS <- distinct(aveplusall_bind_data)
# USE_THIS <- aveplusall_bind_data %>% 
#   distinct()

#class(USE_THIS$TIME)

#convert time - not necessary
#averaged_bind_data %>% 
#  strptime(unclass(averaged_bind_data$time), format="%H:%M:%S",tz="")


# change temp from F to C -------------------------------------------------

## formula for Convert Fahrenheit to Celsius - https://www.geeksforgeeks.org/temperature-conversion-in-r/
# fahrenheit_to_celsius <- function(fahrenheit) {
#   return((fahrenheit - 32) * 5/9)
# }
# 
# ##convert with function
# USE_THIS$average_result <- ifelse(USE_THIS$Parameter=="Temp (F)",
#                            fahrenheit_to_celsius(USE_THIS$average_result),USE_THIS$average_result)
# ##change name of parameter
# USE_THIS$Parameter <- ifelse(USE_THIS$Parameter=="Temp (F)",
#                              "Temp (C)",USE_THIS$Parameter)


#Use sites and parameter crosswalks to match NPSTORET and filter for only data you want to enter -------------------------------------------------------------------------

#Bring in Locations Crosswalk and Character Names Crosswalk

LocationsCrosswalk <- read_excel(path=paste0(directory,"Water Quality/LocationsCrosswalk.xlsx"))

LocalCharNamesCrosswalk <- read_excel(path=paste0(directory,"Water Quality/LocCharName_Crosswalk.xlsx"))

##keep everything in  YSI files and bring in only columns that match ysi_sitenames and ysipar in crosswalk
##filter out all other parameters not entered in NPSTORET
bind_dataxwloc <- left_join(USE_THIS,LocationsCrosswalk,by=c('SITE'='ysi_sitenames')) %>% 
  filter(Parameter=="do_mg_l" | Parameter=="do_percent_sat"| Parameter=="ph"|
           Parameter=="sal_psu"| Parameter=="barometer_mmhg"| 
           Parameter=="sp_cond_m_s_cm"| Parameter=="temp_c")

bind_dataxwlocpar <- left_join(bind_dataxwloc,LocalCharNamesCrosswalk,
                               by=c('Parameter'='ysipar_cleannames'))

# add cross for cal & drift data to match ---------------------------------
USE_THISCross <- bind_dataxwlocpar %>% 
  mutate(cross=case_when((Parameter=="ph") ~'pH',
                         (Parameter=="sp_cond_m_s_cm") ~'Cond',
                         (Parameter== "sal_psu") ~'Cond',
                         (str_detect(Parameter,"do")) ~ 'ODO',
                         TRUE ~ ''))

# bring cal and drift and results all together ----------------------------


bind_ALL <- left_join(USE_THISCross, swqm_qc_criteria, by=c("DATE", "cross"))
bind_ALLdistinct <- bind_ALL %>% 
  select(DATE,SITE,average,Count,time,LocSTATN_ORG_ID,StationID,StationName,DISPLAY_NAME,LocCharNameCode,
         ysipar,cross,Criteria_method,Criteria) %>% 
  distinct()

#CHECK that the qualifiers and rejections match the QC_Checks_YSI_ProDSS log



# add fields NPSTORET EED needs, rename for EED ---------------------------

##"AutoGenerate" will tell NPSTORET to make the next activity ID for that site and date
YSIALLNPS <- bind_ALLdistinct %>% 
  # filter(average_result!="NA") %>% 
  mutate(ActivityType="Field Msr/Obs-Portable Data Logger",ProjectID="SWQM",
         # ActivityStartTimeZone="PDT",
         DetectionCondition="Detected and Quantified",
         ActivityID="AutoGenerate",ChainOfCustodyID="Alena Reynolds",
         PersonName="Alena Reynolds",
         ActivityStartDate=as.character(DATE),
         MeasureQualifier=case_when((Criteria=="Qualify") ~'FDC',
                                    TRUE ~ ''),
         ValueStatus=case_when((Criteria=="Reject") ~'R',
                               TRUE ~ 'F')) %>% 
  rename(ResultText=average,StationID=StationID) %>% 
  #reclass date to date/time class
  mutate(
    # ActivityStartDate = as.POSIXct(ActivityStartDate,format="%Y-%m-%d" ,tz = "America/Los_Angeles"),
         # ActivityStartTime= str_after_nth(as.character(average_time) ," ", 1),
         #make timezone - CHANGE DATE to reflect if daylight savings time is correct since dst() stopped working
         ActivityStartTimeZone= ifelse(ActivityStartDate>'2024-03-10' & ActivityStartDate<'2024-11-03',
                                       "PDT", "PST"),
           # ifelse(dst(ActivityStartDate)=="TRUE","PDT","PST")) %>% 
         ValueType= if_else(Count > 1, "Calculated","Actual")) %>% 
  distinct()

#dst(YSIALLNPSTORET$ActivityStartDate)
#Reorder for final upload, get rid of extra columns
YSIALLNPSTORET <- YSIALLNPS %>% 
  select(ProjectID,StationID,ActivityID,ActivityType,ActivityStartDate,
         ActivityStartTime,ActivityStartTimeZone,ChainOfCustodyID,PersonName,LocCharNameCode,
         DetectionCondition,ResultText,ValueStatus,ValueType,MeasureQualifier) %>% 
  filter(ResultText!="NaN") %>% 
  distinct()


# export xlsx  ------------------------------------------------------------

#http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r

##CHANGE file NAME to correspond to what data is in it so we cannot import duplicates 
library(xlsx)
write.xlsx(as.data.frame(YSIALLNPSTORET),file=paste0(directory,"Water Quality/NPSTORET/YSI_imports/YSI_test_12.04.24.xlsx"))




