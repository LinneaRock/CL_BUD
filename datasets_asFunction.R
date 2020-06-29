library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)
source("Functions/readSP.R")
source("Functions/SC.R")
source("Functions/readcl.R")
source("Functions/readfieldcl.R")
source("Functions/readfieldcond.R")
source("Functions/readLL.R")
source("Functions/readSPlost_hour.R")

Sites <- c("Lake Mendota - Epilimnion", "Lake Mendota - Hypolimnion", "Lake Monona - Epilimnion", "Lake Monona - Hypolimnion", "Yahara River - North", "Yahra River - Isthmus", "Yahara River - South", "Stakweather Creek", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Spring Harbor Storm Sewer", "Willow Creek")
Abbr <- c("ME_Epi", "ME_Hypo", "MO_Epi", "MO_Hypo", "YN", "YI", "YS", "SW", "6MC or SMC", "DC", "PBMS", "PBSF", "SH", "WC")

KEY <- data.frame(Sites, Abbr) 

#CONDUCTIVITY DATA####

loggerME_Epi <- readSP("ME", c("Data/HOBO_Loggers/MENDOTA/SURFACE_2019-20/20758346.csv")) %>%
  filter(date <= ymd_hms("2020-04-01 09:45:00"))

loggerME_Hypo <- readSP("ME", c("Data/HOBO_Loggers/MENDOTA/BOTTOM_2019-20/20758341.csv")) %>%
  filter(date <= ymd_hms("2020-04-01 09:45:00"))

loggerMO_Epi <- readSP("MO", c("Data/HOBO_Loggers/MONONA/SURFACE_2019-20/20758345.csv")) %>%
  filter(date <= ymd_hms("2020-05-12 12:45:00"))

loggerMO_Hypo <- readSP("MO", c("Data/HOBO_Loggers/MONONA/BOTTOM_2019-20/20758340.csv")) %>%
  filter(date <= ymd_hms("2020-05-12 12:45:00"))



loggerYN <- readSP("YN", c("Data/HOBO_Loggers/YN/Dec19_Feb4/20758343_YN.csv", "Data/HOBO_Loggers/YN/Feb4_Mar16/20758343_YN.csv"))
loggerYN <- rbind(loggerYN, readSPlost_hour("YN","Data/HOBO_Loggers/YN/Mar16_Jun17/20758343_YN.csv")) %>%
  mutate(Low.Range = ifelse(date == ymd_hms("2020-05-11 07:00:00"), 424.3, Low.Range),
         Full.Range = ifelse(date == ymd_hms("2020-05-11 07:00:00"), 424.05, Full.Range),
         sp.cond = ifelse(date == ymd_hms("2020-05-11 07:00:00"), SC(424.05, 9.25), sp.cond)) #There is a massive spike at this timestep only in the original dataset so I am setting the values to the mean of previous and post timestep.
  



loggerYI <- readSP("YI",c("Data/HOBO_Loggers/YI/Dec19_Feb4/20758347_YI.csv", "Data/HOBO_Loggers/YI/Feb4_Mar16/20758347_YI.csv")) %>%
  filter(date != ymd_hms("2020-02-04 14:30:00")) 
loggerYI <- rbind(loggerYI, readSPlost_hour("YI", "Data/HOBO_Loggers/YI/Mar16_Jun17/20758347_YI.csv"))



loggerYS <- readSP("YS", c("Data/HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv","Data/HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")) %>% 
  filter(date != ymd_hms("2020-03-16 08:30:00"))  #getting rid of data that were collected while logger was out of the water
loggerYS <- rbind(loggerYS, readSPlost_hour("YS", "Data/HOBO_Loggers/YS/Mar16_Jun17/20758348_YS.csv"))



loggerSW <- readSP("SW", c("Data/HOBO_Loggers/SW/Dec19_Feb3/20378151_SW.csv", "Data/HOBO_Loggers/SW/Feb3_Mar16/20378151_SW.csv")) 
loggerSW <- rbind(loggerSW, readSPlost_hour("YS", "Data/HOBO_Loggers/SW/Mar16_Jun17/20378151_SW.csv"))



  
logger6MC <- readSP("6MC", c("Data/HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv"))  %>%
  filter(date != ymd_hms("2020-03-16 10:00:00")) 
logger6MC <- rbind(logger6MC, readSPlost_hour("6MC", "Data/HOBO_Loggers/6MC/Mar16_Jun17/20758342_6MC.csv"))




loggerDC <- readSP("DC", c("Data/HOBO_Loggers/DC/Dec19_Feb4/20758338_DC.csv", "Data/HOBO_Loggers/DC/Feb4_Mar16/20758338_DC.csv")) 
loggerDC <- rbind(loggerDC, readSPlost_hour("DC", "Data/HOBO_Loggers/DC/Mar16_Jun17/20758338_DC.csv"))


  
loggerPBMS <- readSP("PBMS", c("Data/HOBO_Loggers/PBMS/Dec19_Feb3/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Feb3_Mar16/20758344_PBMS.csv")) 
loggerPBMS <- rbind(loggerPBMS, readSPlost_hour("PBMS", "Data/HOBO_Loggers/PBMS/Mar16_Jun17/20758344_PBMS.csv"))
  


loggerPBSF <- readSP("PBSF", c("Data/HOBO_Loggers/PBSF/Jan2_Jan15/20758339_PBSF.csv","Data/HOBO_Loggers/PBSF/Jan21_Feb4/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Feb4_Mar16/20758339_PBSF.csv"))
loggerPBSF <- rbind(loggerPBSF, readSPlost_hour("PBSF", "Data/HOBO_Loggers/PBSF/Mar16_Jun17/20758339_PBSF.csv"))


fieldcondYN <- readfieldcond("YN")
fieldcondYI <- readfieldcond("YI")
fieldcondYS <- readfieldcond("YS")
fieldcondSW <- readfieldcond("SW")
fieldcond6MC <- readfieldcond("6MC")
fieldcondDC <- readfieldcond("DC")
fieldcondPBMS <- readfieldcond("PBMS")
fieldcondPBSF <- readfieldcond("PBSF")


#Willow Creek data retrieved outside of function because it is not a standardized file
frmt <- function(d) {
  d %>%
    rename(sp.cond = CONDUCTANCE) %>%
    mutate(date = as.POSIXct(DATE, format = "%m-%d-%Y %H:%M:%S", tz = "GMT"))
}
downstream <- read_xlsx("Data/WRM_data/Downstream.xlsx") %>%
  filter(CONDUCTANCE > 26.8) 

downstream <- downstream[-c(132490:132541, 132586:132971, 136429), ] %>%
  frmt()

upstream <- read_xlsx("Data/WRM_data/Upstream.xlsx") %>%
  filter(CONDUCTANCE > 2.8) %>%
  frmt()

#CHLORIDE DATA####

labYN <- readCL("YN")
labYI <- readCL("YI")
labYS <- readCL("YS")
labSW <- readCL("SW")
lab6MC <- readCL("6MC")
labDC <- readCL("DC")
labPBMS <- readCL("PBMS")
labPBSF <- readCL("PBSF")

fieldclYN <- readfieldcl("YN")
fieldclYI <- readfieldcl("YI")
fieldclYS <- readfieldcl("YS")
fieldclSW <- readfieldcl("SW")
fieldcl6MC <- readfieldcl("6MC")
fieldclDC <- readfieldcl("DC")
fieldclPBMS <- readfieldcl("PBMS")

fieldclPBSF <- readfieldcl("PBSF") %>%
  mutate(chloride_mgL = as.numeric(chloride_mgL))


#Spring Harbor Data retreived outside of function because this is not a standardized file
labSH <- read_xlsx("Data/Historical_External/SpringHarborChloride.xlsx") %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "GMT")) %>%
  mutate(date = as.Date(as.character(date)))


##DISCHARGE DATA ####
format <- function(d) {
  d %>%
  rename(discharge = X_00060_00000,
         date = dateTime) %>%
  select(date, discharge) %>%
  mutate(discharge = discharge * 0.028316847)
}

d.YN <- readNWISuv("05427850", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()
d.YI <- readNWISuv("05428500", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()
d.6MC <- readNWISuv("05427910", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()
d.DC <- readNWISuv("05427930", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()
d.PBMS <- readNWISuv("05427948", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()
d.PBSF <- readNWISuv("054279465", "00060", "2019-01-01", "", tz = "GMT") %>%
  format()


#Spring Harbor Data retrieved outside of function because we are retrieving specific conductivity and discharge at the same time ####
d.sc.SH <- readNWISuv("05427965", c("00060", "00095"), "2014-02-19", "", tz = "GMT") %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(sp.cond = sp_cond,
         date = dateTime)
  m=


#Data needed to calculate Water Depth
readLL("Data/HOBO_Loggers/YS/Feb3_Mar16/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Feb3_Mar16")

readLL("Data/HOBO_Loggers/YS/Mar16_Jun17/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Mar16_Jun17")



