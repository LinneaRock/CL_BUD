library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)
source("Functions/readSP.R")
source("Functions/readcl.R")
source("Functions/readfieldcl.R")
source("Functions/readfieldcond.R")
source("Functions/readLL.R")
source("Functions/readLLDST.R")
source("Functions/readSPDST.R")

Sites <- c("Lake Mendota - Epilimnion", "Lake Mendota - Hypolimnion", "Lake Monona - Epilimnion", "Lake Monona - Hypolimnion", "Yahara River - North", "Yahra River - Isthmus", "Yahara River - South", "Stakweather Creek", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Spring Harbor Storm Sewer", "Willow Creek")
Abbr <- c("ME_Epi", "ME_Hypo", "MO_Epi", "MO_Hypo", "YN", "YI", "YS", "SW", "6MC or SMC", "DC", "PBMS", "PBSF", "SH", "WC")

KEY <- data.frame(Sites, Abbr) 

#CONDUCTIVITY DATA####

loggerME_Epi <- readSP("ME", c("Data/HOBO_Loggers/MENDOTA/SURFACE_2019-20/20758346.csv")) #%>%
  #filter(date <= ymd_hms("2020-04-01 14:45:00")) 
loggerME_Hypo <- readSP("ME", c("Data/HOBO_Loggers/MENDOTA/BOTTOM_2019-20/20758341.csv")) #%>%
  #filter(date <= ymd_hms("2020-04-01 14:45:00"))

loggerMO_Epi20 <- readSP("MO", c("Data/HOBO_Loggers/MONONA/SURFACE_2019-20/20758345.csv")) #%>%
  #filter(date <= ymd_hms("2020-05-12 18:45:00"))
loggerMO_Hypo20 <- readSP("MO", c("Data/HOBO_Loggers/MONONA/BOTTOM_2019-20/20758340.csv")) #%>%
  #filter(date <= ymd_hms("2020-05-12 18:45:00"))

loggerMO_Epi21 <- readSP("MO", "Data/HOBO_Loggers/MONONA/SURFACE_2020-21/20758346.csv")
loggerMO_Hypo21 <- readSP("MO", "Data/HOBO_Loggers/MONONA/BOTTOM_2020-21/20758340.csv")

loggerYN <- readSP("YN", c("Data/HOBO_Loggers/YN/Dec19_Feb4/20758343_YN.csv", "Data/HOBO_Loggers/YN/Feb4_Mar16/20758343_YN.csv"))
loggerYN <- rbind(loggerYN, readSPDST("YN",c("Data/HOBO_Loggers/YN/Mar16_Jun17/20758343_YN.csv", "Data/HOBO_Loggers/YN/Jun17_Aug26/20758343_YN.csv", "Data/HOBO_Loggers/YN/Aug26_Oct22/20758343_YN.csv", "Data/HOBO_Loggers/YN/Oct22_Dec15/20758343_YN.csv"))) %>%
  rbind(readSP("YN", c("Data/HOBO_Loggers/YN/Dec15_Mar9/20758343_YN.csv","Data/HOBO_Loggers/YN/Mar9_April10/20758343_YN.csv")))
     
  



loggerYI <- readSP("YI",c("Data/HOBO_Loggers/YI/Dec19_Feb4/20758347_YI.csv", "Data/HOBO_Loggers/YI/Feb4_Mar16/20758347_YI.csv")) %>%
  #filter(date != ymd_hms("2020-02-04 20:30:00")) %>%
  rbind(readSPDST("YI", c("Data/HOBO_Loggers/YI/Mar16_Jun17/20758347_YI.csv", "Data/HOBO_Loggers/YI/Jun17_Jul22/20758347_YI.csv", "Data/HOBO_Loggers/YI/Jul22_Aug26/20758347_YI.csv", "Data/HOBO_Loggers/YI/Aug26_Oct22/20758347_YI.csv", "Data/HOBO_Loggers/YI/Oct22_Dec15/20758347_YI.csv"))) %>%
  rbind(readSP("YI", c("Data/HOBO_Loggers/YI/Dec15_Mar9/20758347_YI.csv","Data/HOBO_Loggers/YI/Mar9_April10/20758347_YI.csv")))

#here is an example of how to filter, select, etc. by a specific datetime
test <- loggerYI %>%
  filter(date == as.POSIXct("2020-06-16 9:30:00", tz = "ETC/GMT-6"))

loggerYS <- readSP("YS", c("Data/HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv","Data/HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")) %>% 
  #filter(date != ymd_hms("2020-03-16 14:30:00")) %>%  #getting rid of data that were collected while logger was out of the water
  rbind(readSPDST("YS", c("Data/HOBO_Loggers/YS/Mar16_Jun17/20758348_YS.csv", "Data/HOBO_Loggers/YS/Jun17_Aug26/20758348_YS.csv", "Data/HOBO_Loggers/YS/Aug26_Oct22/20758348_YS.csv", "Data/HOBO_Loggers/YS/Oct22_Dec15/20758348_YS.csv"))) %>%
  rbind(readSP("YS", c("Data/HOBO_Loggers/YS/Dec15_Mar9/20758348_YS.csv", "Data/HOBO_Loggers/YS/Mar9_April10/20758348_YS.csv")))





loggerSW <- readSP("SW", c("Data/HOBO_Loggers/SW/Dec19_Feb3/20378151_SW.csv", "Data/HOBO_Loggers/SW/Feb3_Mar16/20378151_SW.csv")) %>%
  rbind(readSPDST("SW", c("Data/HOBO_Loggers/SW/Mar16_Jun17/20378151_SW.csv", "Data/HOBO_Loggers/SW/Jun17_Aug26/20378151_SW.csv", "Data/HOBO_Loggers/SW/Aug26_Oct22/20378151_SW.csv", "Data/HOBO_Loggers/SW/Oct22_Dec15/20882401_SW.csv"))) %>%
  rbind(readSP("SW", c("Data/HOBO_Loggers/SW/Dec15_Mar9/20882401_SW.csv", "Data/HOBO_Loggers/SW/Mar9_April10/20882401_SW.csv")))



  
logger6MC <- readSP("6MC", c("Data/HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv"))  %>%
 # filter(date != ymd_hms("2020-03-16 16:00:00")) %>%
  rbind(readSPDST("6MC", c("Data/HOBO_Loggers/6MC/Mar16_Jun17/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Jun17_Jul21/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Jul21_Aug26/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Aug26_Oct22/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Oct22_Dec15/20758342_6MC.csv"))) %>%
  rbind(readSP("6MC", c("Data/HOBO_Loggers/6MC/Dec15_Mar9/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Mar9_April10/20758342_6MC.csv")))




loggerDC <- readSP("DC", c("Data/HOBO_Loggers/DC/Dec19_Feb4/20758338_DC.csv", "Data/HOBO_Loggers/DC/Feb4_Mar16/20758338_DC.csv")) %>%
  rbind(readSPDST("DC", c("Data/HOBO_Loggers/DC/Mar16_Jun17/20758338_DC.csv", "Data/HOBO_Loggers/DC/Jun17_Jul21/20758338_DC.csv", "Data/HOBO_Loggers/DC/Jul21_Aug26/20758338_DC.csv", "Data/HOBO_Loggers/DC/Aug26_Oct22/20758338_DC.csv", "Data/HOBO_Loggers/DC/Oct22_Dec15/20758338_DC.csv"))) %>%
  rbind(readSP("DC", c("Data/HOBO_Loggers/DC/Dec15_Mar9/20758338_DC.csv","Data/HOBO_Loggers/DC/Mar9_April10/20758338_DC.csv")))


  
loggerPBMS <- readSP("PBMS", c("Data/HOBO_Loggers/PBMS/Dec19_Feb3/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Feb3_Mar16/20758344_PBMS.csv")) %>%
  rbind(readSPDST("PBMS", c("Data/HOBO_Loggers/PBMS/Mar16_Jun17/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Jun17_Jul21/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Jul21_Aug26/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Aug26_Oct22/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Oct22_Dec15/20758344_PBMS.csv"))) %>%
  rbind(readSP("PBMS", c("Data/HOBO_Loggers/PBMS/Dec15_Mar9/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Mar9_April10/20758344_PBMS.csv")))
  


loggerPBSF <- readSP("PBSF", c("Data/HOBO_Loggers/PBSF/Jan2_Jan15/20758339_PBSF.csv","Data/HOBO_Loggers/PBSF/Jan21_Feb4/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Feb4_Mar16/20758339_PBSF.csv")) %>%
  rbind(readSPDST("PBSF", c("Data/HOBO_Loggers/PBSF/Mar16_Jun17/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Jun17_Jul21/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Jul21_Aug26/20758339_PBSF.csv"))) %>%
  rbind(readSPDST("PBSF", "Data/HOBO_Loggers/PBSF/Aug26_Oct22/20758339_PBSF.csv")) %>%
  rbind(readSPDST("PBSF", c("Data/HOBO_Loggers/PBSF/Oct22_Dec15/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Dec15_Feb2/20758339_PBSF.csv"))) %>%
  rbind(readSP("PBSF", c("Data/HOBO_Loggers/PBSF/Feb21_Mar9/20758339_PBSF.csv", "Data/HOBO_Loggers/PBSF/Mar9_April10/20758339_PBSF.csv")))

loggerWIC <- readSPDST("WIC", c("Data/HOBO_Loggers/WIC/OCt22_Dec15/20882406_WIC.csv")) %>%
  rbind(readSP("WIC", c("Data/HOBO_Loggers/WIC/Dec15_Mar9/20882406_WIC.csv", "Data/HOBO_Loggers/WIC/Mar9_April10/20882406_WIC.csv")))


fieldcondWIC <- readfieldcond("WIC", "WIC")
fieldcondYN <- readfieldcond("YN", "YN")
fieldcondYI <- readfieldcond("YI", "YI")
fieldcondYS <- readfieldcond("YS", "YS")
fieldcondSW <- readfieldcond("SW", "SW")
fieldcond6MC <- readfieldcond("6MC", "6MC")
fieldcondDC <- readfieldcond("DC", "DC")
fieldcondPBMS <- readfieldcond("PBMS", "PBMS")
fieldcondPBSF <- readfieldcond("PBSF", "PBSF")
fieldcondME <- readfieldcond("ME", "ME")
fieldcondMO <- readfieldcond("MO", "MO")


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
labWIC <- readCL("WIC")
labYN <- readCL("YN")
labYI <- readCL("YI")
labYS <- readCL("YS")
labSW <- readCL("SW")
lab6MC <- readCL("6MC")
labDC <- readCL("DC")
labPBMS <- readCL("PBMS")
labPBSF <- readCL("PBSF")
labME <- readCL("ME") %>% dplyr::select(1:5, 7:10)
labMO <- readCL("MO")

fieldclWIC <- readfieldcl("WIC")
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
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "Etc/GMT-6")) %>%
  mutate(date = as.Date(as.character(date))) %>%
  mutate(mon = months.POSIXt(date)) %>%
  mutate(season = NA) %>%
  mutate(season = ifelse(
    mon == "November" |
      mon == "December" |
      mon == "January" |
      mon == "February" |
      mon == "March", "November - March", season),
    season = ifelse(is.na(season), "April - October", season))


##DISCHARGE DATA ####
format <- function(d) {
  d %>%
  rename(discharge = X_00060_00000,
         date = dateTime) %>%
  select(date, discharge) %>%
  mutate(discharge = discharge * 0.028316847)
}

d.YN <- readNWISuv("05427850", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()
YNsiteINFO <- readNWISsite("05427850")
d.YI <- readNWISuv("05428500", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()
d.6MC <- readNWISuv("05427910", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()
d.DC <- readNWISuv("05427930", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()
SMC_DCSiteINFO <- readNWISsite(c("05427910", "05427930"))
d.PBMS <- readNWISuv("05427948", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()
d.PBSF <- readNWISuv("054279465", "00060", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  format()

stage_PBSF <- readNWISuv("054279465", "00065", "2019-12-01", "", tz = "Etc/GMT+6") %>%
  rename(stage = X_00065_00000,
         date = dateTime) %>%
  select(date, stage)

#Spring Harbor Data retrieved outside of function because we are retrieving specific conductivity and discharge at the same time ####
d.sc.SH <- readNWISuv("05427965", c("00060", "00095"), "2019-12-01", "", tz = "Etc/GMT+6") %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  dplyr::select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(sp.cond = sp_cond,
         date = dateTime)


cl_data_flow_sc <- readNWISuv("05427965", c("00060", "00095"), "2014-02-20", "2016-09-29", tz = "Etc/GMT-6")
cl_data_flow_sc <- cl_data_flow_sc %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  dplyr::select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(runningmean = sp_cond,
         date = dateTime,
         runningmeandis = discharge)
#cl_data_daily <- readNWISdv("05427965", c("00940", "70290", "00060"), "2014-02-20", "2016-09-29")




#Data needed to calculate Water Depth - readLLDST is used to deal with dates in central daylight savings time. 
#datasets need to be downloaded and saved by their dates because the model needs reference height points based on specific times. 
#The data is saved as an rds file when the function is run!! So it only needs to be run once (ever) for each time interval 
#readLL("Data/HOBO_Loggers/YS/Feb3_Mar16/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Feb3_Mar16")
#readLLDST("Data/HOBO_Loggers/YS/Mar16_Jun17/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Mar16_Jun17")
#readLLDST("Data/HOBO_Loggers/YS/Jun17_Aug26/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Jun17_Aug26")
#readLLDST("Data/HOBO_Loggers/YS/Aug26_Oct22/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Aug26_Oct22")
#Presssure data not available??? readLLDST("Data/HOBO_Loggers/YS/Oct22_Dec15/WL20484276.csv", "C:/Users/linne/Downloads/pressure_AOS.csv", "Aug26_Oct22")
