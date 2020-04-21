library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)

Sites <- c("Lake Mendota - Epilimnion", "Lake Mendota - Hypolimnion", "Lake Monona - Epilimnion", "Lake Monona - Hypolimnion", "Yahara River - North", "Yahra River - Isthmus", "Yahara River - South", "Stakweather Creek", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Spring Harbor Storm Sewer", "Willow Creek")
Abbr <- c("ME_Epi", "ME_Hypo", "MO_Epi", "MO_Hypo", "YN", "YI", "YS", "SW", "6MC or SMC", "DC", "PBMS", "PBSF", "SH", "WC")

KEY <- data.frame(Sites, Abbr) 



#Conductivity Datasets - final versions are called loggerXX where XX refers to the sample site, with exception - see notes in sections with exceptions ####

#function to convert actual conductivity to specific conductivity 
SC <- function(AC, t) {
  AC / (1 - (25 - t) * .019)
}

##ME####
#Loading in data from surface conductivity logger:
loggerME_Epi <- read.csv("HOBO_Loggers/MENDOTA/SURFACE_2019-20/20758346.csv") %>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>% #formatting date/time
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp)) %>%
  filter(date < "2020-04-01 9:45:00 ") #filtering datapooints collected post removal from lake

#Loading in data from bottom conductivity logger:
loggerME_Hypo <- read.csv("HOBO_Loggers/MENDOTA/BOTTOM_2019-20/20758341.csv") %>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>% #formatting date/time
  select(date, Low.Range, Full.Range, Temp) %>% #
  mutate(sp.cond = SC(Full.Range, Temp)) %>%
  filter(date < "2020-04-01 9:45:00 ") #filtering datapooints collected post removal from lake





##YN####
#Loading in data from conductivity loggers:
loggerYNY <- read.csv("HOBO_Loggers/YN/Dec19_Feb4/20758343_YN.csv")

loggerYNZ <- read.csv("HOBO_Loggers/YN/Feb4_Mar16/20758343_YN.csv")

#Combining and formatting logger data:
loggerYN <- loggerYNY %>%
  rbind(loggerYNZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp)%>%
  mutate(sp.cond = SC(Full.Range, Temp))



##YI####
#Loading in data from conductivity loggers:
loggerYIY <- read.csv("HOBO_Loggers/YI/Dec19_Feb4/20758347_YI.csv")

loggerYIZ <- read.csv("HOBO_Loggers/YI/Feb4_Mar16/20758347_YI.csv")

#Combining and formatting logger data:
loggerYI <- loggerYIY %>%
  rbind(loggerYIZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp)%>%
  mutate(sp.cond = SC(Full.Range, Temp)) %>%
  filter(date != "2020-02-04 14:30:00") #conductivity data collected while logger was out of water at this time YI

##YS####
#Loading in data from conductivity loggers:
loggerYSY <- read.csv("HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv")

loggerYSZ <- read.csv("HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")

#Combining and formatting logger data:
loggerYS <- loggerYSY %>%
  rbind(loggerYSZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp)) %>%
  filter(date != "2020-03-16 08:30:00") #getting rid of data that were collected while logger was out of the water


##SW####
#Loading in data from conductivity loggers:
loggerSWY <- read.csv("HOBO_Loggers/SW/Dec19_Feb3/20378151_SW.csv")

loggerSWZ <- read.csv("HOBO_Loggers/SW/Feb3_Mar16/20378151_SW.csv")

#Combining and formatting logger data:
loggerSW <- loggerSWY %>%
  rbind(loggerSWZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp)%>%
  mutate(sp.cond = SC(Full.Range, Temp))

##6MC####
#Loading in data from conductivity loggers:
logger6MCY <- read.csv("HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv")

logger6MCZ <- read.csv("HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv")

#Combining and formatting logger data:
logger6MC <- logger6MCY %>%
  rbind(logger6MCZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp)) %>%
  filter(date != "2020-03-16 10:00:00") #remove data collected while logger was out of water

##DC####
#Loading in data from conductivity loggers:
loggerDCY <- read.csv("HOBO_Loggers/DC/Dec19_Feb4/20758338_DC.csv")

loggerDCZ <- read.csv("HOBO_Loggers/DC/Feb4_Mar16/20758338_DC.csv")

#Combining and formatting logger data:
loggerDC <- loggerDCY %>%
  rbind(loggerDCZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp))

##PBMS####
#Loading in data from conductivity loggers:
loggerPBMSY <- read.csv("HOBO_Loggers/PBMS/Dec19_Feb3/20758344_PBMS.csv")

loggerPBMSZ <- read.csv("HOBO_Loggers/PBMS/Feb3_Mar16/20758344_PBMS.csv")

#Combining and formatting logger data:
loggerPBMS <- loggerPBMSY %>%
  rbind(loggerPBMSZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp))

##PBSF####
#Loading in data from conductivity loggers:
loggerPBSFY <- read.csv("HOBO_Loggers/PBSF/Jan2_Jan15/20758339_PBSF.csv")

loggerPBSFZ <- read.csv("HOBO_Loggers/PBSF/Jan21_Feb4/20758339_PBSF.csv")

loggerPBSFA <- read.csv("HOBO_Loggers/PBSF/Feb4_Mar16/20758339_PBSF.csv")

#Combining and formatting logger data:
loggerPBSF <- loggerPBSFY %>%
  rbind(loggerPBSFZ) %>%
  rbind(loggerPBSFA) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(#char = as.character(Date),
    date = as.POSIXct(Date, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
  select(date, Low.Range, Full.Range, Temp) %>%
  mutate(sp.cond = SC(Full.Range, Temp))

##SH#### 
#note this df is named d.sc.SH because it contains discharge and conductivity data
d.sc.SH <- readNWISuv("05427965", c("00060", "00095"), "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)



#Chloride Datasets - final versions are called called labXX where XX refers to sample site


##YN####
labYN.o <- read_xlsx("chloride_lab.xlsx", sheet = "YN") 
labYN <- labYN.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##YI####
#Loading in chloride data:
labYI.o <- read_xlsx("chloride_lab.xlsx", sheet = "YI") 
labYI <- labYI.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes")) 

##YS####
labYS.o <- read_xlsx("chloride_lab.xlsx", sheet = "YS") 
labYS <- labYS.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##SW####
labSW.o <- read_xlsx("chloride_lab.xlsx", sheet = "SW") 
labSW <- labSW.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##6MC####
lab6MC.o <- read_xlsx("chloride_lab.xlsx", sheet = "6MC") 
lab6MC <- lab6MC.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##DC####
labDC.o <- read_xlsx("chloride_lab.xlsx", sheet = "DC") 
labDC <- labDC.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##PBMS####
labPBMS.o <- read_xlsx("chloride_lab.xlsx", sheet = "PBMS") 
labPBMS <- labPBMS.o %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))

##PBSF####
labPBSF.o <- read_xlsx("chloride_lab.xlsx", sheet = "PBSF") 
labPBSF <- labPBSF.o %>%






#Discharge Datasets - final version are called d.XX where XX refers to the sample site ####
#retrieving data for USGS gage sites
##rename columns to make dfs easier to understand
###convert discharge from cfs to cms
####selectdate and discharge, sp. conductance columns

##YN####
d.YN <- readNWISuv("05427850", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

##YI####
d.YI <- readNWISuv("05428500", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

#6MC####
d.6MC <- readNWISuv("05427910", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

#DC####
d.DC <- readNWISuv("05427930", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

#PBMS####
d.PBMS <- readNWISuv("05427948", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

#PBSF####
d.PBSF <- readNWISuv("054279465", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)

