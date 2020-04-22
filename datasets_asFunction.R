
library(tidyverse)
library(readxl)
library(lubridate)
library(dataRetrieval)

Sites <- c("Lake Mendota - Epilimnion", "Lake Mendota - Hypolimnion", "Lake Monona - Epilimnion", "Lake Monona - Hypolimnion", "Yahara River - North", "Yahra River - Isthmus", "Yahara River - South", "Stakweather Creek", "Sixmile Creek", "Dorn Creek", "Pheasant Branch - Main Stem", "Pheasant Branch - South Fork", "Spring Harbor Storm Sewer", "Willow Creek")
Abbr <- c("ME_Epi", "ME_Hypo", "MO_Epi", "MO_Hypo", "YN", "YI", "YS", "SW", "6MC or SMC", "DC", "PBMS", "PBSF", "SH", "WC")

KEY <- data.frame(Sites, Abbr) 

##CONDUCTIVITY DATA####
#function to convert actual conductivity to specific conductivity 
SC <- function(AC, t) {
  AC / (1 - (25 - t) * .019)
}

# Example of how to read in data from different standardized files using a function 
readSP <- function(files) {
  logger <- read.csv(files[1]) # read in first file
  
  for(i in 1:length(files)){ # if there are more than one file, rbind files 
    logger <- rbind(logger,read.csv(files[i]))
  }
  
  logger =  logger %>% 
    drop_na() %>%
    select(Date, Low.Range, Full.Range, Temp) %>%
    mutate(char = as.character(Date),
           date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago")) %>%
    select(date, Low.Range, Full.Range, Temp) %>%
    mutate(sp.cond = SC(Full.Range, Temp))
  
  return(logger)
}

##ME###
loggerME_Epi <- readSP(c("Data/HOBO_Loggers/MENDOTA/SURFACE_2019-20/20758346.csv")) %>%
  filter(date < "2020-04-01 9:45:00 ")

loggerME_Hypo <- readSP(c("Data/HOBO_Loggers/MENDOTA/BOTTOM_2019-20/20758341.csv"))%>%
  filter(date < "2020-04-01 9:45:00 ")

##YN###
loggerYN = readSP(c("Data/HOBO_Loggers/YN/Dec19_Feb4/20758343_YN.csv", "Data/HOBO_Loggers/YN/Feb4_Mar16/20758343_YN.csv"))

##YI###
loggerYI <- readSP(c("Data/HOBO_Loggers/YI/Dec19_Feb4/20758347_YI.csv", "Data/HOBO_Loggers/YI/Feb4_Mar16/20758347_YI.csv")) %>%
  filter(date != "2020-02-04 14:30:00")

##YS### 
#Loading in data from conductivity loggers:
loggerYS = readSP(c("Data/HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv","Data/HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")) %>% 
  filter(date != "2020-03-16 08:30:00") #getting rid of data that were collected while logger was out of the water

##SW###
loggerSW <- readSP(c("Data/HOBO_Loggers/SW/Dec19_Feb3/20378151_SW.csv", "Data/HOBO_Loggers/SW/Feb3_Mar16/20378151_SW.csv"))

##6MC###
#Loading in data from conductivity loggers:
fileNames = c("Data/HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv", "Data/HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv")
logger6MC = readSP(fileNames)  %>%
  filter(date != "2020-03-16 10:00:00")

##DC###
loggerDC <- readSP(c("Data/HOBO_Loggers/DC/Dec19_Feb4/20758338_DC.csv", "Data/HOBO_Loggers/DC/Feb4_Mar16/20758338_DC.csv"))

##PBMS###
loggerPBMS <- readSP(c("Data/HOBO_Loggers/PBMS/Dec19_Feb3/20758344_PBMS.csv", "Data/HOBO_Loggers/PBMS/Feb3_Mar16/20758344_PBMS.csv"))

##PBSF###
loggerPBSF <- readSP(c("Data/HOBO_Loggers/PBSF/Jan2_Jan15/20758339_PBSF.csv","Data/HOBO_Loggers/PBSF/Jan21_Feb4/20758339_PBSF.csv", "HOBO_Loggers/PBSF/Feb4_Mar16/20758339_PBSF.csv" ))


#Willow Creek data retrieved outside of function because it is not a standardized file
frmt <- function(d) {
  d %>%
    rename(sp.cond = CONDUCTANCE) %>%
    mutate(date = as.POSIXct(DATE, format = "%m-%d-%Y %H:%M:%S", tz = "America/Chicago"))
}
downstream <- read_xlsx("Data/WRM_data/Downstream.xlsx") %>%
  filter(CONDUCTANCE > 26.8) 

downstream <- downstream[-c(132490:132541, 132586:132971, 136429), ] %>%
  frmt()

upstream <- read_xlsx("Data/WRM_data/Upstream.xlsx") %>%
  filter(CONDUCTANCE > 2.8) %>%
  frmt()

##CHLORIDE DATA####
#function to read in data for chloride
readCL <- function(X) {
  lab <- read_xlsx("Data/chloride_lab.xlsx", sheet = X) %>%
    mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
    mutate(date = round_date(datetime_collected, "30 minutes"))
}

labYN <- readCL("YN")
labYI <- readCL("YI")
labYS <- readCL("YS")
labSW <- readCL("SW")
lab6MC <- readCL("6MC")
labDC <- readCL("DC")
labPBMS <- readCL("PBMS")
labPBSF <- readCL("PBSF")

#Spring Harbor Data retreived outside of function because this is not a standardized file
labSH <- read_xlsx("Data/SpringHarborChloride.xlsx") %>%
  mutate(date = as.POSIXct(datetime_collected, format = "%m-%d-%Y %h:%m:%s", tz = "America/Chicago")) %>%
  mutate(date = round_date(datetime_collected, "30 minutes"))


##DISCHARGE DATA ####
format <- function(d) {
  d %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)
}

d.YN <- readNWISuv("05427850", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()
d.YI <- readNWISuv("05428500", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()
d.6MC <- readNWISuv("05427910", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()
d.DC <- readNWISuv("05427930", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()
d.PBMS <- readNWISuv("05427948", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()
d.PBSF <- readNWISuv("054279465", "00060", "2019-01-01", "", tz = "America/Chicago") %>%
  format()





#Spring Harbor Data retrieved outside of function because we are retrieving specific conductivity and discharge at the same time ####
d.sc.SH <- readNWISuv("05427965", c("00060", "00095"), "2019-01-01", "", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847) %>%
  rename(date = dateTime)
