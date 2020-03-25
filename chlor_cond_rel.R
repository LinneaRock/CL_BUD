library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(lubridate)
library(ggpubr)

############################YS############################

#Loading in data from conductivity loggers:
loggerYSY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YS/Dec19_Feb3/20758348_YS.csv")

loggerYSZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YS/Feb3_Mar16/20758348_YS.csv")

#Combining and formatting logger data:
loggerYS <- loggerYSY %>%
  rbind(loggerYSZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labYS <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "YS")

#Formatting time:
labYS$datetime_collected <- as.POSIXct(labYS$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time: 
labYS$datetime_collected <- round_date(labYS$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerYS %>%
  select(date, Full.Range) 

d.2 <- labYS %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Yahara River - South") +
  stat_cor(label.x = 64.75, label.y = 200) +
  stat_regline_equation(label.x = 64.75, label.y = 250) #how do I get r^2 value? 


###########################SW###########################

#Loading in data from conductivity loggers:
loggerSWY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/SW/Dec19_Feb3/20378151_SW.csv")

loggerSWZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/SW/Feb3_Mar16/20378151_SW.csv")

#Combining and formatting logger data:
loggerSW <- loggerSWY %>%
  rbind(loggerSWZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labSW <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "SW")

#Formatting time:
labSW$datetime_collected <- as.POSIXct(labSW$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labSW$datetime_collected <- round_date(labSW$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerSW %>%
  select(date, Full.Range) 

d.2 <- labSW %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Starkweather Creek") +
  stat_cor(label.x = 50, label.y = 3500) +
  stat_regline_equation(label.x = 50, label.y = 4000) #how do I get r^2 value? 



################################YI#############################

#Loading in data from conductivity loggers:
loggerYIY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YI/Dec19_Feb4/20758347_YI.csv")

loggerYIZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YI/Feb4_Mar16/20758347_YI.csv")

#Combining and formatting logger data:
loggerYI <- loggerYIY %>%
  rbind(loggerYIZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labYI <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "YI")

#Formatting time:
labYI$datetime_collected <- as.POSIXct(labYI$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labYI$datetime_collected <- round_date(labYI$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerYI %>%
  select(date, Full.Range) 

d.2 <- labYI %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Yahara River - Isthmus") +
  stat_cor(label.x = 52, label.y = 50) +
  stat_regline_equation(label.x = 52, label.y = 100)#how do I get r^2 value? 



##########################YN############################

#Loading in data from conductivity loggers:
loggerYNY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YN/Dec19_Feb4/20758343_YN.csv")

loggerYNZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/YN/Feb4_Mar16/20758343_YN.csv")

#Combining and formatting logger data:
loggerYN <- loggerYNY %>%
  rbind(loggerYNZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labYN <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "YN")

#Formatting time:
labYN$datetime_collected <- as.POSIXct(labYN$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labYN$datetime_collected <- round_date(labYN$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerYN %>%
  select(date, Full.Range) 

d.2 <- labYN %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Yahara River - North") +
  stat_cor(label.x = 35, label.y = 450) +
  stat_regline_equation(label.x = 35, label.y = 475)#how do I get r^2 value? 



################################6MC##################################

#Loading in data from conductivity loggers:
logger6MCY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/6MC/Dec19_Feb4/20758342_6MC.csv")

logger6MCZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/6MC/Feb4_Mar16/20758342_6MC.csv")

#Combining and formatting logger data:
logger6MC <- logger6MCY %>%
  rbind(logger6MCZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
lab6MC <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "6MC")

#Formatting time:
lab6MC$datetime_collected <- as.POSIXct(lab6MC$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
lab6MC$datetime_collected <- round_date(lab6MC$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- logger6MC %>%
  select(date, Full.Range) 

d.2 <- lab6MC %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Sixmile Creek") +
  stat_cor(label.x = 37.5, label.y = 600) +
  stat_regline_equation(label.x = 37.5, label.y = 650)#how do I get r^2 value?


#############################DC###############################

#Loading in data from conductivity loggers:
loggerDCY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/DC/Dec19_Feb4/20758338_DC.csv")

loggerDCZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/DC/Feb4_Mar16/20758338_DC.csv")

#Combining and formatting logger data:
loggerDC <- loggerDCY %>%
  rbind(loggerDCZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labDC <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "DC")

#Formatting time:
labDC$datetime_collected <- as.POSIXct(labDC$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labDC$datetime_collected <- round_date(labDC$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerDC %>%
  select(date, Full.Range) 

d.2 <- labDC %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Dorn Creek") +
  stat_cor(label.x = 36, label.y = 350) +
  stat_regline_equation(label.x = 36, label.y = 400)#how do I get r^2 value?


############################PBMS###########################

#Loading in data from conductivity loggers:
loggerPBMSY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/PBMS/Dec19_Feb3/20758344_PBMS.csv")

loggerPBMSZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/PBMS/Feb3_Mar16/20758344_PBMS.csv")

#Combining and formatting logger data:
loggerPBMS <- loggerPBMSY %>%
  rbind(loggerPBMSZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(char = as.character(Date),
         date = as.POSIXct(char, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)

#Loading in chloride data:
labPBMS <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "PBMS")

#Formatting time:
labPBMS$datetime_collected <- as.POSIXct(labPBMS$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labPBMS$datetime_collected <- round_date(labPBMS$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerPBMS %>%
  select(date, Full.Range) 

d.2 <- labPBMS %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Pheasant Branch Main Stem") +
  stat_cor(label.x = 117.5, label.y = 750) +
  stat_regline_equation(label.x = 117.5, label.y = 825)#how do I get r^2 value?


########################PBSF#########################

#Loading in data from conductivity loggers:
loggerPBSFY <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/PBSF/Jan2_Jan15/20758339_PBSF.csv")

loggerPBSFZ <- read.csv("C:/Users/linne/Box Sync/Chloride Research/Data/HOBO_Loggers/PBSF/Jan21_Feb4/20758339_PBSF.csv")

#Combining and formatting logger data:
loggerPBSF <- loggerPBSFY %>%
  rbind(loggerPBSFZ) %>%
  drop_na() %>%
  select(Date, Low.Range, Full.Range, Temp)%>%
  mutate(#char = as.character(Date),
    date = as.POSIXct(Date, format = "%m-%d-%Y %H:%M:%S")) %>%
  select(date, Low.Range, Full.Range, Temp)
loggerPBSF$date <- round_date(loggerPBSF$date, "30 minutes") #Needed to round time because the logger was collecting at H:15 and H:45 for a few weeks rather than at H:00 and H:30.

#Loading in chloride data:
labPBSF <- read_xlsx("C:/Users/linne/Box Sync/Chloride Research/Data/chloride_lab.xlsx", sheet = "PBSF")

#Formatting time:
labPBSF$datetime_collected <- as.POSIXct(labPBSF$datetime_collected, format = "%m-%d-%Y %h:%m:%s")

#Rounding time to match logger collection time:
labPBSF$datetime_collected <- round_date(labPBSF$datetime_collected, "30 minutes")



#combining chloride and conductivity into single dataframe
d.1 <- loggerPBSF %>%
  select(date, Full.Range) 

d.2 <- labPBSF %>%
  select(datetime_collected, chloride_mgL) %>%
  rename(date = datetime_collected) 

d.3 <- left_join(d.1, d.2, by = "date")

#plotting
ggplot(data = d.3, aes(chloride_mgL, Full.Range)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) +
  labs(y = "Conductivity (uS/cm)", x = "Chloride (mg/L)", title = "Pheasant Branch South Fork") +
  stat_cor(label.x = 250, label.y = 2500) +
  stat_regline_equation(label.x = 250, label.y = 3000)#how do I get r^2 value?
