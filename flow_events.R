
precip <- read.csv("Data/Historical_External/precip_temp.csv") %>%
  mutate(PRCP = PRCP * 25.4) %>%  #inches to mm
  mutate(date = as.POSIXct(as.character(DATE)))

DC_discharge <- read_rds("Data/discharge/DC_discharge.rds")
DC_cond_data <- read_rds("Data/HOBO_Loggers/DC/DC_cond_data.rds")


PBSF_discharge <- read_rds("Data/discharge/PBSF_discharge.rds")
PBSF_cond_data <- read_rds("Data/HOBO_Loggers/PBSf/PBSF_cond_data.rds")

#Get the baseflow information using the package of EcoHydRology. Then restart R before continuing.
#library(EcoHydRology)

DC_baseflow <- BaseflowSeparation(DC_discharge$runningmeandis, filter_parameter = 0.925, passes = 3) 
PBSF_baseflow <- BaseflowSeparation(PBSF_discharge$runningmeandis, filter_parameter = 0.925, passes = 3)


source("Functions/qC_events.R")

DC_event <- find_all_events(DC_discharge, DC_baseflow, "DC", DC_cond_data)
#check the plots in the folder
x <- c(1, 4:6, 8, 10, 11, 13, 15, 16, 18, 20, 22, 23)
y <- LETTERS[1:14]

QC_plots(x, y, DC_event, "DC", "Dorn Creek")


PBSF_event <- find_all_events(PBSF_discharge, PBSF_baseflow, "PBSF", PBSF_cond_data)
x <- c(1, 2, 4, 6, 8, 11, 12, 13, 15, 18, 21)
y <- LETTERS[1:11]

QC_plots(x, y, PBSF_event, "PBSF", "Pheasant Branch South Fork")
