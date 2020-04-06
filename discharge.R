library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(dataRetrieval)


#retrieving data for USGS gage sites
##rename columns to make dfs easier to understand
###convert discharge from cfs to cms
####slect date and discharge, sp. conductance columns

d.YI <- readNWISuv("05428500", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)
  


d.YN <- readNWISuv("05427850", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)


d.6MC <- readNWISuv("05427910", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)


d.DC <- readNWISuv("05427930", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)


d.PBMS <- readNWISuv("05427948", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)


d.PBSF <- readNWISuv("054279465", "00060", "2019-12-19", "2020-03-16", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000) %>%
  select(dateTime, discharge) %>%
  mutate(discharge = discharge * 0.028316847)


d.sc.SH <- readNWISuv("05427965", c("00060", "00095"), "2019-12-19", "2020-04-06", tz = "America/Chicago") %>%
  rename(discharge = X_00060_00000,
         sp_cond = X_00095_00000) %>%
  select(dateTime, discharge, sp_cond) %>%
  mutate(discharge = discharge * 0.028316847)



