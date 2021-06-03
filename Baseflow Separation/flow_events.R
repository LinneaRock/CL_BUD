source("Baseflow Separation/baseflow_separation_fct.R")

#read in discharge data

YI_d<- read_rds("Data/discharge/YI_discharge.rds") %>%
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

YN_d <- read_rds("Data/discharge/YN_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

SMC_d <- read_rds("Data/discharge/SMC_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

DC_d<- read_rds("Data/discharge/DC_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

PBMS_d <- read_rds("Data/discharge/PBMS_discharge.rds")%>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) 

# PBSF_d <- read_rds("Data/discharge/PBSF_discharge.rds")%>% 
#   dplyr::filter(!is.na(discharge)) %>% # Filter NAs
#   dplyr::filter(discharge >= 0) 

SH_d <- d.sc.SH %>% 
  dplyr::filter(!is.na(discharge)) %>% # Filter NAs
  dplyr::filter(discharge >= 0) %>%
  rename(runningmeandis = discharge)

#run baseflow calculations, get baseflow index, & plot 
YN_d <- get_eckhardt_bf("05427850", YN_d)
calc_bfi(YN_d) #bfi = 48%
plot_bf_qt(YN_d, "YN")

YI_d <- get_eckhardt_bf("05428500", YI_d)
calc_bfi(YI_d) #bfi = 82%
plot_bf_qt(YI_d, "YI")

DC_d <- get_eckhardt_bf("05427930", DC_d)
calc_bfi(DC_d) #bfi = 76%
plot_bf_qt(DC_d, "DC")

SMC_d <- get_eckhardt_bf("05427910", SMC_d)
calc_bfi(SMC_d) #bfi = 83%
plot_bf_qt(SMC_d, "SMC")

PBMS_d <- get_eckhardt_bf("05427948", PBMS_d)
calc_bfi(PBMS_d) #bfi = 50%
plot_bf_qt(PBMS_d, "PBMS")

# PBSF_d <- get_eckhardt_bf("054279465", PBSF_d)
# calc_bfi(PBSF_d) #bfi = 17%
# plot_bf_qt(PBSF_d, "PBSF")

SH_d <- get_eckhardt_bf("05427965", SH_d)
calc_bfi(SH_d) #bfi = 55%
plot_bf_qt(SH_d, "SH")



ggplot(df.peaks) +
  geom_line(aes(date, runningmeandis)) +
  geom_point(aes(date, peak.flag))




#finding all non-baseflow and calling it event = 0.25, combine with conductivity data
bfqt_d <- PBMS_d %>%
  mutate(event = ifelse(runningmeandis > threshold_peak, "Y", "N")) %>%
  left_join(PBMS_cond_data) %>%
  dplyr::select(date, runningmeandis, variable, value, threshold_peak, threshold_low, event, runningmean) %>%
  mutate(thresholdmax = 0.01 * max(runningmeandis))


ggplot(bfqt_d) +
  geom_line(aes(date, runningmeandis)) +
  #geom_line(aes(date, event), color = "red") +
  geom_line(aes(date, threshold_low), color = "blue") +
  geom_line(aes(date, thresholdmax),color = "green")


#plot to check
ggplot(bfqt_d) +
  geom_line(aes(date, runningmeandis)) +
  geom_line(aes(date, value), color = "hot pink") +
  geom_line(aes(date, threshold_peak), color = "blue") +
  geom_line(aes(date, threshold_low), color = "blue") +
  geom_point(aes(date, event), color = "purple") #+
  #geom_line(aes(date, runningmean/100), color = "red")



#just event days (qt)
date_of_events <- bfqt_d %>%
  dplyr::select(date, event) %>%
  filter(event == 0.25) %>%
  mutate(date2 = as.Date(date)) %>%
  select(date2, event) %>%
  distinct()
#Noting which dates of data to keep (i.e., before and after a rain event and flow exceeds threshold)
precip2 <- precip %>%
  left_join(date_of_events, by = c("date" = "date2")) %>%
  #mutate(keep2 = ifelse(event == 0.25, 1, 0)) %>%
  mutate(keep2 = ifelse(lag(event, n = 1L) == 0.25 | lag(event, n = 2L) == 0.25 | lead(event, n = 2L) == 0.25 | lead(event, n = 1L) == 0.25 | event == 0.25, 1, 0)) %>%
  filter(keep2 == 1)

#conmbine discharge/cond with precip. Keep only day prior to rain event, during rain event, and day after rain event
events <- bfqt_d %>%
  mutate(date) %>%
  mutate(date2 = as.Date(date)) %>%
  left_join(precip2, by = c("date2" = "date")) %>%
  filter(keep2 == 1) %>%
  filter(event.x == 0.25) %>%
  dplyr::select(date, runningmeandis, variable, value, threshold, runningmean, PRCP, date2, event.x)


#plot to check
ggplot(events) +
  geom_point(aes(date, runningmeandis)) +
  geom_line(aes(date, value), color = "hot pink")  +
  geom_line(aes(date, threshold), color = "blue")


#create groupings based on dates of events
dates_only <- events %>%
  select(date2) %>%
  unique()
dates <- as.vector(as.character(dates_only$date2))
dates1 <- sort(as.Date((dates)))
split(dates1, cumsum(c(TRUE, diff(dates1) != 1)))
dates2 <- data.frame(dates1, group = cumsum(c(TRUE, diff(dates1) != 1))) 

#putting the date groups into the dataset
events_grouped <- events %>%
  left_join(dates2, by = c("date2" = "dates1")) %>%
  rename(date_event = date2) %>%
  rename(event_flow = runningmeandis) %>%
  rename(event_cond = runningmean)

ggplot(events_grouped) +
  geom_point(aes(date, event_flow)) +
  geom_point(aes(date, value),color = "hot pink") +
  #geom_point(aes(date, event.y), color = "blue") +
  facet_wrap("group", scales = "free")

ggplot(events_grouped) +
  geom_point(aes(date, event_cond)) +
  facet_wrap("group", scales = "free")


join_all <- bfqt_d %>%
  mutate(all_date = as.Date(date)) %>%
  left_join(events_grouped) %>%
  mutate(bt = ifelse(is.na(group), runningmeandis, NA)) %>%
  mutate(sp.cond = ifelse(is.na(group), runningmean, NA)) %>%
  dplyr::select(date, bt, value, threshold, sp.cond, event_flow, event_cond, PRCP, group) %>%
  rename(Eckhardt = value)
  

ggplot(join_all) +
  geom_line(aes(date, bt)) +
  geom_line(aes(date, event_flow), color = "red") +
  geom_line(aes(date, threshold), color = "blue") +
  geom_ribbon(mapping = aes(x = date, ymin = 0, ymax = Eckhardt),fill = "hot pink")
  
























#old code, bad code.

#Get the baseflow information using the package of EcoHydRology. Then restart R before continuing.
# library(EcoHydRology)
# 
# YI_baseflow <- BaseflowSeparation(YI_discharge$runningmeandis, passes = 3)
# hydrograph(input = YI_discharge, streamflow2 = YI_baseflow[,2], S.units =  "m3s")
# test <- left_join(YI_discharge %>% mutate(number = row_number()), YI_baseflow %>% mutate(number = row_number()), by = "number") %>%
#   mutate(testing = bt + qft)
# YN_baseflow <- BaseflowSeparation(YN_discharge$runningmeandis, filter_parameter = 0.925, passes = 3)
# hydrograph(input = YN_discharge, streamflow2 = YN_baseflow[,2], S.units =  "m3s")
# SMC_baseflow <- BaseflowSeparation(SMC_discharge$runningmeandis, filter_parameter = 0.925, passes = 3)
# hydrograph(input = SMC_discharge, streamflow2 = SMC_baseflow[,2], S.units =  "m3s")
# DC_baseflow <- BaseflowSeparation(DC_discharge$runningmeandis, filter_parameter = 0.925, passes = 3) 
# hydrograph(input = DC_discharge, streamflow2 = DC_baseflow[,2], S.units =  "m3s")
# PBMS_baseflow <- BaseflowSeparation(PBMS_discharge$runningmeandis, filter_parameter = 0.925, passes = 3)
# hydrograph(input = PBMS_discharge, streamflow2 = PBMS_baseflow[,2], S.units =  "m3s")
# PBSF_baseflow <- BaseflowSeparation(PBSF_discharge$runningmeandis, filter_parameter = 0.925, passes = 3)
# hydrograph(input = PBSF_discharge, streamflow2 = PBSF_baseflow[,2], S.units =  "m3s")

source("Functions/qC_events.R")

# library(DVstats)
# YI_tst <- YI_discharge %>% 
#   mutate(base= baseflow(YI_discharge$runningmeandis))
# test <- DVstats::hysep(YI_discharge$runningmeandis, Dates = date, da = 1456)
# 
# ggplot(YI_tst) +
#   geom_line(aes(date, base), color = "red") +
#   geom_line(aes(date, runningmeandis), color = "black")

#YI's hydrograph does not show reaction to precip events ????????
YI_event <- find_all_events(YI_discharge, YI_baseflow, "YI", YI_cond_data)
#check the plots in the folder
x <- c()
y <- LETTERS[]

YN_event <- find_all_events(YN_discharge, YN_baseflow, "YN", YN_cond_data)
#check the plots in the folder
x <- c(1, 2, 4, 8, 9, 13, 14)
y <- LETTERS[1:7]
QC_plots(x, y, YN_event, "YN", "Yahara North", YN_discharge, YN_cond_data)

SMC_event <- find_all_events(SMC_discharge, SMC_baseflow, "SMC", SMC_cond_data)
#check the plots in the folder
x <- c(2:5, 7, 9:11, 13, 14, 16)
y <- LETTERS[1:11]
QC_plots(x, y, SMC_event, "SMC", "Sixmile Creek", SMC_discharge, SMC_cond_data)

DC_event <- find_all_events(DC_discharge, DC_baseflow, "DC", DC_cond_data)
#check the plots in the folder
x <- c(1, 4:6, 8, 10, 11, 13, 15, 16, 18, 20, 22, 23)
y <- LETTERS[1:14]
QC_plots(x, y, DC_event, "DC", "Dorn Creek", DC_discharge, DC_cond_data)

PBMS_event <- find_all_events(PBMS_discharge, PBMS_baseflow, "PBMS", PBMS_cond_data)
#check the plots in the folder
x <- c(1, 3:5, 7, 9:11, 13:16, 18, 19, 21, 23, 25, 26)
y <- LETTERS[1:18]
QC_plots(x, y, PBMS_event, "PBMS", "Pheasant Branch Main Stem", PBMS_discharge, PBMS_cond_data)

PBSF_event <- find_all_events(PBSF_discharge, PBSF_baseflow, "PBSF", PBSF_cond_data)
#check the plots in the folder
x <- c(1, 2, 4, 8, 11:13, 15, 18, 21)
y <- LETTERS[1:10]
QC_plots(x, y, PBSF_event, "PBSF", "Pheasant Branch South Fork", PBSF_discharge, PBSF_cond_data)


  

