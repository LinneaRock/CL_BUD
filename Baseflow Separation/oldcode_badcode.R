
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




