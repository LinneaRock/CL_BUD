
#6-hour moving average for discharge data
rolling_ave_discharge <- function(logger_data, discharge_data) {
  
  dis_data1 <- join_datasets_cond(logger_data, discharge_data)
  
  dis_data2 <- dis_data1 %>%
    select(date, discharge) %>%
    mutate(runningmeandis = rollmean(discharge, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmeandis = ifelse(row_number() <= 6, mean(discharge[1:6]), runningmeandis)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmeandis = ifelse(row_number() >= (nrow(dis_data1) - 5), mean(discharge[(nrow(dis_data1) - 5):nrow(dis_data1)]), runningmeandis)) %>%
    mutate(res = discharge - runningmeandis) %>%
    mutate(runningmeandis = ifelse(runningmeandis <= 0, 0, runningmeandis))
  
}

#plot timeseries discharge data
discharge_ts <- function(dischargedata) {
  ggplot(dischargedata) +
    geom_line(aes(date, discharge), color = "snow3") +
    geom_line(aes(date, runningmeandis)) +
    L_theme() +
    labs(x = "",
         y = "Discharge"~(m^3~s^-1))
}





