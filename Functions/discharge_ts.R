
#6-hour moving average for discharge data
rolling_ave_discharge <- function(discharge_data) {
  
  dis_data2 <- discharge_data %>%
    mutate(runningmean = rollmean(discharge, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(discharge[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(discharge_data) - 5), mean(discharge[(nrow(discharge_data) - 5):nrow(discharge_data)]), runningmean))
  
}

#plot timeseries discharge data
discharge_ts <- function(dischargedata) {
  ggplot(dischargedata) +
    geom_line(aes(date, discharge)) +
    L_theme() +
    labs(x = "",
         y = "Discharge (cms)")
}





