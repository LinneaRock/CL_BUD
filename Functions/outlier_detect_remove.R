library(zoo)

#function to detect outliers

outlier_detect_remove <- function(cond_data, plotname) {
  #add column for 6 hour running mean and get the total average specific conductivity of the dataset
  cond_data1 <- cond_data %>%
    mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(cond_data) - 5), mean(sp.cond[(nrow(cond_data) - 5):nrow(cond_data)]), runningmean))
  
  
  
  cond_data2 <- cond_data1 %>%
    mutate(outlier = ifelse(sp.cond > (1.05 * runningmean), "Y", "N")) %>% #detect outliers as 1.05x greater than running average
    mutate(outlier = ifelse(sp.cond < (0.95 * runningmean), "Y", outlier)) #or as 0.95x less than running average
  
  
  ggplot(cond_data2) +
    geom_point(aes(date, sp.cond, color = outlier)) +
    geom_line(aes(date, runningmean), color = "red")
  
  ggsave(
    paste(
      "Plots/conductance_time_series/outlier_check/",
      plotname,
      ".png",
      sep = ""
    ),
    width = 20,
    height = 15,
    units = "cm"
  )
  
  cond_data3 <- cond_data2 %>%
    mutate(sp.cond2 = ifelse(outlier == "Y", NA, sp.cond)) %>%
    mutate(runningmean = rollmean(sp.cond2, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond2[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(cond_data2) - 5), mean(sp.cond2[(nrow(cond_data2) - 5):nrow(cond_data2)]), runningmean))
  
  cond_data4 <- cond_data3 %>%
    dplyr::select(
      date,
      Low.Range,
      Full.Range,
      Temp,
      sp.cond,
      ID,
      sp.cond,
      outlier,
      sp.cond2,
      runningmean
    ) %>%
    rename(orig.sp.cond = sp.cond,
           sp.cond = sp.cond2)
}
