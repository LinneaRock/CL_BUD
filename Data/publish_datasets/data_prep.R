library(tidyverse)
library(zoo)

#Function to convert raw, absolute conductivity to specific conductivit @ 25 degrees C
convert_to_sp.cond <- function(ActualCond, temp) {
  ActualCond / (1 - (25 - temp) * 0.021)
}


#function to detect outliers
find_outliers <- function(ConductivityData) {
  #add column for 6 hour running mean and get the total average specific conductivity of the dataset
  cond_data1 <- ConductivityData %>%
    mutate(runningmean = rollmean(SpCond_uScm, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(SpCond_uScm[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(cond_data) - 5), mean(SpCond_uScm[(nrow(cond_data) - 5):nrow(cond_data)]), runningmean))
  
  #outliers identified as datapoints greater than 1.05x the running mean or less than 0.95x the running mean
  cond_data2 <- cond_data1 %>%
    mutate(outlier = ifelse(SpCond_uScm > (1.05 * runningmean), "Y", "N")) %>% #detect outliers as 1.05x greater than running average
    mutate(outlier = ifelse(SpCond_uScm < (0.95 * runningmean), "Y", outlier)) #or as 0.95x less than running average
  
   #write new dataset that includes all original data and the data with outliers removed and the running mean
  cond_data3 <- cond_data2 %>%
    dplyr::select(
      dateTime, #original data: date the conductivivty was measured
      EC_lowRange_uScm, #original data: low range conductivity
      EC_highRange_uScm, #original data: full range conductivity
      Temp_C, #original data: water temperature
      SpCond_uScm, #original data (first modified): specific conductivity calculated
      ID, #original data: lake ID
      outlier #Y or N, identifies outliers
    ) 
  
  return(cond_data3)
}
