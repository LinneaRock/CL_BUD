
library(anomalize)
library(tidyverse)
library(zoo)

#Visualize outliers and decide which flagged ones need to be corrected.
#use data after it has imputed values added already
flagged_data <- function(loggerdata) { #step to flag the outlier data
  data <- loggerdata %>%
    as_tibble() %>%
    time_decompose(sp.cond) %>%
    anomalize(remainder) %>%
    mutate(Year_Month = paste(year(date), month(date), sep = "-"))
}

plot_flagged <- function(flaggedloggerdata) { #plot the data with flags by month so it is easy to determine how to proceed
  flaggedloggerdata %>% plot_anomalies() + facet_wrap("Year_Month", scales = "free") 
}

#Next subset the data so that actual outliers are removed and clean_anomalies()
#Next visualize cleaned data to ensure correct
plot_cleaned <- function(cleaned_data) { #plot cleaned data by month
  ggplot(cleaned_data) +
    geom_point(aes(date, observed_cleaned)) +
    facet_wrap("Year_Month", scales = "free") 
}

#finally, combine important information into a working dataset and get 6 hour moving average.
final_cond_data <- function(loggerdata, cleaned_data, flaggedloggerdata) {
  cond_data <- loggerdata %>%
    left_join(cleaned_data, by = "date") %>%
    left_join(flaggedloggerdata %>% select(date, trend), by = "date") %>%
    mutate(sp.cond = ifelse(is.na(observed_cleaned), sp.cond, observed_cleaned)) %>%
    select(date, sp.cond, trend.y, Low.Range, Full.Range, Temp, observed_cleaned) %>%
    rename(trend = trend.y) %>%
    mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(loggerdata) - 5), mean(sp.cond[(nrow(loggerdata) - 5):nrow(loggerdata)]), runningmean)) 
    
  }










  
 

