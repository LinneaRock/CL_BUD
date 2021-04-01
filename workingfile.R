
 loggerYI1 <- loggerYI %>%
   filter(sp.cond > 200) #deletes a couple measurements that were collected while the logger was outside of the water
# 
# 
# # #flag outliers using anomalize package
  YI_outlier <- flagged_data(loggerYI1)
# # #plot to inspect where to correct outliers
  plot_flagged(YI_outlier)
# # #after inspecting, filter and clean anomalies
  YI_cleaned <- YI_outlier %>%
    filter(Year_Month != "2020-1" &
             Year_Month != "2020-2" &
             Year_Month != "2021-3" &
             Year_Month != "2021-2") %>%
    clean_anomalies()
# # #insepect cleaned points
  plot_cleaned(YI_cleaned)
# # #final dataset with runningmean, trend, and corrected specific conductance data
  YI_cond_data <- final_cond_data(loggerYI1, YI_cleaned, YI_outlier)
#  write_rds(YI_cond_data, "Data/HOBO_Loggers/YI/YI_cond_data.rds")
  
  

  
  loggerYI2 <- loggerYI1 %>%
    mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(loggerYI1) - 5), mean(sp.cond[(nrow(loggerYI1) - 5):nrow(loggerYI1)]), runningmean)) %>%
    mutate(allmean = mean(sp.cond))
  
  loggerYI3 <- loggerYI2 %>%
    #rowwise() %>%
    mutate(outlier = ifelse(sp.cond > allmean &
                              sp.cond > (1.05 * runningmean), "Y", "N"))
  
  loggerYI4 <- loggerYI3 %>%
    #rowwise() %>%
    mutate(outlier = ifelse(sp.cond < allmean &
                              sp.cond < (0.95 * runningmean), "Y", outlier))
    
  
  ggplot(loggerYI1) + geom_point(aes(date, sp.cond)) +
    geom_line(loggerYI2, mapping = aes(date, runningmean), color = "red") +
    geom_line(loggerYI2, mapping = aes(date, allmean), color = "blue") +
    geom_point(loggerYI3, mapping = aes(date, sp.cond, color = outlier)) +
    geom_point(loggerYI4, mapping = aes(date, sp.cond, color = outlier))

  loggerYI5 <- loggerYI4 %>%
    mutate(sp.cond2 = ifelse(outlier == "Y", NA, sp.cond)) 
    
  loggerYI6 <- loggerYI5 %>%
    mutate(runningmean2 = rollmean(sp.cond2, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean2 = ifelse(row_number() <= 6, mean(sp.cond2[1:6]), runningmean2)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean2 = ifelse(row_number() >= (nrow(loggerYI5) - 5), mean(sp.cond2[(nrow(loggerYI5) - 5):nrow(loggerYI5)]), runningmean2))

  loggerYI7 <- loggerYI6 %>%
    mutate(did = ifelse(runningmean == runningmean2, "Y", "N"))
  
  ggplot(loggerYI7) + geom_point(aes(date, sp.cond2)) +
    geom_line(mapping = aes(date, runningmean2), color = "red") 

  loggerYI_try <- loggerYI7 %>%
    select(date, Low.Range, Full.Range, Temp, sp.cond, ID, sp.cond, outlier, sp.cond2, runningmean2) %>%
    rename(orig.sp.cond = sp.cond,
           sp.cond = sp.cond2,
           runningmean = runningmean2)

join <- join_datasets_chloride(labDC, fieldcondDC)

fit <- lm(chloride_mgL~sp.cond, join)
summary(fit) 0.32

info(labDC, DC_cond_data)

ggplot(join) + geom_point(aes(sp.cond, chloride_mgL))  
