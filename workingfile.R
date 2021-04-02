
 # loggerPBSF1 <- loggerPBSF %>%
 #   filter(sp.cond > 200) #deletes a couple measurements that were collected while the logger was outside of the water


  
  loggerPBSF2 <- loggerPBSF1 %>%
    mutate(runningmean = rollmean(sp.cond, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(loggerPBSF1) - 5), mean(sp.cond[(nrow(loggerPBSF1) - 5):nrow(loggerPBSF1)]), runningmean)) #%>%
    #mutate(allmean = mean(sp.cond, na.rm = TRUE))
  
  loggerPBSF3 <- loggerPBSF2 %>%
    #rowwise() %>%
    mutate(outlier = ifelse(
                              sp.cond > (1.05 * runningmean), "Y", "N"))
  
  loggerPBSF4 <- loggerPBSF3 %>%
    #rowwise() %>%
    mutate(outlier = ifelse(
                              sp.cond < (0.95 * runningmean), "Y", outlier))
    
  
  ggplot(loggerPBSF1) + geom_point(aes(date, sp.cond)) +
    geom_line(loggerPBSF2, mapping = aes(date, runningmean), color = "red") +
    #geom_line(loggerPBSF2, mapping = aes(date, allmean), color = "blue") +
    geom_point(loggerPBSF3, mapping = aes(date, sp.cond, color = outlier)) +
    geom_point(loggerPBSF4, mapping = aes(date, sp.cond, color = outlier))

  loggerPBSF5 <- loggerPBSF4 %>%
    mutate(sp.cond2 = ifelse(outlier == "Y", NA, sp.cond)) 
    
  loggerPBSF6 <- loggerPBSF5 %>%
    mutate(runningmean2 = rollmean(sp.cond2, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean2 = ifelse(row_number() <= 6, mean(sp.cond2[1:6]), runningmean2)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean2 = ifelse(row_number() >= (nrow(loggerPBSF5) - 5), mean(sp.cond2[(nrow(loggerPBSF5) - 5):nrow(loggerPBSF5)]), runningmean2))

  loggerPBSF7 <- loggerPBSF6 %>%
    mutate(did = ifelse(runningmean == runningmean2, "Y", "N"))
  
  ggplot(loggerPBSF7) + geom_point(aes(date, sp.cond2)) +
    geom_line(mapping = aes(date, runningmean2), color = "red") 

  loggerPBSF_try <- loggerPBSF7 %>%
    select(date, Low.Range, Full.Range, Temp, sp.cond, ID, sp.cond, outlier, sp.cond2, runningmean2) %>%
    rename(orig.sp.cond = sp.cond,
           sp.cond = sp.cond2,
           runningmean = runningmean2)

  
  
  
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
    
  ggsave(paste("Plots/conductance_time_series/outlier_check/", plotname, ".png", sep = ""), width = 20, height = 15, units = "cm")
  
  cond_data3 <- cond_data2 %>%
    mutate(sp.cond2 = ifelse(outlier == "Y", NA, sp.cond)) %>%
    mutate(runningmean = rollmean(sp.cond2, 13, fill = NA, na.rm = TRUE)) %>% #use zoo::rollmean over 13 rows (6 hours - 3 before and 3 after each point)
    mutate(runningmean = ifelse(row_number() <= 6, mean(sp.cond2[1:6]), runningmean)) %>% # rollmean leaves empty rows at beginning and end of dataset. This line and the one below uses the mean of those empty rows
    mutate(runningmean = ifelse(row_number() >= (nrow(cond_data2) - 5), mean(sp.cond2[(nrow(cond_data2) - 5):nrow(cond_data2)]), runningmean))
  
  cond_data4 <- cond_data3 %>%
    select(date, Low.Range, Full.Range, Temp, sp.cond, ID, sp.cond, outlier, sp.cond2, runningmean) %>%
    rename(orig.sp.cond = sp.cond,
           sp.cond = sp.cond2)
 
  }
  
  
pbsf_test <- outlier_detect_remove(loggerPBSF1, "PBSF")
YI_test <- outlier_detect_remove(loggerYI1, "YI")  
DC_test <- outlier_detect_remove(loggerDC1, "DC")
SMC_test <- outlier_detect_remove(logger6MC, "SMC")
PBMS_test <- outlier_detect_remove(loggerPBMS, "PBMS")
YN_test <- outlier_detect_remove(loggerYN1, "YN")  
YS_test <- outlier_detect_remove(loggerYS1, "YS")  
WIC_test <- outlier_detect_remove(loggerWIC1, "WIC")
SW_test <- outlier_detect_remove(loggerSW1, "SW")

cond(SW_test)
  
  linreg(labYS, YS_test)
  info(labYS, YS_test)
  

  
join <- join_datasets_chloride(labYS, fieldcondYS)

fit <- lm(chloride_mgL~sp.cond, join)
summary(fit) 

info(labPBSF, PBSF_cond_data)

ggplot(join) + geom_point(aes(sp.cond, chloride_mgL))  
