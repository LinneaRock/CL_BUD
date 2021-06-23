
ME_chloride <- labME %>%
  dplyr::select(date, Depth_m, chloride_mgL) %>%
  rename(depth_m = Depth_m)

write_csv(ME_chloride, "Data/publish_datasets/ME_cl.csv")

MO_chloride <- labMO %>%
  dplyr::select(date, Depth_m, chloride_mgL) %>%
  rename(depth_m = Depth_m)

write_csv(MO_chloride, "Data/publish_datasets/MO_cl.csv")

Mendota_all <- (loggerME_Epi1 %>% mutate(depth_m = 2)) %>%
  bind_rows(loggerME_Hypo1 %>% mutate(depth_m = 23.5)) 
  
write_csv(Mendota_all, "Data/publish_datasets/ME_cond.csv")

ggplot(Mendota_all) +
  geom_line(aes(date, sp.cond, group = depth_m))


Monona_all <- (loggerMO_Epi20_1 %>% mutate(depth_m = 2)) %>%
  bind_rows(loggerMO_Hypo20_1 %>% mutate(depth_m = 20)) %>%
  bind_rows(loggerMO_Epi21_1 %>% mutate(depth_m = 2)) %>%
  bind_rows(loggerMO_Hypo21_1 %>% mutate(depth_m = 20)) 

ggplot(Monona_all) +
  geom_line(aes(date, sp.cond, group = depth_m))

write_csv(Monona_all, "Data/publish_datasets/MO_cond.csv")


prep_trib_cond <- function(cond_data, name) {
  cond_data1 <- cond_data%>%
    dplyr::select(date,
                  Low.Range,
                  Full.Range,
                  Temp,
                  orig.sp.cond,
                  ID
    ) %>%
    rename(dateTime = date,
           EC_lowRange_uScm = Low.Range,
           EC_highRange_uScm = Full.Range,
           Temp_C = Temp,
           SpCond_uScm = orig.sp.cond
    )
  
  write_csv(cond_data1, paste("Data/publish_datasets/", name, "_cond.csv", sep = ""))
}


prep_trib_cond(DC_cond_data, "DC")
prep_trib_cond(YN_cond_data, "YN")
prep_trib_cond(SMC_cond_data, "SMC")
prep_trib_cond(PBMS_cond_data, "PBMS")
prep_trib_cond(PBSF_cond_data, "PBSf")
prep_trib_cond(YI_cond_data, "YI")
prep_trib_cond(SW_cond_data, "SW")
prep_trib_cond(YS_cond_data, "YS")
prep_trib_cond(WIC_cond_data, "WIC")

library(lubridate)

join <- function(chloride_data, field_cond, name) {
  
  cl_edit <- chloride_data %>%
    dplyr::select(date, chloride_mgL, mon, season, ID) %>%
    mutate(date2 = round_date(date, unit = "30 minutes"))
  cond_edit <- field_cond %>%
    dplyr::select(date, conductivity_uscm, temp_C, sp.cond,  ID)  %>%
    mutate(date2 = round_date(date, unit = "30 minutes"))
  
  join <- left_join(cl_edit, cond_edit, by = "date2") %>%
    dplyr::select(date.x, chloride_mgL, conductivity_uscm, temp_C, sp.cond, ID.y) %>%
    rename(dateTime = date.x,
           EC_uScm = conductivity_uscm,
           Temp_C = temp_C,
           SpCond_uScm = sp.cond,
           ID = ID.y) %>%
    mutate(ID = name)
  
  write_csv(join, paste("Data/publish_datasets/", name, "_cl.csv", sep = ""))
  
}


join(labDC, fieldcondDC, "DC")
join(lab6MC, fieldcond6MC, "SMC")
join(labYN, fieldcondYN, "YN")
join(labPBMS, fieldcondPBMS, "PBMS")
join(labPBSF, fieldcondPBSF, "PBSF")
join(labYI, fieldcondYI, "YI")
join(labSW, fieldcondSW, "SW")
join(labWIC, fieldcondWIC, "WIC")
join(labYS, fieldcondYS, "YS")


#geographic location of tribs
trib_locations = data.frame(site = c('YN', '6MC', 'DC', 'PBMS', 'PBSF', 'YI','SW', 'YS', 'WIC'),lat = c(43.15083333, 43.14683333, 43.14027778, 43.10333333, 43.09861111, 43.08944444, 43.09259, 43.04718, 43.05416667), lon = c(-89.40194444, -89.43694444, -89.44222222, -89.51166667, -89.52138889, -89.36083333, -89.33318, -89.33605, -89.379444))
write_csv(trib_locations, "Data/publish_datasets/trib_locations.csv")





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

